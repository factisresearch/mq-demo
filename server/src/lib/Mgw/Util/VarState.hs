{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Mgw.Util.VarState
    ( HasVars(..), HasChans(..), HasMVars(..), HasQSems(..)
    , VarState, VarName, HasVarState(..)
    , viewChan, readMVar, swapMVar, tryReadMVar, modifyMVar, modifyReadMVar
    , runVarState, runVarStateT, emptyVarState
    , runStateVar'
) where

#include "src/macros.h"

#define LOG_NAMES

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Control.Monad.Error (ErrorT, Error)
import Control.Monad.State ( State, StateT, evalState, evalStateT, gets, modify)
import Control.Monad.Reader (ReaderT)
import Control.Monad.ST
import Control.Concurrent.STM (STM, atomically)
import Mgw.Util.STM (TVar, newTVar, readTVar, writeTVar)
import Mgw.Util.STM (TMVar, newEmptyTMVar, takeTMVar, putTMVar, tryTakeTMVar)
import Mgw.Util.STM (TChan, newTChan, readTChan, writeTChan, isEmptyTChan)
import Mgw.Util.STM (BTChan, newBTChan, readBTChan, writeBTChan, isEmptyBTChan)
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Monad.State.Strict as StrictState

import Data.Map (Map)
import Data.IORef
import Data.Sequence (Seq, (<|))
import Data.STRef

import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as T

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.ThreadActivity (bracketThreadActivity)
import qualified Mgw.Util.MVar as MVar

class (Monad m, Functor m, Applicative m) => HasVars m where
    type Var m :: * -> *
    newVar :: a -> m (Var m a)
    newNamedVar :: String -> a -> m (Var m a)
    newNamedVar _ = newVar
    readVar :: Var m a -> m a
    writeVar :: Var m a -> a -> m ()
    modifyVar :: Var m a -> (a -> a) -> m ()
    modifyVar var f =
        do val <- readVar var
           let val' = f val
           writeVar var val'
           return (val' `seq` ())
    -- in STM, if you can, use modifyVar instead of modifyVar' and pass the resulting
    -- value out of the transaction
    modifyVar' :: Var m a -> (a -> a) -> m ()
    modifyVar' v f =
        do x <- modifyVar v f
           x `seq` return ()
    modifyReadVar :: Var m a -> (a -> (a, b)) -> m b
    modifyReadVar var f =
        do val <- readVar var
           let (val', res) = f val
           writeVar var val'
           return res
    modifyReadVar' :: Var m a -> (a -> (a, b)) -> m b
    modifyReadVar' var f =
        do val <- readVar var
           let (!val', res) = f val
           writeVar var val'
           return res

class Monad m => HasChans m where
    type Chan m :: * -> *
    newChan :: m (Chan m a)
    newBoundedChan :: Int -> m (Chan m a)
    writeChan :: Chan m a -> a -> m ()
    readChan :: Chan m a -> m a
    isEmptyChan :: Chan m a -> m Bool

class Monad m => HasMVars m where
    type MVar m :: * -> *
    newMVar :: m (MVar m a)
    takeMVar :: MVar m a -> m a
    putMVar :: MVar m a -> a -> m ()
    tryTakeMVar :: MVar m a -> m (Maybe a)
    withMVar :: MVar m a -> (a -> m b) -> m b

class Monad m => HasQSems m where
    type QSem m
    newQSem :: Int -> m (QSem m)
    waitQSem :: QSem m -> m ()
    signalQSem :: QSem m -> m ()

viewChan :: HasChans m => Chan m a -> m (Seq a)
viewChan chan =
    do elems <- readLoop
       T.mapM_ (writeChan chan) elems
       return elems
    where
      readLoop =
          do empty <- isEmptyChan chan
             if empty
                then return Seq.empty
                else do x <- readChan chan
                        xs <- readLoop
                        return (x <| xs)

readMVar :: HasMVars m => MVar m a -> m a
readMVar v =
    do x <- takeMVar v
       putMVar v x
       return x

swapMVar :: HasMVars m => MVar m a -> a -> m a
swapMVar mv x = takeMVar mv >>= \old -> putMVar mv x >> return old

tryReadMVar :: HasMVars m => MVar m a -> m (Maybe a)
tryReadMVar v =
    do mx <- tryTakeMVar v
       case mx of
         Just x ->
             do putMVar v x
                return (Just x)
         Nothing -> return Nothing

modifyMVar :: HasMVars m => MVar m a -> (a -> a) -> m ()
modifyMVar v f =
    do x <- takeMVar v
       putMVar v (f x)

modifyReadMVar :: HasMVars m => MVar m a -> (a -> (a, b)) -> m b
modifyReadMVar v f =
    do curX <- takeMVar v
       let (newX, res) = f curX
       putMVar v newX
       return res

#ifdef LOG_NAMES
logAccess :: String -> STM ()
logAccess _msg = return ()

data NamedVar v a = NamedVar (Maybe String) (v a) deriving Eq

instance HasVars STM where
    type Var STM = NamedVar TVar
    newVar = liftM (NamedVar Nothing) . newTVar
    newNamedVar n = liftM (NamedVar (Just n)) . newTVar
    readVar (NamedVar mn v) =
        do case mn of
             Just n -> logAccess $ "reading " ++ n
             Nothing -> return ()
           readTVar v
    writeVar (NamedVar mn v) x =
        do case mn of
             Just n -> logAccess $ "writing " ++ n
             Nothing -> return ()
           writeTVar v x
#else
instance HasVars STM where
    type Var STM = TVar
    newVar = newTVar
    readVar = readTVar
    writeVar = writeTVar
#endif

data GenericChan a = UnboundedChan (TChan a)
                   | BoundedChan (BTChan a)

instance HasChans STM where
    type Chan STM = GenericChan
    newChan = newTChan >>= return . UnboundedChan
    newBoundedChan n = newBTChan n >>= return . BoundedChan
    readChan c =
        case c of
          UnboundedChan uc -> readTChan uc
          BoundedChan bc -> readBTChan bc
    writeChan c x =
        case c of
          UnboundedChan uc -> writeTChan uc x
          BoundedChan bc -> writeBTChan bc x
    isEmptyChan c =
        case c of
          UnboundedChan uc -> isEmptyTChan uc
          BoundedChan bc -> isEmptyBTChan bc

instance HasChans IO where
    type Chan IO = GenericChan
    newChan = atomically newChan
    newBoundedChan n = atomically (newBoundedChan n)
    readChan c = atomically (readChan c)
    writeChan c x = atomically (writeChan c x)
    isEmptyChan c = atomically (isEmptyChan c)

instance HasMVars STM where
    type MVar STM = TMVar
    newMVar = newEmptyTMVar
    takeMVar = takeTMVar
    putMVar = putTMVar
    tryTakeMVar = tryTakeTMVar
    withMVar mv act =
        do x <- takeMVar mv
           y <- act x
           putMVar mv x
           return y

instance HasVars IO where
    type Var IO = IORef
    newVar = newIORef
    readVar = readIORef
    writeVar = writeIORef
    modifyReadVar = atomicModifyIORef
    modifyVar = modifyIORef
    modifyVar' = modifyIORef'

instance HasMVars IO where
    type MVar IO = MVar.MVar
    newMVar = MVar.newEmptyMVar
    takeMVar = MVar.takeMVar
    putMVar = MVar.putMVar
    tryTakeMVar = safeError "tryTakeMVar is unsafe"
    withMVar = MVar.withMVar

instance HasQSems IO where
    type QSem IO = MSem.MSem Int
    newQSem = MSem.new
    waitQSem = bracketThreadActivity "waiting for sem" . MSem.wait
    signalQSem = bracketThreadActivity "signalling sem" . MSem.signal

instance HasVars (ST s) where
    type Var (ST s) = STRef s
    newVar = newSTRef
    readVar = readSTRef
    writeVar = writeSTRef

instance (HasVars m, Error e) => HasVars (ErrorT e m) where
    type Var (ErrorT e m) = Var m
    newVar = lift . newVar
    readVar = lift . readVar
    writeVar v = lift . writeVar v

instance HasVars m => HasVars (ReaderT r m) where
    type Var (ReaderT r m) = Var m
    newVar = lift . newVar
    readVar = lift . readVar
    writeVar v = lift . writeVar v

newtype VarName a = VarName Int
data VarBox = forall b. VarBox b
data VarState = VarState { vs_counter :: Int
                         , vs_map :: Map Int VarBox
                         }

emptyVarState = VarState 0 Map.empty

runVarState :: State VarState a -> a
runVarState s = evalState s emptyVarState

runVarStateT :: Monad m => StateT VarState m a -> m a
runVarStateT s = evalStateT s emptyVarState

class HasVarState s where
    updateVarState :: (VarState -> VarState) -> s -> s
    getVarState :: s -> VarState

instance HasVarState VarState where
    updateVarState = ($)
    getVarState = id

instance HasVarState s => HasVars (State s) where
    type Var (State s) = VarName
    newVar = newVarSt
    readVar = readVarSt
    writeVar = writeVarSt

instance (HasVarState s, Monad m, Functor m, Applicative m) => HasVars (StateT s m) where
    type Var (StateT s m) = VarName
    newVar = newVarSt
    readVar = readVarSt
    writeVar = writeVarSt

newVarSt a =
    do varId <- gets (vs_counter . getVarState)
       let varName = VarName varId
           varBox = VarBox a
       modify (updateVarState (\vs -> vs { vs_counter = varId + 1
                                         , vs_map = Map.insert varId varBox (vs_map vs)}))
       return varName

readVarSt (VarName varId) =
    do varMap <- gets (vs_map . getVarState)
       case Map.lookup varId varMap of
         Nothing -> safeError "The `impossible' happened."
         Just (VarBox value) -> return (unsafeCoerce value)

writeVarSt (VarName varId) a =
    modify (updateVarState (\vs -> vs { vs_map = Map.insert varId (VarBox a) (vs_map vs)}))

runStateVar' :: (HasVars m) => Var m s -> StrictState.State s a -> m a
runStateVar' ref st =
    do old <- readVar ref
       let (!res, !new) = StrictState.runState st old
       writeVar ref new
       return res
