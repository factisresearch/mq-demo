{-# LANGUAGE TypeFamilies, FlexibleContexts, CPP, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-missing-signatures #-}

module Mgw.Util.Tx
    ( HasTx(..), HasRetry(..), InTx(..)
    , readChanWithTimeout, trackLazyTx, trackTx, force
    , retryUnless, retryWhen
) where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Prelude
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception (throwIO, catch, BlockedIndefinitelyOnSTM, Exception)
import qualified Control.Exception as E
import Data.Typeable

import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.Monad.Trans.Resource (ResourceT)

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Logging
import Mgw.Util.VarState
import Mgw.Util.TimeSpan
import Mgw.Util.Sleep
import Mgw.Util.Force
import Mgw.Util.STM (STM, atomically, atomically', retry, ioInSTM)
import Mgw.Util.Time (HasTime, measure)

type Map = Map.Map

class (Monad m, Monad (Tx m), HasVars (Tx m)) => HasTx m where
    type Tx m :: * -> *
    runTx' :: (Tx m) a -> m a
    runNamedTx' :: String -> Tx m a -> m a
    trackTx' :: Maybe String -> FilePath -> Int -> Tx m a -> m (Int, a)
    trackTx' mn _ _ tx =
        do x <-
               case mn of
                 Just name -> runNamedTx' name tx
                 Nothing -> runTx' tx
           return (1, x)
    evaluate :: a -> m a

class (Monad m, Monad (CurTx m)) => InTx m where
    type CurTx m :: * -> *
    inTx :: CurTx m a -> m a

instance InTx IO where
    type CurTx IO = STM
    inTx = runTx

instance InTx STM where
    type CurTx STM = STM
    inTx = id

class HasRetry m where
    retryTx :: m a


retryWhen :: (HasRetry m, Monad m) => Bool -> m ()
retryWhen = flip when retryTx

retryUnless :: (HasRetry m, Monad m) => Bool -> m ()
retryUnless = flip unless retryTx

instance (Error e, HasTx m) => HasTx (ErrorT e m) where
    type Tx (ErrorT e m) = Tx m
    runTx' = lift . runTx'
    runNamedTx' n = lift . runNamedTx' n
    trackTx' name f n tx = lift (trackTx' name f n tx)
    evaluate = lift . evaluate

instance (HasTx m) => HasTx (StateT s m) where
    type Tx (StateT s m) = Tx m
    runTx' = lift . runTx'
    runNamedTx' n = lift . runNamedTx' n
    trackTx' name f n tx = lift (trackTx' name f n tx)
    evaluate = lift . evaluate

instance (HasTx m) => HasTx (ReaderT e m) where
    type Tx (ReaderT e m) = Tx m
    runTx' = lift . runTx'
    runNamedTx' n = lift . runNamedTx' n
    trackTx' name f n tx = lift (trackTx' name f n tx)
    evaluate = lift . evaluate

instance HasTx m => HasTx (LogT m) where
    type Tx (LogT m) = Tx m
    runTx' = lift . runTx'
    runNamedTx' n = lift . runNamedTx' n
    trackTx' name f n tx = lift (trackTx' name f n tx)
    evaluate = lift . evaluate

instance HasTx m => HasTx (ResourceT m) where
    type Tx (ResourceT m) = Tx m
    runTx' = lift . runTx'
    runNamedTx' n = lift . runNamedTx' n
    trackTx' name f n tx = lift (trackTx' name f n tx)
    evaluate = lift . evaluate


instance HasRetry STM where
    retryTx = retry

readChanWithTimeout :: (HasTimeout m, HasTx m, HasChans (Tx m), HasMVars (Tx m)) =>
                       TimeSpan
                    -> Chan (Tx m) a
                    -> m (Maybe a)
readChanWithTimeout ts chan =
    do -- do a cheap check first...
       ma <- checkData
       if isJust ma
          then return ma
          else do mvar <- runTx newMVar
                  _ <- timeout ts (runTx $ readChan chan >>= putMVar mvar)
                  runTx (tryTakeMVar mvar)
    where
      checkData =
          runTx $
          do hasData <- liftM not (isEmptyChan chan)
             if hasData
                then liftM Just (readChan chan)
                else return Nothing

tryThreshold :: Int
tryThreshold = 10

globalRetryThreshold :: Int
globalRetryThreshold = 3000

globalRetryCountMap :: IORef (Map (Int, String) Int)
globalRetryCountMap = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE globalRetryCountMap #-}

trackTxTag :: Tags
trackTxTag = "trackTx"

trackTx :: HasTx m => Maybe String -> FilePath -> Int -> Tx m a -> m a
trackTx name f n tx = liftM snd $ trackTx' name f n tx

data BlockedIndefinitelyOnNamedTransaction =
    BlockedIndefinitelyOnNamedTransaction (Maybe String) FilePath Int
                                          deriving (Typeable)

instance Show BlockedIndefinitelyOnNamedTransaction where
    show (BlockedIndefinitelyOnNamedTransaction mName file line) =
        "thread blocked indefinitely in STM transaction" ++
        (case mName of
           Just s -> " " ++ show s
           Nothing -> "") ++
        ": " ++ file ++ " at line " ++ show line

instance Exception BlockedIndefinitelyOnNamedTransaction

instance HasTx IO where
    type Tx IO = STM
    evaluate = E.evaluate
    runTx' = atomically
    runNamedTx' = atomically'
    trackTx' mName file line txm =
        do counter <- newIORef 0
           let wrappedTx =
                   do (i, warn) <- ioInSTM $ atomicModifyIORef counter incCounter
                      when warn $
                           logWarnWithTag trackTxTag (msgPrefix ++ "reached try count of "
                                                      ++ show i)
                      txm
           res <- case mName of
                    Just name -> runNamedTx' name wrappedTx
                    Nothing -> runNamedTx' (file ++ ":" ++ show line) wrappedTx
                  `catch` (\(_::BlockedIndefinitelyOnSTM) ->
                               throwIO (BlockedIndefinitelyOnNamedTransaction mName file line))
           i <- readIORef counter
           when (i > tryThreshold) $
                logDebugWithTag trackTxTag $
                  msgPrefix ++ "finished after " ++ show i ++ " tries"
           when (i >= 2) $
                incGlobalRetryCount (i - 1)
           return (i, res)
        where
          incCounter i =
              let j = i + 1
                  n = 2 * tryThreshold
              in (j, (j, j >= n && (j >= 2 * n || j `mod` n == 0)))
          msgPrefix =
              "Transaction " ++ (case mName of
                                   Just n -> "`" ++ n ++ "' "
                                   Nothing -> "") ++
               "at " ++ file ++ ":" ++ show line ++ " "
          incGlobalRetryCount i =
              do let f m = let (oldVal, m') = Map.insertLookupWithKey (\_ _ j -> i + j)
                                                                      (line, file) i m
                           in (m', case oldVal of
                                     Nothing -> i
                                     Just j -> i + j)
                 k <- atomicModifyIORef globalRetryCountMap f
                 when (k > 0 && k `mod` globalRetryThreshold == 0) $
                      logWarnWithTag trackTxTag $
                        msgPrefix ++ "reached global retry count of " ++ show k

trackLazyTx :: (LogMonad m, HasTx m, HasTime m) =>
               Maybe String -> Maybe (a -> ()) -> FilePath -> Int -> Tx m (ForceMe a) -> m a
trackLazyTx mName mExtraEval file line txm =
    do logTraceWithTag tag $ "Starting transaction " ++ name
       (_i, value) <- trackTx' mName file line txm
       logTraceWithTag tag ("Done. Evaluating thunk returned from " ++ name ++ ".")
       (t, res) <- measure (do x <- force value
                               case mExtraEval of
                                 Just f -> evaluate (f x)
                                 Nothing -> return ()
                               return x)
       logDebugWithTag tag ("Evaluated thunk returned from " ++ name ++ " in "
                             ++ displayTimeSpan t ++ ".")
       return res
    where
      tag = "lazyness"
      name = fromMaybe "transaction" mName

force :: HasTx m => ForceMe a -> m a
force fm =
    do x <- evaluate fm
       return $ getValue x
