{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Mgw.Util.STM
    ( readTVar, writeTVar, readTChan, writeTChan, takeTMVar, readTMVar, putTMVar, tryTakeTMVar
    , readTVarIO, writeTVarIO, modifyTVarIO, modifyTVarIO'
    , modifyTVar, modifyTVar', isEmptyTChan, isEmptyTMVar, atomically, atomically', retry
    , maybeRetry
    , STM.unsafeIOToSTM
    , STM.STM, STM.TMVar, STM.TVar, STM.TChan, STM.orElse
    , STM.newTVarIO, STM.newTMVarIO, STM.newTChanIO, STM.newEmptyTMVarIO
    , STM.newTVar, STM.newEmptyTMVar, STM.newTChan, STM.newTMVar, throwSTM
    , TBMChan, newTBMChan, readTBMChan, writeTBMChan, writeTBMChanFailIfClosed
    , closeTBMChan, realTBMChan, isClosedTBMChan
    , isEmptyTBMChan, peekTBMChan
    , ioInSTM, runStateTVar'
    , BT.BTChan, BT.newBTChan, readBTChan, writeBTChan, isEmptyBTChan
    )
where

import Control.Monad (liftM)
import qualified GHC.Conc as STM
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exc
import qualified Control.Concurrent.STM.TBMChan as TBM
import qualified Control.Concurrent.STM.BTChan as BT
import qualified Control.Monad.State.Strict as StrictState

import Mgw.Util.ThreadActivity ( bracketThreadActivity )

atomically stm =
    bracketThreadActivity "executing STM transaction" (STM.atomically stm)

atomically' name stm =
    bracketThreadActivity ("executing " ++ name) (STM.atomically stm)

retry :: STM.STM a
retry =
    bracketThreadActivity "retrying stm action" STM.retry

readTVar tv =
    bracketThreadActivity "reading TVar" $ STM.readTVar tv
readTVarIO tv =
    bracketThreadActivity "reading TVar" $ STM.readTVarIO tv

writeTVar tv v =
    bracketThreadActivity "writing TVar" $ STM.writeTVar tv v
writeTVarIO tv v =
    bracketThreadActivity "writing TVar" $ atomically (STM.writeTVar tv v)

modifyTVar tv f =
    do x <- readTVar tv
       writeTVar tv (f x)

modifyTVar' tv f =
    do x <- readTVar tv
       writeTVar tv $! f x

modifyTVarIO tv f = atomically $ modifyTVar tv f
modifyTVarIO' tv f = atomically $ modifyTVar' tv f

readTChan tc =
    bracketThreadActivity "reading TChan" $ STM.readTChan tc

writeTChan tc v =
    bracketThreadActivity "writing TChan" $ STM.writeTChan tc v

readTMVar tmv =
    bracketThreadActivity "reading TMVar" $ STM.readTMVar tmv

takeTMVar tmv =
    bracketThreadActivity "taking TMVar" $ STM.takeTMVar tmv

tryTakeTMVar tmv =
    bracketThreadActivity "trying to take TMVar" $ STM.tryTakeTMVar tmv

putTMVar tmv v =
    bracketThreadActivity "putting TMVar" $ STM.putTMVar tmv v

isEmptyTChan tc =
    bracketThreadActivity "checking if TChan is empty" $  STM.isEmptyTChan tc

isEmptyTMVar tm =
    bracketThreadActivity "checking if MVar is empty" $ STM.isEmptyTMVar tm

throwSTM exc =
    STM.unsafeIOToSTM (Exc.throwIO exc)

maybeRetry :: STM.STM (Maybe a) -> STM.STM a
maybeRetry action = action >>= maybe retry return

data TBMChan a = TBMChan String (TBM.TBMChan a)

newTBMChan :: String -> Int -> STM.STM (TBMChan a)
newTBMChan name = liftM (TBMChan name) . TBM.newTBMChan

readTBMChan (TBMChan name chan)  =
    bracketThreadActivity ("reading TBMChan `" ++ name ++ "'") $ TBM.readTBMChan chan

peekTBMChan (TBMChan name chan)  =
    bracketThreadActivity ("peek TBMChan `" ++ name ++ "'") $ TBM.peekTBMChan chan

writeTBMChan (TBMChan name chan) value =
    bracketThreadActivity ("writing TBMChan `" ++ name ++ "'") $ TBM.writeTBMChan chan value

writeTBMChanFailIfClosed chan@(TBMChan name _) value =
    do b <- isClosedTBMChan chan
       if b then fail ("TBMChan " ++ name ++ " already closed") else writeTBMChan chan value

closeTBMChan (TBMChan name chan) =
    bracketThreadActivity ("closing TBMChan `" ++ name ++ "'") $ TBM.closeTBMChan chan

realTBMChan (TBMChan _ chan) = chan

isClosedTBMChan (TBMChan name c) =
    bracketThreadActivity ("checking if TBMChan `" ++ name ++ "' is closed") $ TBM.isClosedTBMChan c

isEmptyTBMChan (TBMChan name c) =
    bracketThreadActivity ("checking if TBMChan `" ++ name ++ "' is empty") $ TBM.isEmptyTBMChan c


{-# INLINE ioInSTM #-}
ioInSTM :: IO a -> STM.STM a
ioInSTM = STM.unsafeIOToSTM

runStateTVar' :: STM.TVar s -> StrictState.State s a -> STM.STM a
runStateTVar' ref st =
    do old <- readTVar ref
       let (!res, !new) = StrictState.runState st old
       writeTVar ref new
       return res

readBTChan :: BT.BTChan a -> STM.STM a
readBTChan c =
    bracketThreadActivity "reading BTChan" (BT.readBTChan c)

writeBTChan :: BT.BTChan a -> a -> STM.STM ()
writeBTChan c x =
    bracketThreadActivity "writing BTChan" (BT.writeBTChan c x)

isEmptyBTChan :: BT.BTChan a -> STM.STM Bool
isEmptyBTChan c =
    bracketThreadActivity "checking if BTChan is empty" (BT.isEmptyBTChan c)
