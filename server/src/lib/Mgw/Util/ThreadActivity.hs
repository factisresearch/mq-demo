{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Mgw.Util.ThreadActivity
    ( HasThreadActivity(..), ThreadActivity, ThreadData(..)
    , getThreadActivities, logThreadActivities
    , registerThread, unregisterThread, getThreads, getThreadActivity, getThreadActivity'
    )
where
#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Prelude

import Control.Exception (SomeException, throwIO)
import Control.Exception.Lifted (catch, finally)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM (STM)                   -- Mgw.Util.STM introduces cycle

import Data.Map (Map)

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as Map
import qualified Control.Concurrent as Conc

----------------------------------------
-- SITE-PACKAGES
----------------------------------------

import Text.Printf (printf)

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.IORef
import Mgw.Util.Misc (formatTid)
import Mgw.Util.Logging


type ThreadActivity = (String, Maybe String)

data ThreadData
    = ThreadData
      { td_name       :: String
      , td_activity   :: IORef ThreadActivity
      }

_DEFAULT_THREAD_ACTIVITY_ :: ThreadActivity
_DEFAULT_THREAD_ACTIVITY_ = ("running", Nothing)

threadsVar :: IORef (Map Conc.ThreadId ThreadData)
threadsVar = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE threadsVar #-}

getThreads :: IO [String]
getThreads = liftM (map td_name . Map.elems) (readIORef threadsVar)

getThreadActivities :: IO [(String, String, String)]
getThreadActivities =
    do tmap <- readIORef threadsVar
       mapM mkTuple (Map.toList tmap)
    where
      mkTuple (k, (ThreadData n ioref)) =
          do activity <- readIORef ioref
             return (formatTid k, n, formatThreadActivity activity)

formatThreadActivity :: ThreadActivity -> String
formatThreadActivity (x, Nothing) = x
formatThreadActivity (x, Just y)
    | y /= fst _DEFAULT_THREAD_ACTIVITY_ = y ++ " -> " ++ x
    | otherwise = x

class HasThreadActivity m where
    setThreadActivity :: String -> m ()
    bracketThreadActivity :: String -> m a -> m a

instance HasThreadActivity IO where
    setThreadActivity = setThreadActivityIO_
    bracketThreadActivity s action =
        do old <- setThreadActivityIO s
           (action `catch` rethrow) `finally` resetThreadActivityIO old
        where
          rethrow :: SomeException -> IO a
          rethrow exc =
              do logInfoWithTag "exc" ("Exception while " ++ s ++ ": " ++ show exc)
                 throwIO exc


instance HasThreadActivity STM where
    setThreadActivity s = unsafePerformM (setThreadActivityIO_ s)
    bracketThreadActivity s action =
        do old <- unsafePerformM (setThreadActivityIO s)
           res <- action
           unsafePerformM (resetThreadActivityIO old)
           return res

setThreadActivityIO :: String -> IO ThreadActivity
setThreadActivityIO new = modifyThreadActivityIO f
    where
      f cur@(now, _old)
          | new == now = cur
          | otherwise = (new, Just now)

setThreadActivityIO_ :: String -> IO ()
setThreadActivityIO_ s = setThreadActivityIO s >>= \x -> x `seq` return ()


resetThreadActivityIO :: ThreadActivity -> IO ()
resetThreadActivityIO (oldCur, _) = modifyThreadActivityIO f >>= \x -> x `seq` return ()
    where
      f (curCur, _) = (oldCur, Just curCur)

modifyThreadActivityIO  :: (ThreadActivity -> ThreadActivity) -> IO ThreadActivity
modifyThreadActivityIO  f =
    do m <- readIORef threadsVar
       tid <- Conc.myThreadId
       x <-
           case Map.lookup tid m of
             Just (ThreadData { td_activity = ioRef }) ->
                  atomicModifyIORef ioRef (\x -> (f x, x))
             Nothing -> return _DEFAULT_THREAD_ACTIVITY_
       logTraceWithTag "threadactivity" (show x)
       return x

logThreadActivities :: IO ()
logThreadActivities =
    do logNoteOnStderr "All running threads with their activities:"
       threadActivities <- getThreadActivities
       let formatThread (tid,name,activity) =
               printf "%-6s  %-60s  %-25s" tid (take 60 name) activity
       mapM_ (logNoteOnStderr . formatThread) threadActivities

modMap f = liftIO $ atomicUpdateIORef threadsVar f

registerThread name =
    liftIO $ newIORef _DEFAULT_THREAD_ACTIVITY_ >>= insert
    where
      insert ioref =
          liftIO Conc.myThreadId >>= \tid -> modMap (\m -> Map.insert tid (ThreadData name ioref) m)

unregisterThread = liftIO Conc.myThreadId >>= \tid -> modMap (\m -> Map.delete tid m)

unsafePerformM :: Monad m => IO a -> m a
unsafePerformM io = unsafePerformIO (io >>= return . return)

getThreadActivity' =
    do m <- liftIO $ readIORef threadsVar
       tid <- liftIO $ Conc.myThreadId
       let mx = Map.lookup tid m
       return (mx, tid)

getThreadActivity =
    do mx <- getThreadActivity'
       case mx of
         (Just (ThreadData { td_activity = ioRef }), _) ->
             liftM formatThreadActivity (liftIO $ readIORef ioRef)
         (Nothing, _) ->
             return (formatThreadActivity _DEFAULT_THREAD_ACTIVITY_)
