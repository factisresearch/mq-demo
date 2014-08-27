{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Mgw.Util.ExitAction.Base
    ( ExitActionId, ExitActionBehavior, ExitActionDelay(..), ExitActionPriority(..)
    , registerExitAction, unregisterExitAction, runExitActions, withExitActions''
    )
where

#include "src/macros.h"

import Prelude
import Data.IORef
import Data.Ord (comparing)
import Control.Monad (liftM)
import Control.Concurrent (ThreadId, myThreadId)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, stderr)
import System.Timeout
import System.Posix.Process
import qualified Data.List as List

import Mgw.Util.Logging
import Mgw.Util.Logging.LogWriter
import Mgw.Util.TimeSpan
import Mgw.Util.MVar
import Mgw.Util.Time (measure)
import Mgw.Util.Exception
    ( SomeException, Handler(..), ExitActionsException(..)
    , catches, throw, throwTo, throwIO, catchSafe
    )
import Mgw.Util.OSMap (OSMap)
import qualified Mgw.Util.OSMap as OSM

newtype ExitActionId = ExitActionId { _unExitActionId :: Int }
    deriving (Eq, Ord, Show, Num)

data ExitState
    = Exiting (Maybe ExitActionMap)
    | Starting
    | Running !ExitActions
    deriving (Show)

data ExitAction
    = ExitAction
    { ea_name     :: String
    , ea_behavior :: ExitActionBehavior
    , _ea_action   :: IO ()
    }

instance Show ExitAction where
    showsPrec p (ExitAction name behavior _) =
        showParen (p > 10) $
        showString "ExitAction { ea_name = " .
        showsPrec 5 name .
        showString ", ea_behavior = " .
        showsPrec 5 behavior .
        showString " }"

type ExitActionBehavior = (ExitActionPriority, ExitActionDelay)

type ExitActionMap = OSMap ExitActionId ExitAction

data ExitActionDelay
    = ExitDelayShort
    | ExitDelayLong
    deriving (Eq, Ord, Show)

data ExitActionPriority
    = ExitActionImportant
    | ExitActionRegular
    deriving (Eq, Ord, Show)

data ExitActions
    = ExitActions
    { ea_nextId :: !ExitActionId
    , ea_actions :: !ExitActionMap
    , ea_tid :: !ThreadId
    } deriving (Show)

mkExitActions :: ThreadId -> ExitActions
mkExitActions = ExitActions 0 OSM.empty

exitActions :: IORef ExitState
exitActions = unsafePerformIO (newIORef Starting)
{-# NOINLINE exitActions #-}

checkExiting :: (ExitActions -> (ExitActions, a)) -> (ExitState -> (ExitState, a))
checkExiting f mea =
    case mea of
      Running curEa ->
          let (newEa, value) = f curEa
          in (newEa `seq` Running newEa, value)
      Starting ->
          throw (ExitActionsNotInitializedException
                 "checkExiting: in state Starting but should be Running")
      Exiting _ ->
          let msg = "in state `Exiting` while doing checkExiting"
          in pureAlert msg $
             throw (ExitActionsAlreadyExitingException msg)

registerExitAction :: String -> ExitActionBehavior -> IO () -> IO ExitActionId
registerExitAction name eb io =
    do logDebug ("Registering exit-action `" ++ name ++ "'.")
       atomicModifyIORef exitActions (checkExiting f)
    where
      f ea =
          let i = ea_nextId ea
          in (ea { ea_nextId = (i+1)
                 , ea_actions = OSM.insert i (ExitAction name eb io) (ea_actions ea)
                 }
             , i
             )

unregisterExitAction :: ExitActionId -> IO ()
unregisterExitAction i =
    do mv <- atomicModifyIORef exitActions (checkExiting f)
       case mv of
         Just (ExitAction { ea_name = name }) ->
             logDebug ("Unregistered exit-action `" ++ name ++ "'.")
         Nothing ->
             logError ("Tried to unregister a non-existing exit action with id " ++ show i ++ "!")
    where
      f ea =
          let (mv, m) = OSM.deleteLookup i (ea_actions ea)
          in (ea { ea_actions = m }, mv)

intToExitCode :: Int -> ExitCode
intToExitCode i = if i == 0 then ExitSuccess else ExitFailure i

runExitActions :: Int -> IO ()
runExitActions exitCode =
    do thisTid <- myThreadId
       mEaTid <- atomicModifyIORef exitActions setExiting
       case mEaTid of
         Nothing ->
             logInfo "Already running exit actions"
         Just eaTid ->
             if eaTid == thisTid
                then do logInfo "We're in the right thread to run the exit actions."
                        mvar <- newEmptyMVar
                        throwIO (ExitActionsTerminateException exitCode mvar)
                else do logInfo ("This thread (" ++ show thisTid ++ ") is the wrong thread to run "
                                  ++ "exit actions.  Throwing exception to " ++ show eaTid ++ ".")
                        mvar <- newEmptyMVar
                        throwTo eaTid (ExitActionsTerminateException exitCode mvar)
                        res <- timeout (1000 * 100) {- 100ms -} (takeMVar mvar)
                        case res of
                          Just () -> logInfo ("Exit action execution delegated successfully")
                          Nothing ->
                              do logWarn ("Could not delegate exit action execution, running " ++
                                          "them by myself")
                                 checkOrSetExitingAndRunExitActions teardownLogging
                                 hPutStrLn stderr
                                     (__FILE__ ++ ":" ++ show __LINE__ ++
                                      ": Nobody heard my request to execute the exit actions! "
                                      ++ "(You use `saveToCatch` all the time, don't you?). "
                                      ++ "I now have to die painfully...")
                                 exitImmediately exitCode'
    where
      exitCode' =
          if exitCode == 0 then ExitSuccess else ExitFailure exitCode
      setExiting mea =
          case mea of
            Exiting _ -> (mea, Nothing)
            Running ea -> (Exiting (Just $ ea_actions ea), Just (ea_tid ea))
            Starting -> throw (ExitActionsNotInitializedException
                               "runExitActions: in state Starting")

checkOrSetExitingAndRunExitActions :: IO () -> IO ()
checkOrSetExitingAndRunExitActions finalization =
    do actions <- atomicModifyIORef exitActions checkExiting
       runTheseExitActions finalization actions
    where
      checkExiting mea =
          case mea of
            Exiting (Just actions) -> (Exiting Nothing, actions)
            Exiting Nothing ->
                let msg = "Already running exit actions!"
                in pureAlert msg $
                   throw (ExitActionsAlreadyExitingException msg)
            Running ea -> (Exiting Nothing, ea_actions ea)
            Starting ->
                throw (ExitActionsNotInitializedException
                       ("checkOrSetExitingAndRunExitActions: in state Starting, should be " ++
                        "in state Running or Exiting"))

runTheseExitActions :: IO () -> OSMap ExitActionId ExitAction -> IO ()
runTheseExitActions finalization m =
    do let (log1, log2) = if OSM.size m > 0 then (logNote, logInfo) else (logDebug, logDebug)
       log1 ("Running " ++ show (OSM.size m) ++ " exit actions...")
       runActions timeAvailable (List.sortBy (comparing ea_behavior) $ OSM.elems m)
       log2 "finished running exit actions"
       atomicModifyIORef exitActions setStarting
       finalization
    where
      timeAvailable = seconds 1
      setStarting mea =
          case mea of
            Exiting _ -> (Starting, ())
            _ ->
                pureAlert ("Internal error: " ++ show mea) $
                (mea, ())
      runActions _remaining [] = return ()
      runActions curRemaining (ea : eas) =
          do let avail = microseconds (asMicroseconds curRemaining `div` (1 + length eas))
             (used, wasTimeout) <-  measure (timeout avail (runAction ea))
             let nextRemaining = curRemaining - used
             case wasTimeout  of
               Nothing ->
                   logAlert ("Exit action `" ++ ea_name ea ++ "' timed out after "
                             ++ displayTimeSpan used ++ ".  Now "
                             ++ displayTimeSpan nextRemaining ++ " remaining.")
               Just () ->
                   logInfo ("Exit action `" ++ ea_name ea ++ "' terminated after "
                            ++ displayTimeSpan used ++ ".  " ++ show (length eas) ++ " actions and "
                            ++ displayTimeSpan nextRemaining ++ " remaining.")
             runActions nextRemaining eas
      timeout _avail =
          {- using timeout currently doesn't work (hangs forever) -}
          liftM Just
      runAction (ExitAction name _behavior action) =
          do logInfo ("Running exit action `" ++ name ++ "'...")
             action `catchSafe`
                        \e ->
                            do let msg = "Exit action " ++ name ++ " failed: " ++ show e
                               hPutStrLn stderr msg
                               logError msg
                               return ()
             logInfo ("Finished with exit action `" ++ name ++ "'.")

withExitActions'' :: IO a -> IO () -> IO (Either ExitCode a)
withExitActions'' io fin =
    do tid <- myThreadId
       hPutStrLn stderr ("Running with exit actions, thread for handling exit actions is " ++
                         show tid)
       () <- atomicModifyIORef exitActions (setRunning tid)
       catches (io >>= \a -> checkOrSetExitingAndRunExitActions fin >> return (Right a))
                   [ Handler $ \e ->
                         do logInfo "Caught exit exception in exit actions thread."
                            checkOrSetExitingAndRunExitActions fin
                            return (Left e)
                   , Handler $ \(ExitActionsTerminateException code mvar) ->
                       do logInfo "Caught exit request exception in exit actions thread."
                          putMVar mvar ()
                          checkOrSetExitingAndRunExitActions fin
                          return (Left (intToExitCode code))
                   , Handler $ \(e :: SomeException) ->
                       do let msg = ("Caught `" ++ show e ++ "'.  Exiting with exit code 1.")
                          logWarn msg
                          hPutStrLn stderr msg
                          checkOrSetExitingAndRunExitActions fin
                          return (Left (ExitFailure 1))
                   ]
    where
      setRunning tid es =
          case es of
            Starting -> (Running $ mkExitActions tid, ())
            Running {} -> throw ExitActionsAlreadyInitializedException
            Exiting {} ->
                let msg = "in state `Exiting` while calling setRunning"
                in pureAlert msg $
                   throw (ExitActionsAlreadyExitingException msg)
