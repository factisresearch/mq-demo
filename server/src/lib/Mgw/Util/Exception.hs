{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.Exception (
      ExitActionsException(..)
    , bracket, bracketOnError, bracketWithResult, catchIO, ignoreIOException
    , logIOException, throwIOException, safeToCatch, catchSafe
    , Exc.Exception, Exc.SomeException, Exc.ErrorCall(..), Exc.Handler(..), Exc.IOException
    , Exc.finally, Exc.throw, Exc.catch, Exc.throwIO, Exc.handle, Exc.evaluate
    , Exc.onException, Exc.throwTo, Exc.catches, Exc.catchJust, Exc.try, Exc.AsyncException(..)
    , IOErrorType, failForSafeException
    , reportTooManyOpenFiles, reportTooManyOpenFiles', handleTooManyOpenFiles
    , tooManyOpenFilesMessage
) where

#include "src/macros.h"

import qualified Control.Exception as Exc
import qualified GHC.IO as GHC
import Data.List (isInfixOf)
import Data.Typeable
import System.Posix.Process (getProcessID)
import System.Process (readProcess)

import Control.Monad
import System.Exit
import System.IO.Error
import System.IO

import Mgw.Util.Fail
import Mgw.Util.Logging
import Mgw.Util.MVar

data ExitActionsException
    = ExitActionsAlreadyExitingException String
    | ExitActionsNotInitializedException String
    | ExitActionsAlreadyInitializedException
    | ExitActionsTerminateException Int (MVar ())
    deriving (Typeable)

instance Show ExitActionsException where
    showsPrec prec exc =
        case exc of
          ExitActionsAlreadyExitingException msg ->
              showParen (prec > 10) $
              showString "ExitActionsAlreadyExitingException " .
              showsPrec 5 msg
          ExitActionsNotInitializedException msg ->
              showParen (prec > 10) $
              showString "ExitActionsNotInitializedException " .
              showsPrec 5 msg
          ExitActionsAlreadyInitializedException ->
              showString "ExitActionsAlreadyExitingException"
          ExitActionsTerminateException ecode _ ->
              showParen (prec > 10) $
              showString "ExitActionsTerminateException " .
              showsPrec 5 ecode

instance Exc.Exception ExitActionsException

bracket
    :: String       -- ^ description of activity
    -> IO a         -- ^ computation to run first (\"acquire resource\")
    -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
    -> (a -> IO c)  -- ^ computation to run in-between
    -> IO c         -- ^ returns the value from the in-between computation
bracket desc first last between =
    Exc.catch (Exc.bracket first last between) $
       \(exc :: Exc.SomeException) ->
           do logInfoWithTag "exc" ("Exception while " ++ desc ++ ": " ++ show exc)
              Exc.throwIO exc

bracketWithResult
    :: String                               -- ^ description of activity
    -> IO a                                 -- ^ computation to run first (\"acquire resource\")
    -> (a
        -> Either Exc.SomeException c
        -> IO b)                            -- ^ computation to run last (\"release resource\")
    -> (a -> IO c)                          -- ^ computation to run in-between
    -> IO c                                 -- ^ returns the value from the in-between computation
bracketWithResult desc before after thing =
  GHC.mask $ \restore ->
      do a <- before
         r <- Exc.catch (restore (thing a)) $
                \(exc :: Exc.SomeException) ->
                    do logInfoWithTag "exc" ("Exception while " ++ desc ++ ": " ++ show exc)
                       _ <- after a (Left exc)
                       Exc.throwIO exc
         _ <- after a (Right r)
         return r

-- | Like 'bracket', but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError
    :: String       -- ^ description of activity
    -> IO a         -- ^ computation to run first (\"acquire resource\")
    -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
    -> (a -> IO c)  -- ^ computation to run in-between
    -> IO c         -- returns the value from the in-between computation
bracketOnError desc before after thing =
  GHC.mask $ \restore ->
      do a <- before
         Exc.catch (restore (thing a)) $
                \(exc :: Exc.SomeException) ->
                    do logInfoWithTag "exc" ("Exception while " ++ desc ++ ": " ++ show exc)
                       _ <- after a
                       Exc.throwIO exc

catchIO :: IO a -> (Exc.IOException -> IO a) -> IO a
catchIO = Exc.catch

ignoreIOException :: IO () -> IO ()
ignoreIOException action =
    action `catchIO` (\_ -> return ())

logIOException msg action =
    action `catchIO` (\e -> do logError (msg ++ ": " ++ show e)
                               Exc.throwIO e)

throwIOException :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IO a
throwIOException ty msg mh mp =
    let err = mkIOError ty msg mh mp
    in Exc.throwIO err

safeToCatch :: Exc.SomeException -> Bool
safeToCatch exc =
    case Exc.fromException exc of
      Just (_ :: Exc.BlockedIndefinitelyOnMVar) -> False
      Nothing ->
          case Exc.fromException exc of
            Just (_ :: Exc.BlockedIndefinitelyOnSTM) -> False
            Nothing ->
                case Exc.fromException exc of
                  Just (_ :: Exc.Deadlock) -> False
                  Nothing ->
                      case Exc.fromException exc of
                        Just (_ :: ExitActionsException) -> False
                        Nothing ->
                            case Exc.fromException exc of
                              Just (async :: Exc.AsyncException) ->
                                  case async of
                                    Exc.StackOverflow -> True
                                    _ -> False
                              Nothing ->
                                  case Exc.fromException exc of
                                    Just (_ :: ExitCode) -> False
                                    Nothing -> True

catchSafe :: IO a -> (Exc.SomeException -> IO a) -> IO a
catchSafe action handler =
    Exc.catchJust conv action handler
    where
      conv someExc = if safeToCatch someExc then (Just someExc) else Nothing

failForSafeException :: IO a -> IO (Fail a)
failForSafeException action =
    catchSafe (liftM Ok action) (\exc -> return (Fail (show exc)))

tooManyOpenFilesMessage :: Exc.Exception e => e -> IO (Maybe String)
tooManyOpenFilesMessage exception =
    do let isTooManyOpenFiles = isInfixOf "Too many open files" (show exception)
       if isTooManyOpenFiles
       then do pid <- getProcessID
               out <- readProcess "lsof" ["-p " ++ show pid] ""
               return (Just ("Too many open files for PID " ++ show pid ++ ": "
                             ++ out))
       else return Nothing

reportTooManyOpenFiles :: Exc.Exception e => String -> e -> IO a
reportTooManyOpenFiles ident exception = reportTooManyOpenFiles' ident exception Exc.throwIO

reportTooManyOpenFiles' :: Exc.Exception e => String -> e -> (e -> IO a) -> IO a
reportTooManyOpenFiles' ident exception otherwise =
    do mMsg <- tooManyOpenFilesMessage exception
       case mMsg of
         Nothing -> otherwise exception
         Just msg ->
             do logError (ident ++ ": " ++ msg)
                Exc.throwIO exception

handleTooManyOpenFiles :: String -> IO a -> IO a
handleTooManyOpenFiles ident action =
    action `Exc.catch` (\(e::Exc.SomeException) -> reportTooManyOpenFiles ident e)
