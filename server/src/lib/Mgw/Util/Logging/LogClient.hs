{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mgw.Util.Logging.LogClient
    ( module Mgw.Util.Logging.Core
    , HsLoggerHandler(..), LogT, adjustSourceFilePath, runLogT
    , doLogWithLevel, withLogId, unsafeDoLog
    ) where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Logging.Core
import Mgw.Util.Logging.LogFormatting
import Mgw.Util.Fail

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.Monad.Trans.Resource (ResourceT)

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, MonadTrans(..))
import Control.Monad.Identity (Identity(..))
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
#if __GLASGOW_HASKELL__ >= 704
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (ST, unsafeIOToST)
#endif
import Control.Monad.Writer.Lazy (WriterT)
import Control.Monad.Writer.Strict (WriterT)
import Control.Monad.Error (ErrorT, Error)
import Control.Monad.Maybe (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Concurrent (myThreadId)
import Control.Concurrent.STM (STM)             -- Mgw.Util.STM introduces cycle
import GHC.Conc (unsafeIOToSTM)

import Data.Monoid (Monoid)

import Data.IORef (readIORef, atomicModifyIORef')
import Data.Sequence ((|>))

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Time (getClockTime)

import qualified Data.ByteString.Lazy as BSL
import qualified System.Log as Log
import qualified System.Log.Formatter as LogF
import qualified System.Log.Handler as LogH

logMsg :: FilePath
       -> LineNo
       -> LogLevel
       -> ExtraLogInfo
       -> Bool
       -> Either String BSL.ByteString
       -> IO ()
logMsg file line level extraInfo insideStm msg =
    do enabled <- {-# SCC "isEnabled/logMsg/Logging" #-} isEnabled
       if enabled
          then do ts <- {-# SCC "getClockTime/logMsg/Logging" #-} getClockTime
                  tid <- {-# SCC "myThreadId/logMsg/Logging" #-} myThreadId
                  let logmsg =  LogMessage { lm_time = ts
                                           , lm_threadId = tid
                                           , lm_insideStm = insideStm
                                           , lm_file = file
                                           , lm_line = line
                                           , lm_level = level
                                           , lm_extraInfo = extraInfo
                                           , lm_msg = msg }
                  didEnqueue <-
                      ({-# SCC "atomicModifyIORef/logMsg/Logging" #-}
                       atomicModifyIORef' _LOGQUEUE_ (enqueue logmsg))
                  unless (didEnqueue || level < WARN) $ outputToScreen logmsg
                  return ()
          else return ()
    where
      isEnabled =
          do minlevel <- readIORef _MINLEVEL_
             return $ level >= minlevel
      enqueue msg (enabled, curQ)
          | enabled = let newQ = curQ |> msg in ((enabled,newQ), newQ `seq` True)
          | otherwise = ((enabled, curQ), False)
      outputToScreen logmsg =
          case lm_msg logmsg of
            Left msg ->
                do pref <- getLogMsgPrefix logmsg False
                   hPutStrLn stderr (pref ++ msg)
            Right _ -> return ()

instance LogMonad IO where
    doLog file line level extraInfo msg = logMsg file line level extraInfo False msg

instance LogMonad STM where
    doLog file line level ei msg = unsafeIOToSTM $ logMsg file line level ei True msg

instance LogMonad m => LogMonad (Lazy.StateT s m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

instance LogMonad m => LogMonad (Strict.StateT s m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

instance (Monoid s, LogMonad m) => LogMonad (Control.Monad.Writer.Lazy.WriterT s m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

instance (Monoid s, LogMonad m) => LogMonad (Control.Monad.Writer.Strict.WriterT s m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

instance LogMonad Identity where
    doLog file line level extraInfo msg =
        Identity $ unsafePerformIO (logMsg file line level extraInfo False msg)

instance LogMonad Fail where
    doLog file line level extraInfo msg =
        unsafePerformIO (logMsg file line level extraInfo False msg)
        `seq` Ok ()

instance LogMonad m => LogMonad (MaybeT m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

instance LogMonad m => LogMonad (ReaderT r m) where
    doLog file line level ei msg = lift (doLog file line level ei msg)

instance (Error e, LogMonad m) => LogMonad (ErrorT e m) where
    doLog file line level ei msg = lift (doLog file line level ei msg)

instance (LogMonad m, Monoid w) => LogMonad (RWST r w s m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

instance LogMonad (ST s) where
    doLog file line level extraInfo msg = unsafeIOToST (doLog file line level extraInfo msg)

instance LogMonad m => LogMonad (ResourceT m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

doLogWithLevel :: LogMonad m => String -> Int -> LogLevel -> String -> m ()
doLogWithLevel fn li level msg = doLog (adjustSourceFilePath fn) li level noExtraLogInfo (Left msg)

newtype LogT m a = LogT (ReaderT (LogFun m) m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => LogMonad (LogT m) where
    doLog file line level extraInfo msg =
        LogT $ ask >>= \f -> lift $ f file line level extraInfo msg

instance MonadTrans LogT where
    lift = LogT . lift

runLogT :: LogT m a -> LogFun m -> m a
runLogT (LogT r) f = runReaderT r f

data HsLoggerHandler = HsLoggerHandler

instance LogH.LogHandler HsLoggerHandler where
    setLevel h _ = h
    getLevel _ = Log.DEBUG
    setFormatter h _ = h
    getFormatter _ = LogF.nullFormatter
    handle h (prio, msg) logname =
        LogH.emit h (prio, msg) logname
    emit _h (prio, msg) logname =
        doLog logname 0 (logLevelFromHsLoggerPriority prio) noExtraLogInfo (Left msg)
    close _ = return ()

unsafeDoLog :: FilePath
            -> LineNo
            -> LogLevel
            -> ExtraLogInfo
            -> Either String BSL.ByteString
            -> a
            -> a
unsafeDoLog fp line level info dat val =
    unsafePerformIO $ do doLog fp line level info dat
                         return val
