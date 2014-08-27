{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Urg.
module Mgw.Util.Concurrent
    ( HasConcurrency(..), ThreadType(..), ThreadIdIO, getThreads
    , KillableThreadIdIO, KillException(..), forkKillableThreadIO, killThreadIO
    , wrapForkAction, fork_, createThread, labelCurrentThread
) where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import Mgw.Util.Exception hiding (catch, finally)
import Mgw.Util.Logging
import Mgw.Util.Misc (formatTid, runErrorTorFail)
import Mgw.Util.Sleep (HasSleep(..))
import Mgw.Util.ThreadActivity
import Mgw.Util.VarState

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.Monad.Error (Error, ErrorT)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT, runStateT, get)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (ResourceT, resourceForkIO, runResourceT)
import qualified Control.Monad.Trans.Resource as RT

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Exception.Lifted (catch, finally)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable (Typeable)
import Prelude
import qualified Control.Concurrent as Conc


type ThreadIdIO = Conc.ThreadId

data ThreadType
    = WorkerThread
    | BoundThread
    deriving (Eq, Ord, Read, Show)


class (Show (ThreadId m), Show (KillableThreadId m), HasSleep m)
    => HasConcurrency m where
    type ThreadId m :: *
    type KillableThreadId m :: *
    myThreadId :: m (ThreadId m)
    throwExceptionTo :: Exception e => ThreadId m -> e -> m ()
    fork :: ThreadType -> String -> m () -> m (ThreadId m)
    forkKillableThread :: ThreadType -> String -> m () -> m (KillableThreadId m)
    killThread :: KillableThreadId m -> m ()
    tryKillThread :: ThreadId m -> m ()
    tryKillThread tid = throwExceptionTo tid ThreadKilled

fork_ :: HasConcurrency m => ThreadType -> String -> m () -> m ()
fork_ tt name action = fork tt name action >> return ()

forkHelper get runStateT tt name action =
    get >>= lift . fork tt name . liftM (const ()) . runStateT action

forkHelperK get runStateT tt name action =
    get >>= lift . forkKillableThread tt name . liftM (const ()) . runStateT action

forkInIO :: ThreadType -> String -> IO () -> IO Conc.ThreadId
forkInIO tt name action = forkXX (wrapForkAction name action)
    where
      forkXX =
          if tt == BoundThread && Conc.rtsSupportsBoundThreads
             then Conc.forkOS
             else Conc.forkIO

wrapForkAction :: (MonadIO m, MonadBaseControl IO m) => String -> m () -> m ()
wrapForkAction name action =
    do registerThread name
       tid <- liftIO $ Conc.myThreadId
       liftIO $ logTrace ("thread '" ++ name ++ "' started, thread id: " ++ show tid)
       (action `catch` handler) `finally`
         (unregisterThread  >> liftIO (logTrace ("thread '" ++ name ++ "' terminated")))
    where
      handler (e::SomeException) =
          do activity <- getThreadActivity
             liftIO $ reportTooManyOpenFiles' ("Thread `" ++ name ++ "`") e
                        (notTooManyOpen activity)

      notTooManyOpen activity e =
          let msg = ("Thread `" ++ name ++
                     "' terminated with an exception while " ++
                     activity ++ ": " ++ show e)
          in logError msg

labelCurrentThread :: (MonadIO m, MonadBaseControl IO m) => String -> m () -> m ()
labelCurrentThread name action =
    do (mx, tid) <- getThreadActivity'
       case mx of
         Just td ->
             liftIO $
             logWarn ("Thread " ++ show tid ++ " already registered as `" ++
                      td_name td ++ "'")
         Nothing -> wrapForkAction name action

instance HasConcurrency IO where
    type ThreadId IO = Conc.ThreadId
    type KillableThreadId IO = KillableThreadIdIO
    myThreadId = Conc.myThreadId
    fork tt name action = forkInIO tt name action
    forkKillableThread = forkKillableThreadIO
    killThread = killThreadIO
    throwExceptionTo  = throwTo

instance HasConcurrency m => HasConcurrency (StateT s m) where
    type ThreadId (StateT s m) = ThreadId m
    type KillableThreadId (StateT s m) = KillableThreadId m
    myThreadId = lift myThreadId
    throwExceptionTo tid e = lift (throwExceptionTo tid e)
    fork = forkHelper get runStateT
    forkKillableThread = forkHelperK get runStateT
    killThread = lift . killThread


instance HasConcurrency m => HasConcurrency (ReaderT r m) where
    type ThreadId (ReaderT r m) = ThreadId m
    type KillableThreadId (ReaderT r m) = KillableThreadId m
    myThreadId = lift myThreadId
    throwExceptionTo tid e = lift (throwExceptionTo tid e)
    fork = forkHelper ask runReaderT
    forkKillableThread = forkHelperK ask runReaderT
    killThread = lift . killThread

instance (Show e, Error e, HasConcurrency m) => HasConcurrency (ErrorT e m) where
    type ThreadId (ErrorT e m) = ThreadId m
    type KillableThreadId (ErrorT e m) = KillableThreadId m
    myThreadId = lift myThreadId
    throwExceptionTo tid e = lift (throwExceptionTo tid e)
    fork tt name = lift . fork tt name . runErrorTorFail
    forkKillableThread tt name = lift . forkKillableThread tt name . runErrorTorFail
    killThread = lift . killThread

instance (MonadIO m, HasSleep m, MonadBaseControl IO m) => HasConcurrency (ResourceT m) where
    type ThreadId (ResourceT m) = ThreadId IO
    type KillableThreadId (ResourceT m) = KillableThreadId IO
    myThreadId = liftIO myThreadId
    throwExceptionTo tid e = liftIO (throwExceptionTo tid e)
    fork BoundThread _ _ = fail "Forking bound threads not supported."
    fork _ name action = resourceForkIO (wrapForkAction name action)
    forkKillableThread BoundThread _ _ = fail "Forking bound threads not supported."
    forkKillableThread WorkerThread name unpreparedAction =
        do (goodAction, mkKTid) <- prepareKillableForkThread unpreparedAction
           tid <- resourceForkIO (wrapForkAction name goodAction)
           return (mkKTid tid)
    killThread = liftIO . killThread

data KillableThreadIdIO
    = KillableThreadId
      { kt_threadId :: !Conc.ThreadId
      , _kt_waitSem  :: !(QSem IO)
      }

instance Show KillableThreadIdIO where
    showsPrec n tid =
        showParen (n > 10) $
          showString "K" .
          showString (formatTid (kt_threadId tid))

data KillException = KillException
     deriving (Show, Typeable)

instance Exception KillException

prepareKillableForkThread action =
    do sem <- liftIO (newQSem 1)
       return ( (liftIO (waitQSem sem) >> action) `catch` (\(_::KillException) -> return ())
                `finally` (liftIO (logNote ("signalling qsem")) >> liftIO (signalQSem sem))
              , \tid -> KillableThreadId tid sem
              )

forkKillableThreadIO :: ThreadType -> String -> IO () -> IO (KillableThreadIdIO)
forkKillableThreadIO tt name unpreparedAction =
    do (goodAction, mkKTid) <- prepareKillableForkThread unpreparedAction
       tid <- forkInIO tt name goodAction
       return (mkKTid tid)

killThreadIO :: KillableThreadIdIO -> IO ()
killThreadIO (KillableThreadId tid sem) =
    do bracketThreadActivity ("throwing exception to " ++ show tid) (throwTo tid KillException)
       waitQSem sem

createThread :: String -> ResourceT IO () -> IO () -> ResourceT IO ()
createThread name action shutdown =
    do tid <- liftIO $ fork WorkerThread name $
              wrap (runResourceT (RT.register logEnd >> logStart >> action))
       _ <- RT.register (logInfoWithTag "threads" ("Killing " ++ name ++ " thread.") >>
                         tryKillThread tid)
       return ()
    where
      wrap = wrapTK . wrapIO
      wrapTK realwork =
          catch realwork $ \ThreadKilled -> return ()
      wrapIO realwork =
          catchIO realwork $ \ioe ->
          case ioe of
            IOError { ioe_type = ResourceVanished } -> logInfo (name ++ ": " ++ show ioe)
            _ -> throwIO ioe
      logStart = logInfoWithTag "threads" (name ++ " thread started.")
      logEnd =
          do shutdown
             logInfoWithTag "threads" (name ++ " thread terminated.")
