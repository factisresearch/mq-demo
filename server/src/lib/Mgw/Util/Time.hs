{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Mgw.Util.Time
    ( HasTime(..), measure, measure_, measureTime, measureTime_
    , formatClockTime, formatClockTimeForHuman
    , StaticTimeT, runCurrentStaticTimeT, runEpochStaticTimeT, forwardTime
) where

#include "src/macros.h"

import Control.Applicative
import Control.Monad.Trans.Resource (ResourceT)

import Mgw.Util.STM
import Data.Monoid (Monoid)
import Control.Monad.Error (ErrorT, Error)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, MonadIO, lift)
import qualified Control.Monad.Writer.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified System.Time as T

import Mgw.Model.XsdTypes (XsdDateTime, getCurrentXsdDateTime, clockTimeToXsdDateTime,
                           formatXsdDateTime, formatXsdDateTimeForHuman)
import Mgw.Util.TimeSpan (TimeSpan, diffClockTimes)

class Monad m => HasTime m where
    getXsdDateTime :: m XsdDateTime
    getSecondsSinceEpoch :: m Integer
    getClockTime :: m T.ClockTime

measure :: HasTime m => m a -> m (TimeSpan, a)
measure = measureTime

measure_ :: HasTime m => m () -> m TimeSpan
measure_ = measureTime_

measureTime :: HasTime m => m a -> m (TimeSpan, a)
measureTime ma =
    do t0 <- getClockTime
       a <- ma
       t1 <- a `seq` getClockTime
       let diff = t1 `diffClockTimes` t0
       return (diff, a)

measureTime_ :: HasTime m => m () -> m TimeSpan
measureTime_ ma =
    do (diff, ()) <- measureTime ma
       return diff

instance HasTime IO where
    getXsdDateTime = getCurrentXsdDateTime
    getClockTime = T.getClockTime
    getSecondsSinceEpoch =
        do T.TOD n _ <- T.getClockTime
           return n

instance HasTime STM where
    getXsdDateTime = unsafeIOToSTM getXsdDateTime
    getClockTime = unsafeIOToSTM getClockTime
    getSecondsSinceEpoch = unsafeIOToSTM getSecondsSinceEpoch

instance HasTime m => HasTime (ReaderT r m) where
    getXsdDateTime = lift getXsdDateTime
    getSecondsSinceEpoch = lift getSecondsSinceEpoch
    getClockTime = lift getClockTime

instance (HasTime m, Error e) => HasTime (ErrorT e m) where
    getXsdDateTime = lift getXsdDateTime
    getSecondsSinceEpoch = lift getSecondsSinceEpoch
    getClockTime = lift getClockTime

instance HasTime m => HasTime (Lazy.StateT s m) where
    getXsdDateTime = lift getXsdDateTime
    getSecondsSinceEpoch = lift getSecondsSinceEpoch
    getClockTime = lift getClockTime

instance HasTime m => HasTime (Strict.StateT s m) where
    getXsdDateTime = lift getXsdDateTime
    getSecondsSinceEpoch = lift getSecondsSinceEpoch
    getClockTime = lift getClockTime

instance (Monoid r, HasTime m) => HasTime (Lazy.WriterT r m) where
    getXsdDateTime = lift getXsdDateTime
    getSecondsSinceEpoch = lift getSecondsSinceEpoch
    getClockTime = lift getClockTime

instance (Monoid w, HasTime m) => HasTime (Strict.WriterT w m) where
    getXsdDateTime = lift getXsdDateTime
    getSecondsSinceEpoch = lift getSecondsSinceEpoch
    getClockTime = lift getClockTime

instance HasTime m => HasTime (ResourceT m) where
    getXsdDateTime = lift getXsdDateTime
    getSecondsSinceEpoch = lift getSecondsSinceEpoch
    getClockTime = lift getClockTime

formatClockTime :: T.ClockTime -> String
formatClockTime = formatXsdDateTime . clockTimeToXsdDateTime

formatClockTimeForHuman :: T.ClockTime -> String
formatClockTimeForHuman = formatXsdDateTimeForHuman . clockTimeToXsdDateTime

newtype StaticTimeT m a
    = StaticTimeT (Strict.StateT T.ClockTime m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans StaticTimeT where
    lift = StaticTimeT . lift

runCurrentStaticTimeT :: HasTime m => StaticTimeT m a -> m a
runCurrentStaticTimeT (StaticTimeT ra) =
    do t <- getClockTime
       Strict.evalStateT ra t

runEpochStaticTimeT :: Monad m => Integer -> StaticTimeT m a -> m a
runEpochStaticTimeT i (StaticTimeT ra) = Strict.evalStateT ra (T.TOD i 0)

instance Monad m => HasTime (StaticTimeT m) where
    getXsdDateTime = StaticTimeT (Strict.gets clockTimeToXsdDateTime)
    getClockTime = StaticTimeT Strict.get
    getSecondsSinceEpoch = StaticTimeT (Strict.gets $ \(T.TOD i _) -> i)

forwardTime :: Monad m => Integer -> StaticTimeT m ()
forwardTime seconds = StaticTimeT $
    do T.TOD i x <- Strict.get
       Strict.put (T.TOD (i + seconds) x)
