module Mgw.Util.Sleep
    ( HasSleep(..), HasTimeout(..)
    , sleepDyn, sleepDynWithLogging, sleepDynWithGranularityAndLogging, sleepUntil
    )
where

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Concurrent (threadDelay)

import Control.Monad.Trans (lift)
import Control.Monad.Error (Error, ErrorT, runErrorT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Writer.Strict

import qualified System.Timeout as Sys
import Data.Monoid

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.Monad.Trans.Resource (ResourceT)

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.ThreadActivity (bracketThreadActivity)
import Mgw.Model.XsdTypes
import Mgw.Util.Time (HasTime, getXsdDateTime)
import Mgw.Util.TimeSpan

class Monad m => HasSleep m where
    sleep :: Int -> m ()
    sleepTimeSpan :: TimeSpan -> m ()
    sleepTimeSpan ts = sleep (asMilliseconds ts)

instance HasSleep IO where
    sleep msecs = bracketThreadActivity msg $
                  threadDelay (msecs * 1000)
        where msg = if msecs > 1000
                      then "sleeping for " ++ show (msecs `div` 1000) ++ "s"
                      else "sleeping for " ++ show msecs ++ "ms"

instance HasSleep m => HasSleep (StateT s m) where
    sleep = lift . sleep

instance HasSleep m => HasSleep (ReaderT r m) where
    sleep = lift . sleep

instance (Error e, HasSleep m) => HasSleep (ErrorT e m) where
    sleep = lift . sleep

instance (Monoid e, HasSleep m) => HasSleep (WriterT e m) where
    sleep = lift . sleep

instance HasSleep m => HasSleep (ResourceT m) where
    sleep = lift . sleep

class Monad m => HasTimeout m where
    timeout :: TimeSpan -> m a -> m (Maybe a)

instance HasTimeout IO where
    timeout t action =
        bracketThreadActivity msg $
        Sys.timeout (asMicroseconds t) action
        where
          msecs = asMilliseconds t
          msg =
              "running action with timeout in " ++
              if msecs > 1000
                 then show (msecs `div` 1000) ++ "s"
                 else show msecs ++ "ms"

instance HasTimeout m => HasTimeout (StateT s m) where
    timeout t ma =
        do s <- get
           mx <- lift (timeout t (runStateT ma s))
           case mx of
             Nothing -> return Nothing
             Just (a, s) ->
                 do put s
                    return (Just a)

instance HasTimeout m => HasTimeout (ReaderT r m) where
    timeout t ma =
        do r <- ask
           lift (timeout t (runReaderT ma r))

instance (Error e, HasTimeout m) => HasTimeout (ErrorT e m) where
    timeout t ma =
        do mx <- lift (timeout t (runErrorT ma))
           case mx of
             Nothing -> return Nothing
             Just (Right a) -> return (Just a)
             Just (Left e) -> throwError e

sleepDyn :: HasSleep m => m TimeSpan -> m ()
sleepDyn t = sleepDynWithLogging t (\_ _ -> return ())

sleepUntil :: (HasTime m, HasSleep m) => XsdDateTime -> m ()
sleepUntil targetTime =
    do curTime <- getXsdDateTime
       let sleepTime = targetTime `diffXsdDateTime` curTime
       if isPositiveTimeSpan sleepTime
          then sleepTimeSpan sleepTime
          else return ()

sleepDynWithLogging :: HasSleep m => m TimeSpan
                    -> (TimeSpan -> Maybe TimeSpan -> m ()) -> m ()
sleepDynWithLogging = sleepDynWithGranularityAndLogging (seconds 5)

sleepDynWithGranularityAndLogging :: HasSleep m => TimeSpan -> m TimeSpan
                                  -> (TimeSpan -> Maybe TimeSpan -> m ()) -> m ()
sleepDynWithGranularityAndLogging wakeupGranularity getSleepTime logFun = loop (seconds 0) Nothing
    where
      loop timePassed prevSleepTime =
          do t <- getSleepTime
             let remaining = t - timePassed
             case () of
               _| remaining <= 0 -> return ()
                | remaining <= wakeupGranularity ->
                    do doLog prevSleepTime t remaining
                       sleepTimeSpan remaining
                | otherwise ->
                    do doLog prevSleepTime t remaining
                       sleepTimeSpan wakeupGranularity
                       loop (timePassed + wakeupGranularity) (Just t)
      doLog prevSleepTime curSleepTime remaining =
          case prevSleepTime of
            Nothing -> logFun curSleepTime Nothing
            Just t | t /= curSleepTime -> logFun curSleepTime (Just remaining)
                   | otherwise -> return ()
