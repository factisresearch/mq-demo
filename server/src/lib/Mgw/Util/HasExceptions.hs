{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Mgw.Util.HasExceptions (HasExceptions(..)) where

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Error (ErrorT(ErrorT), Error, runErrorT)
import Control.Monad.Trans (lift)
import qualified Control.Exception as Exc
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted as LExc

class Monad m => HasExceptions m where
    throwException :: Exc.Exception e => e -> m a
    throwException = Exc.throw
    catchException :: Exc.Exception e => m a -> (e -> m a) -> m a
    finally :: m a -> m b -> m a

instance HasExceptions IO where
    catchException = Exc.catch
    finally = Exc.finally

-- This instance needs UndecidableInstances.  Senior Master of Type Hackery Dr. Wehr
-- permits use. (2013-05-18/dl)
instance (HasExceptions m, MonadBaseControl IO m) => HasExceptions (ResourceT m) where
    catchException = LExc.catch
    finally = LExc.finally

instance HasExceptions m => HasExceptions (ReaderT r m) where
    catchException m h =
        do r <- ask
           lift (catchException (runReaderT m r) (flip runReaderT r . h))
    finally m m' =
        do r <- ask
           lift (finally (runReaderT m r) (runReaderT m' r))

instance (Error e, HasExceptions m) => HasExceptions (ErrorT e m) where
    catchException m h = ErrorT $ runErrorT m `catchException` (runErrorT . h)
    finally m m' = ErrorT $ runErrorT m `finally` runErrorT m'
