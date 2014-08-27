{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Mgw.Util.Fail
    ( Fail(..), isFail, isOk
    , FailT, runFailT, FIO
    , failEitherStr, failEitherShow, runErrorTFail, failInM, failInM'
    , failToEither, failMaybe, failToMaybe
    , failSwitch
    , MonadFail(..), MonadFailure(..)
    , failForIOException
) where

#include "src/macros.h"

import Mgw.Util.Logging.Core
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Exception (ErrorCall(..), IOException, catch)
import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Error (ErrorT, runErrorT, Error, MonadError(..))
import Control.Monad.IO.Class
import Control.Monad.Fix

data Fail a
    = Fail String
    | Ok a
      deriving (Show, Ord, Eq, Foldable, Traversable)

newtype FailT m a = FailT { unFailT :: m (Fail a) }

type FIO a = FailT IO a

instance LogMonad m => LogMonad (FailT m) where
    doLog file line level ei msg = FailT (doLog file line level ei msg >> return (Ok ()))

instance Monad m => MonadError String (FailT m) where
    throwError = throwFailT
    catchError = catchFailT

instance MonadTrans FailT where
    lift m =
        FailT $
        do a <- m
           return (Ok a)

instance MonadIO m => MonadIO (FailT m) where
    liftIO io = FailT (liftIO io >>= (return . Ok))

instance MonadState s m => MonadState s (FailT m) where
    get = lift get
    put = lift . put

throwFailT l = FailT $ return (Fail l)

m `catchFailT` h =
    FailT $
    do a <- runFailT m
       case a of
         Fail  l -> runFailT (h l)
         Ok r -> return (Ok r)

isFail :: Fail a -> Bool
isFail (Fail _) = True
isFail (Ok _) = False

isOk :: Fail a -> Bool
isOk = not . isFail

instance Monad Fail where
    return = Ok
    {-# INLINE return #-}
    fail   = Fail
    {-# INLINE fail #-}
    (>>=) = failBind
    {-# INLINE (>>=) #-}

instance MonadPlus Fail where
    mzero = failZero
    mplus = failPlus

instance Functor Fail where
    fmap = failMap

instance Applicative Fail where
    pure = Ok
    (<*>) = failAp

instance Alternative Fail where
    empty = failZero
    (<|>) = failPlus

instance MonadFix Fail where
    mfix f = let a = f (unOk a) in a
        where
          unOk (Ok x) = x
          unOk (Fail msg) = safeError ("mfix failed: " ++ (msg :: String))

instance Monad m => Monad (FailT m) where
    return = returnFailT
    fail = FailT . return . Fail
    (>>=) = bindFailT

instance Applicative m => Applicative (FailT m) where
    pure = pureFailT
    (<*>) = appFailT

instance Functor f => Functor (FailT f) where
    fmap f (FailT x) = FailT (fmap (fmap f) x)

failBind :: Fail a -> (a -> Fail b) -> Fail b
failBind ma f =
    case ma of
      Ok x -> {-# SCC "Fail/>>=/f" #-} (f x)
      -- is there a better way to avoid allocations?
      Fail x -> {-# SCC "Fail/>>=/Fail" #-} (Fail x)
{-# INLINE failBind #-}

failAp :: Fail (a -> b) -> Fail a -> Fail b
failAp (Ok f) (Ok a) = Ok (f a)
failAp (Fail msg) _ = Fail msg
failAp _ (Fail msg) = Fail msg
{-# INLINE failAp #-}

failMap :: (a -> b) -> Fail a -> Fail b
failMap f (Ok a) = Ok (f a)
failMap _ (Fail msg) = Fail msg
{-# INLINE failMap #-}

failZero :: Fail a
failZero = Fail "mzero"
{-# INLINE failZero #-}

failPlus :: Fail a -> Fail a -> Fail a
failPlus x@(Ok _) _ = x
failPlus _ x = x
{-# INLINE failPlus #-}

failSwitch :: (String -> c) -> (a -> c) -> Fail a -> c
failSwitch _ g (Ok x) = g x
failSwitch f _ (Fail x) = f x
{-# INLINE failSwitch #-}

{-# INLINE runFailT #-}
runFailT :: FailT m a -> m (Fail a)
runFailT = unFailT

{-# INLINE returnFailT #-}
returnFailT :: Monad m => a -> FailT m a
returnFailT = FailT . return . Ok

{-# INLINE bindFailT #-}
bindFailT :: Monad m => FailT m a -> (a -> FailT m b) -> FailT m b
bindFailT (FailT action) f =
    FailT $
    do mx <- action
       case mx of
         Ok x -> unFailT (f x)
         Fail m -> return (Fail m)

{-# INLINE pureFailT #-}
pureFailT :: Applicative m => a -> FailT m a
pureFailT = FailT . pure . Ok

{-# INLINE appFailT #-}
appFailT :: Applicative m => FailT m (a -> b) -> (FailT m a) -> FailT m b
appFailT (FailT f) (FailT x) =
    FailT (fmap failAp f <*> x)

instance MonadError String Fail where
    throwError             = Fail
    Fail  l `catchError` h = h l
    Ok r `catchError` _    = Ok r

failMaybe :: String -> Maybe a -> Fail a
failMaybe _ (Just x) = Ok x
failMaybe msg Nothing = Fail msg

failEitherStr :: Either String a -> Fail a
failEitherStr e =
    case e of
      Left err -> Fail err
      Right val -> Ok val

failEitherShow :: Show a => Either a b -> Fail b
failEitherShow e =
    case e of
      Left err -> Fail $ show err
      Right val -> Ok val

runErrorTFail :: Monad m => ErrorT String m a -> m (Fail a)
runErrorTFail err =
    do eith <- runErrorT err
       case eith of
         Left err -> return $ Fail err
         Right x -> return $ Ok x

class Monad m => MonadFailure m where
    throwFailure :: String -> m a
    throwFailure = fail
    catchFailure :: m a -> (String -> m a) -> m a

instance MonadFailure Maybe where
    Nothing `catchFailure` hdl = hdl "Failed in Maybe."
    ok `catchFailure` _ = ok

instance MonadFailure IO where
    catchFailure action hdl = action `catch` \(ErrorCall s) -> hdl s

instance MonadFailure Fail where
    ok@(Ok _) `catchFailure` _ =  ok
    Fail msg `catchFailure` hdl = hdl msg

instance Monad m => MonadFailure (FailT m) where
    FailT action `catchFailure` hdl =
        FailT $
        do result <- action
           case result of
             Fail msg -> unFailT (hdl msg)
             Ok _ -> return result

instance MonadFailure m => MonadFailure (ReaderT r m) where
    throwFailure = lift . throwFailure
    action `catchFailure` handler =
        ReaderT $ \r ->
        runReaderT action r `catchFailure` \msg -> runReaderT (handler msg) r

class Monad m => MonadFail m where
    mfail :: MonadFail m => String -> m a
    mfail = fail

failInM :: Monad m => Fail a -> m a
failInM f = failInM' f id

failInM' :: Monad m => Fail a -> (String -> String) -> m a
failInM' f h =
    case f of
      Ok x -> return x
      Fail msg -> fail (h msg)

-- only provide instances if mfail = fail
instance MonadFail Maybe where
instance MonadFail IO
instance MonadFail Fail
instance Monad m => MonadFail (FailT m)
instance (MonadFail m, Error e) => MonadFail (ErrorT e m)

failToEither :: Fail a -> Either String a
failToEither (Ok x) = Right x
failToEither (Fail x) = Left x

failToMaybe :: Fail a -> Maybe a
failToMaybe (Ok x) = Just x
failToMaybe _ = Nothing

failForIOException :: IO a -> IO (Fail a)
failForIOException action =
    catch (liftM Ok action) (\(exc::IOException) -> return (Fail (show exc)))
