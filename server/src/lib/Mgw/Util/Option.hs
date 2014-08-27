{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mgw.Util.Option
    ( Option(..), OptionT(..), ToOptionT(..)
    , fromOption, isSome, isNone, maybeToOption, optionToMaybe, option, catOptions
    , optionToList, listToOption, mapOption, noneIf, emptyIfNone
    , mapOptionM, forOptionM
    , failToOption
    , htf_thisModulesTests
    )
where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.SafeCopy
import Mgw.Util.Fail
import Mgw.Util.ConClasses
import Mgw.Util.Preview

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Test.Framework
import qualified Data.Text as T
import qualified Text.PrettyPrint.HughesPJ as P

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Data
import Data.Foldable (Foldable)
import Data.List as L
import Data.Traversable (Traversable)
import Data.Hashable (Hashable)
import Data.Serialize
import GHC.Generics (Generic)

data Option a
    = Some !a
    | None
    deriving (Eq,Read,Show,Foldable,Traversable,Typeable,Data,Functor,Generic)

newtype OptionT m a
    = OptionT
    { runOptionT :: m (Option a)
    }

class ToOptionT t where
    optionT :: Monad m => m (t a) -> OptionT m a

instance ToOptionT Maybe where
    optionT = OptionT . liftM maybeToOption

instance ToOptionT Option where
    optionT = OptionT

instance ToOptionT Fail where
    optionT a = lift a >>= failInM

instance Functor m => Functor (OptionT m) where
  fmap f = OptionT . fmap (fmap f) . runOptionT

instance Monad m => Monad (OptionT m) where
  fail _ = OptionT (return None)
  return = lift . return
  x >>= f = OptionT (runOptionT x >>= option (return None) (runOptionT . f))

instance Applicative m => Applicative (OptionT m) where
    pure = OptionT . pure . Some
    OptionT f <*> OptionT x = OptionT (fmap (<*>) f <*> x)

instance Ord a => Ord (Option a) where
    compare x y =
        case x of
          Some a ->
              case y of
                Some b -> compare a b
                None -> GT
          None ->
              case y of
                None -> EQ
                Some _ -> LT

instance NFData a => NFData (Option a) where
    rnf None = ()
    rnf (Some b) = rnf b

instance MonadTrans OptionT where
    lift x = OptionT (liftM Some x)

instance (MonadIO m) => MonadIO (OptionT m) where
    liftIO = lift . liftIO

instance Applicative Option where
    pure = Some
    Some f <*> Some a = Some (f a)
    _ <*> _ = None
    Some _ *> x  = x
    _ *> _ = None
    l@(Some _) <* Some _ = l
    _ <* _ = None

instance Alternative Option where
    empty = None
    l@(Some _) <|> _ = l
    _ <|> r = r

instance MonadFail Option where
    mfail _ = None

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary = frequency [(1, return None), (3, liftM Some arbitrary)]

  shrink (Some x) = None : [ Some x' | x' <- shrink x ]
  shrink _        = []

instance (ToJSON a) => ToJSON (Option a) where
    toJSON (Some a) = toJSON a
    toJSON None  = Null
    {-# INLINE toJSON #-}

instance (FromJSON a) => FromJSON (Option a) where
    parseJSON Null   = pure None
    parseJSON a      = Some <$> parseJSON a
    {-# INLINE parseJSON #-}

noneIf :: (a -> Bool) -> a -> Option a
noneIf p x
    | p x = None
    | otherwise = Some x

fromOption :: a -> Option a -> a
fromOption def opt =
    case opt of
      Some x -> x
      None -> def

isSome :: Option a -> Bool
isSome (Some _) = True
isSome _ = False

isNone :: Option a -> Bool
isNone None = True
isNone _ = False

optionToMaybe :: Option a -> Maybe a
optionToMaybe (Some a) = Just a
optionToMaybe None = Nothing

maybeToOption :: Maybe a -> Option a
maybeToOption (Just a) = Some a
maybeToOption Nothing = None

optionToList :: Option a -> [a]
optionToList (Some a) = [a]
optionToList None = []

listToOption :: [a] -> Option a
listToOption [] = None
listToOption (x:_) = Some x

option :: b -> (a -> b) -> Option a -> b
option def f opt =
    case opt of
      Some a -> f $! a
      None -> def

catOptions :: [Option a] -> [a]
catOptions ls = [x | Some x <- ls]

mapOption :: (a -> Option b) -> [a] -> [b]
mapOption _ [] = []
mapOption f (x:xs) =
    let rs = mapOption f xs in
    case f x of
      None -> rs
      Some r  -> r : rs

emptyIfNone :: Option T.Text -> T.Text
emptyIfNone None = T.empty
emptyIfNone (Some t) = t

instance Hashable a => Hashable (Option a)

instance Monad Option where
    return = Some
    m >>= f =
        case m of
          Some x -> f x
          None -> None
    fail _ = None

instance MonadPlus Option where
    mzero = None
    mplus x@(Some _) _ = x
    mplus _ y = y

instance Eq' Option where
    eq' = (==)

instance Ord' Option where
    compare' = compare

instance Show' Option where
    showsPrec' p m = showsPrec p m

instance Read' Option where
    readsPrec' = readsPrec

instance NFData' Option where
    rnf' = rnf

-- don't use template haskell here because it breaks ghci
instance SafeCopy a_acrS => SafeCopy (Option a_acrS) where
      putCopy (Some v) =
          contain $
          do putWord8 0
             safePut v
      putCopy None = contain (putWord8 1)
      getCopy =
          contain $
          do tag <- getWord8;
             case tag of
               0 -> liftM Some safeGet
               1 -> return None
               _ -> fail ("Invalid tag for Mgw.Util.Option: " ++ show tag)
      version = 1
      kind = base

instance Preview a => Preview (Option a) where
    previewsPrec prec mA s =
        case mA of
          None -> showsPrec prec "None" s
          Some a -> previewsPrec prec a s

instance Ppr a => Ppr (Option a) where
    ppr None = P.text "None"
    ppr (Some a) = ppr a

forOptionM :: Monad m => [a] -> (a -> OptionT m b) -> m [b]
forOptionM xs f = liftM catOptions (forM xs (runOptionT . f))

mapOptionM :: Monad m => (a -> OptionT m b) -> [a] -> m [b]
mapOptionM = flip forOptionM

failToOption :: Fail a -> Option a
failToOption (Ok x) = Some x
failToOption _ = None

test_ord =
    let list = [None, None, Some "x", Some "x", Some "y"]
    in forM_ (permutations list) $ \perm ->
           assertEqual list (sort perm)
