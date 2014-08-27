module Mgw.Util.Seq
    ( Seq, ViewL(..), ViewR(..)
    , headMay, lastMay, fromList, toList, concatMap
    , (|>), (<|)
    , singleton
    , Seq.empty, Seq.length, Seq.null, (Seq.><), Seq.take, Seq.drop, Seq.dropWhileL, Seq.dropWhileR
    , Seq.takeWhileR, Seq.takeWhileL, Seq.sortBy, Seq.viewr, Seq.viewl
    )
where

import Prelude hiding (concatMap)
import Control.Monad (liftM)
import Data.Hashable
import Data.Sequence (Seq, ViewL(..), ViewR(..), (><))
import Test.QuickCheck (Arbitrary(..))
import qualified Data.List
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

x |> xs = x `seq` x Seq.|> xs
xs <| x = x `seq` xs Seq.<| x

fromList :: [a] -> Seq a
fromList =  Data.List.foldl' (|>) Seq.empty

toList :: Seq a -> [a]
toList = F.toList

singleton :: a -> Seq a
singleton x = x `seq` Seq.singleton x

{-# INLINE headMay #-}
headMay :: Seq a -> Maybe a
headMay s =
    case Seq.viewl s of
      a :< _ -> Just a
      _ -> Nothing

lastMay :: Seq a -> Maybe a
lastMay s =
    case Seq.viewr s of
      _ :> a -> Just a
      _ -> Nothing

concatMap :: (a -> Seq b) -> Seq a -> Seq b
concatMap f = F.foldr ((><) . f) Seq.empty

instance Arbitrary a => Arbitrary (Seq a) where
    arbitrary = liftM Seq.fromList arbitrary

instance Hashable a => Hashable (Seq a) where
    hashWithSalt salt s =
        hashWithSalt salt (toList s)
