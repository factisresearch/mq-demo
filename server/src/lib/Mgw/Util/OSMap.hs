{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | An ordered, strict map.
module Mgw.Util.OSMap
    ( OSMap, Map, empty, lookup, insert, delete, fromList, toList, map, singleton, insertWith
    , member, elems, unionWith, difference, union, findWithDefault, size, null, isSubmapOf
    , intersection, foldrWithKey, foldlWithKey, keys, toDescList, updateLookupWithKey
    , deleteLookup, insertLookupWithKey, adjust, minViewWithKey, assocs, insertWith'
    , alter, differenceWith, updateWithKey, mapKeys, insertWithKey, insertWithKey'
    , keysSet, maxView, minView, intersectionWith, fromDistinctAscList
    , toDataMap, fromDataMap, hasKey, hasValue
    , htf_thisModulesTests
    )
where

----------------------------------------
-- STDLIB
----------------------------------------
import Prelude hiding (map, lookup, null)

import Control.Arrow (second)
import Control.Monad (liftM)
import Control.DeepSeq (NFData(..))

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Data.Maybe

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Foldable as F

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Serialize (Serialize(..))

import qualified Data.Map as DM
import Test.Framework
import Test.ChasingBottoms.IsBottom

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Preview

type Map= OSMap

newtype OSMap k v = OSMap { unOSMap :: DM.Map k v }
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, NFData, Serialize)

instance (Ord k, Eq k, Arbitrary k, Arbitrary v) => Arbitrary (OSMap k v) where
    arbitrary = liftM fromList arbitrary

instance (Ppr k, Ppr v) => Ppr (OSMap k v) where
    ppr = pprMapping . toList

{-# INLINE fromDataMap #-}
fromDataMap :: DM.Map k v -> OSMap k v
fromDataMap dm = DM.foldr (\a b -> a `seq` b) () dm `seq` OSMap dm

{-# INLINE toDataMap #-}
toDataMap :: OSMap k v -> DM.Map k v
toDataMap = unOSMap

{-# INLINE empty #-}
empty :: OSMap k v
empty = OSMap DM.empty

{-# INLINE member #-}
member :: (Ord k) => k -> OSMap k v -> Bool
member !k (OSMap m) =
    case DM.lookup k m of
      Just _ -> True
      Nothing -> False

{-# INLINE lookup #-}
lookup :: (Ord k) => k -> OSMap k v -> Maybe v
lookup !k (OSMap hm) = DM.lookup k hm

{-# INLINE insert #-}
insert :: (Ord k) => k -> v -> OSMap k v -> OSMap k v
insert !k !v (OSMap hm) = OSMap (DM.insert k v hm)

{-# INLINE delete #-}
delete :: (Ord k) => k -> OSMap k v -> OSMap k v
delete !k (OSMap hm) = OSMap (DM.delete k hm)

{-# INLINE fromList #-}
fromList :: (Ord k) => [(k,v)] -> OSMap k v
fromList = OSMap . List.foldl' (\m (k,v) -> v `seq` DM.insert k v m) DM.empty

{-# INLINE toList #-}
toList :: OSMap k v -> [(k, v)]
toList (OSMap hm) = DM.toList hm

{-# INLINE toDescList #-}
toDescList :: OSMap k v -> [(k, v)]
toDescList (OSMap hm) = DM.toDescList hm

{-# INLINE map #-}
map :: Ord k => (v -> v') -> OSMap k v -> OSMap k v'
map f (OSMap m) =
    case DM.mapAccum f' () m of  -- have to jump some hoops to get it strict
      ((),  m') -> OSMap m'
    where f' () x = let !x' = f x in ((), x')

{-# INLINE singleton #-}
singleton :: Ord k => k -> v -> OSMap k v
singleton !k !v = OSMap (DM.singleton k v)

{-# INLINE insertWith #-}
insertWith :: (Ord k) => (v -> v -> v) -> k -> v -> OSMap k v -> OSMap k v
insertWith f !k !v (OSMap hm) = OSMap (DM.insertWith' f k v hm)

{-# INLINE elems #-}
elems :: OSMap k v -> [v]
elems = DM.elems . unOSMap

{-# INLINE keys #-}
keys :: OSMap k v -> [k]
keys = DM.keys . unOSMap

{-# INLINE keysSet #-}
keysSet :: OSMap k v -> Set.Set k
keysSet = DM.keysSet . unOSMap

{-# INLINE union #-}
union :: Ord k => OSMap k v -> OSMap k v -> OSMap k v
union (OSMap m1) (OSMap m2) = OSMap (DM.union m1 m2)

{-# INLINE unionWith #-}
unionWith :: Ord k => (v -> v -> v) -> OSMap k v -> OSMap k v -> OSMap k v
unionWith f (OSMap m1) (OSMap m2) = OSMap (DM.unionWith f m1 m2)

{-# INLINE difference #-}
difference :: (Ord k) => OSMap k v -> OSMap k w -> OSMap k v
difference (OSMap m1) (OSMap m2) = OSMap (DM.difference m1 m2)

{-# INLINE intersection #-}
intersection :: (Ord k) => OSMap k v -> OSMap k w -> OSMap k v
intersection (OSMap m1) (OSMap m2) = OSMap (DM.intersection m1 m2)

{-# INLINE findWithDefault #-}
findWithDefault :: (Ord k) => a -> k -> OSMap k a -> a
findWithDefault def !k (OSMap m) =
    case DM.lookup k m of
      Just v -> v
      Nothing -> def

{-# INLINE size #-}
size :: OSMap k v -> Int
size = DM.size . unOSMap

{-# INLINE null #-}
null :: OSMap k v -> Bool
null = DM.null . unOSMap

{-# INLINE isSubmapOf #-}
isSubmapOf :: (Ord k, Eq a) => OSMap k a -> OSMap k a -> Bool
isSubmapOf a b = null (a `difference` b)

{-# INLINE foldrWithKey #-}
foldrWithKey :: (k -> v -> a -> a) -> a -> OSMap k v -> a
foldrWithKey f a (OSMap hm) = DM.foldrWithKey f a hm

{-# INLINE foldlWithKey #-}
foldlWithKey :: (a -> k -> v -> a) -> a -> OSMap k v -> a
foldlWithKey f a (OSMap hm) = DM.foldlWithKey f a hm

{-# INLINE updateLookupWithKey #-}
updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> OSMap k a -> (Maybe a, OSMap k a)
updateLookupWithKey fLazy k (OSMap curMap) =
    let (mv, newMap) = DM.updateLookupWithKey fStrict k curMap
    in (mv, OSMap newMap)
    where
      fStrict k v =
          case fLazy k v of
            Nothing -> Nothing
            Just !a -> Just a -- force the value!

{-# INLINE deleteLookup #-}
deleteLookup :: Ord k => k -> OSMap k v -> (Maybe v, OSMap k v)
deleteLookup = updateLookupWithKey (\_k _v -> Nothing)

{-# INLINE insertLookupWithKey #-}
insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> OSMap k a -> (Maybe a, OSMap k a)
insertLookupWithKey f !k !newV (OSMap curM) =
    let newM = DM.insertWithKey' f k newV curM
        mOldV = DM.lookup k curM
    in (mOldV, OSMap newM)

{-# INLINE adjust #-}
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f k = OSMap . DM.adjust f k . unOSMap

{-# INLINE minViewWithKey #-}
minViewWithKey :: Ord k => OSMap k a -> Maybe ((k, a), OSMap k a)
minViewWithKey = fmap (second OSMap) . DM.minViewWithKey . unOSMap

{-# INLINE assocs #-}
assocs :: OSMap k a -> [(k, a)]
assocs = DM.assocs . unOSMap

{-# INLINE insertWith' #-}
insertWith' :: Ord k => (a -> a -> a) -> k -> a -> OSMap k a -> OSMap k a
insertWith' f k !v (OSMap dm) = OSMap (DM.insertWith' f k v dm)

{-# INLINE alter #-}
alter :: Ord k => (Maybe a -> Maybe a) -> k -> OSMap k a -> OSMap k a
alter fLazy k (OSMap dm) = OSMap (DM.alter fStrict k dm)
    where
      fStrict ma =
          case fLazy ma of
            Just !x -> Just x   -- force the value!
            Nothing -> Nothing

{-# INLINE differenceWith #-}
differenceWith :: Ord k => (a -> b -> Maybe a) -> OSMap k a -> OSMap k b -> OSMap k a
differenceWith fLazy (OSMap dmA) (OSMap dmB) = OSMap (DM.differenceWith fStrict dmA dmB)
    where
      fStrict a b =
          case fLazy a b of
            Nothing -> Nothing
            Just !a -> Just a  -- force the value!

{-# INLINE updateWithKey #-}
updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> OSMap k a -> OSMap k a
updateWithKey fLazy k (OSMap dm) = OSMap (DM.updateWithKey fStrict k dm)
    where
      fStrict k v =
          case fLazy k v of
            Nothing -> Nothing
            Just !a -> Just a -- force the value!


{-# INLINE insertWithKey #-}
insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f k !v (OSMap dm) = OSMap (DM.insertWithKey' f k v dm)

{-# INLINE insertWithKey' #-}
insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey' = insertWithKey

{-# INLINE mapKeys #-}
mapKeys :: Ord k2 => (k1 -> k2) -> OSMap k1 a -> OSMap k2 a
mapKeys f (OSMap dm) = OSMap (DM.mapKeys f dm)

{-# INLINE maxView #-}
maxView :: OSMap k a -> Maybe (a, OSMap k a)
maxView (OSMap m) = fmap (second OSMap) (DM.maxView m)

{-# INLINE minView #-}
minView :: OSMap k a -> Maybe (a, OSMap k a)
minView (OSMap m) = fmap (second OSMap) (DM.minView m)

{-# INLINE intersectionWith #-}
intersectionWith :: Ord k => (a -> b -> c) -> OSMap k a -> OSMap k b -> OSMap k c
intersectionWith f (OSMap l) (OSMap r) =
    OSMap (forceValues (DM.intersectionWith f l r))

forceValues :: DM.Map k v -> DM.Map k v
forceValues m = F.foldl' (\() v -> v `seq` ()) () m `seq` m

{-# INLINE fromDistinctAscList #-}
fromDistinctAscList :: Ord k => [(k,v)] -> OSMap k v
fromDistinctAscList = OSMap . DM.fromDistinctAscList

--
-- Tests
--

newtype OSMapInt = OSMapInt (OSMap Int Int)
    deriving (Eq, Show)

bottomInt :: Int
bottomInt = bottom

hasValue :: Int -> OSMap Int Int -> Bool
hasValue v m = any (\(_, x) -> x == v) (toList m)

hasKey :: Int -> OSMap Int Int -> Bool
hasKey k m = isJust (lookup k m)

keyValueByIndex :: Int -> OSMap Int Int -> (Int, Int)
keyValueByIndex i m =
    let n = size m
    in toList m !! (i `mod` n)

valueByIndex :: Int -> OSMap Int Int -> Int
valueByIndex i m = snd (keyValueByIndex i m)

keyByIndex :: Int -> OSMap Int Int -> Int
keyByIndex i m = fst (keyValueByIndex i m)

instance Arbitrary OSMapInt where
    arbitrary =
        do l <- arbitrary
           return $ OSMapInt $ fromList l

prop_insertStrictKey :: OSMapInt -> Int -> Bool
prop_insertStrictKey (OSMapInt m) v =
    isBottom (insert bottom v m)

prop_insertStrictValue :: OSMapInt -> Int -> Bool
prop_insertStrictValue (OSMapInt m) k =
    isBottom (insert k bottom m)

prop_deleteStrict :: OSMapInt -> Bool
prop_deleteStrict (OSMapInt m) = isBottom (delete bottom m)

prop_mapStrict :: OSMapInt -> Int -> Property
prop_mapStrict (OSMapInt m) i =
    not (null m) ==>
    isBottom $ map (\x -> if x == value then bottom else x) m
    where
      value = valueByIndex i m

prop_singletonStrictKey :: Int -> Bool
prop_singletonStrictKey v =
    isBottom $ singleton bottomInt v

prop_singletonStrictValue :: Int -> Bool
prop_singletonStrictValue k =
    isBottom $ singleton k bottom

prop_insertWithStrictKey :: OSMapInt -> Int -> Bool
prop_insertWithStrictKey (OSMapInt m) v =
    isBottom $ insertWith (\_ _ -> 0) bottom v m

prop_insertWithStrictValue1 :: OSMapInt -> Int -> Bool
prop_insertWithStrictValue1 (OSMapInt m) k =
    isBottom $ insertWith (\_ _ -> 0) k bottom m

prop_insertWithStrictValue2 :: OSMapInt -> Int -> Int -> Property
prop_insertWithStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ insertWith (\_ old -> if old == value then bottom else v) key v m
    where
      (key, value) = keyValueByIndex i m

prop_unionStrictLeft :: OSMapInt -> Bool
prop_unionStrictLeft (OSMapInt m) =
    isBottom $ union bottom m

prop_unionStrictRight :: OSMapInt -> Bool
prop_unionStrictRight (OSMapInt m) =
    isBottom $ union m bottom

prop_differenceStrictLeft :: OSMapInt -> Bool
prop_differenceStrictLeft (OSMapInt m) =
    isBottom $ difference bottom m

prop_differenceStrictRight :: OSMapInt -> Property
prop_differenceStrictRight (OSMapInt m) =
    not (null m) ==>
    isBottom $ difference m bottom

prop_intersectionStrictLeft :: OSMapInt -> Bool
prop_intersectionStrictLeft (OSMapInt m) =
    isBottom $ intersection bottom m

prop_intersectionStrictRight :: OSMapInt -> Property
prop_intersectionStrictRight (OSMapInt m) =
    not (null m) ==>
    isBottom $ intersection m bottom

prop_insertLookupWithKeyStrictKey :: OSMapInt -> Int -> Bool
prop_insertLookupWithKeyStrictKey (OSMapInt m) v =
    isBottom $ snd $ insertLookupWithKey (\_ _ _ -> 0) bottom v m

prop_insertLookupWithKeyStrictValue1 :: OSMapInt -> Int -> Bool
prop_insertLookupWithKeyStrictValue1 (OSMapInt m) k =
    isBottom $ snd $ insertLookupWithKey (\_ _ _ -> 0) k bottom m

prop_insertLookupWithKeyStrictValue2 :: OSMapInt -> Int -> Int -> Property
prop_insertLookupWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ snd $ insertLookupWithKey (\_ _ old -> if old == value then bottom else v) key v m
    where
      (key, value) = keyValueByIndex i m

prop_updateLookupWithKeyStrictKey :: OSMapInt -> Maybe Int -> Bool
prop_updateLookupWithKeyStrictKey (OSMapInt m) v =
    isBottom $ snd $ updateLookupWithKey (\_ _ -> v) bottom m

prop_updateLookupWithKeyStrictValue1 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateLookupWithKeyStrictValue1 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ snd $ updateLookupWithKey (\_ old -> if old == value then bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_updateLookupWithKeyStrictValue2 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateLookupWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ snd $ updateLookupWithKey (\_ old -> if old == value then Just bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_deleteLookupStrict :: OSMapInt -> Property
prop_deleteLookupStrict (OSMapInt m) =
    not (null m) ==>
    isBottom $ snd $ deleteLookup bottom m

prop_alterStrictKey :: OSMapInt -> Bool
prop_alterStrictKey (OSMapInt m) =
    isBottom $ alter id bottom m

prop_alterStrictFun1 :: OSMapInt -> Int -> Property
prop_alterStrictFun1 (OSMapInt m) i =
    not (null m) ==>
    isBottom $ alter (\_ -> Just bottomInt) key m
    where
      key = keyByIndex i m

prop_alterStrictFun2 :: OSMapInt -> Int -> Property
prop_alterStrictFun2 (OSMapInt m) i =
    not (null m) ==>
    isBottom $ alter (\_ -> bottom) key m
    where
      key = keyByIndex i m

prop_differenceWithStrictLeft :: OSMapInt -> Maybe Int -> Bool
prop_differenceWithStrictLeft (OSMapInt m) v =
    isBottom $ differenceWith (\_ _ -> v) bottom m

prop_differenceWithStrictRight :: OSMapInt -> Maybe Int -> Property
prop_differenceWithStrictRight (OSMapInt m) v =
    not (null m) ==>
    isBottom $ differenceWith (\_ _ -> v) m bottom

prop_differenceWithStrictFun1 :: OSMapInt -> Int -> Int -> Property
prop_differenceWithStrictFun1 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ differenceWith (\_ _ -> Just bottom) m (insert key v m)
    where
      key = keyByIndex i m

prop_differenceWithStrictFun2 :: OSMapInt -> Int -> Int -> Property
prop_differenceWithStrictFun2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ differenceWith (\_ _ -> bottom) m (insert key v m)
    where
      key = keyByIndex i m

prop_intersectionWithStrictFun2 :: OSMapInt -> Int -> Int -> Property
prop_intersectionWithStrictFun2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ intersectionWith (\_ _ -> bottom) m (insert key v m)
    where
      key = keyByIndex i m

prop_updateWithKeyStrictKey :: OSMapInt -> Maybe Int -> Bool
prop_updateWithKeyStrictKey (OSMapInt m) v =
    isBottom $ updateWithKey (\_ _ -> v) bottom m

prop_updateWithKeyStrictValue1 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateWithKeyStrictValue1 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ updateWithKey (\_ old -> if old == value then bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_updateWithKeyStrictValue2 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ updateWithKey (\_ old -> if old == value then Just bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_insertWithKeyStrictKey :: OSMapInt -> Int -> Bool
prop_insertWithKeyStrictKey (OSMapInt m) v =
    isBottom $ insertWithKey (\_ _ _ -> 0) bottom v m

prop_insertWithKeyStrictValue1 :: OSMapInt -> Int -> Bool
prop_insertWithKeyStrictValue1 (OSMapInt m) k =
    isBottom $ insertWithKey (\_ _ _ -> 0) k bottom m

prop_insertWithKeyStrictValue2 :: OSMapInt -> Int -> Int -> Property
prop_insertWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ insertWithKey (\_ _ old -> if old == value then bottom else v) key v m
    where
      (key, value) = keyValueByIndex i m

prop_mapKeysStrict :: OSMapInt -> Int -> Property
prop_mapKeysStrict (OSMapInt m) i =
    not (null m) ==>
    isBottom $ mapKeys (\k -> if k == key then bottom else k) m
    where
      key = keyByIndex i m
