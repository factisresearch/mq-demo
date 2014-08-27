{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mgw.Util.StrictList
    ( StrictList(..)
    , (+!+)
    , catOptions
    , fromLazyList, toLazyList
    , filter
    , headOpt
    , lastOpt
    , reverse
    , sort
    , sortBy
    , findIndex
    , dropWhile
    , takeWhile
    , length, null
    , map, take
    , htf_thisModulesTests
    )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Option hiding (catOptions)
import Mgw.Util.SafeCopy

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Serialize.Put
import Data.Serialize.Get
import Test.Framework

----------------------------------------
-- STDLIB
----------------------------------------
import Prelude hiding ((!!),dropWhile, takeWhile, length, null, map, take, reverse, filter)
import qualified Prelude as P

import Control.Applicative
import Control.Monad hiding (forM_)
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Typeable

data StrictList a
    = !a :! !(StrictList a)
    | Nil
    deriving (Eq,Ord,Read,Show,Functor,Foldable,Traversable,Typeable)

infixr 5  +!+

(+!+) :: StrictList a -> StrictList a -> StrictList a
(+!+) Nil ys = ys
(+!+) (x :! xs) ys = x :! (xs +!+ ys)

instance Applicative StrictList where
    pure = return
    (<*>) = ap

instance Alternative StrictList where
    empty = Nil
    (<|>) = (+!+)

instance Monad StrictList where
    return = (:! Nil)
    (>>=) xs f = asum (fmap f xs)
    fail _ = Nil

instance Arbitrary a => Arbitrary (StrictList a) where
    arbitrary =
        do v <- arbitrary
           return (fromLazyList v)

instance Monoid (StrictList a) where
    mempty = Nil
    mappend = (+!+)

fromLazyList :: [a] -> StrictList a
fromLazyList [] = Nil
fromLazyList (x : xs) = x :! fromLazyList xs

toLazyList :: StrictList a -> [a]
toLazyList Nil = []
toLazyList (x :! xs) = x : toLazyList xs

length :: StrictList a -> Int
length xxs =
    case xxs of
      _ :! xs -> 1 + length xs
      Nil -> 0

null :: StrictList a -> Bool
null Nil = True
null _ = False

headOpt :: StrictList a -> Option a
headOpt Nil = None
headOpt (x :! _) = Some x

lastOpt :: StrictList a -> Option a
lastOpt Nil = None
lastOpt (x :! xs) = option (Some x) Some $ lastOpt xs

takeWhile :: (a -> Bool) -> StrictList a -> StrictList a
takeWhile _ Nil = Nil
takeWhile p (x :! xs)
    | p x = x:!(takeWhile p xs)
    | otherwise = Nil

dropWhile :: (a -> Bool) -> StrictList a -> StrictList a
dropWhile _ Nil = Nil
dropWhile p (x :! xs)
    | p x = dropWhile p xs
    | otherwise = x :! xs

findIndex :: (a -> Bool) -> StrictList a -> Option Int
findIndex _ Nil = None
findIndex p (x :! xs)
    | p x = Some 0
    | otherwise = fmap (+1) $ findIndex p xs

map :: (a -> b) -> StrictList a -> StrictList b
map = fmap

filter :: (a -> Bool) -> StrictList a -> StrictList a
filter _ Nil = Nil
filter pred (x :! xs)
    | pred x = x :! (filter pred xs)
    | otherwise = filter pred xs

catOptions :: StrictList (Option a) -> StrictList a
catOptions xs =
    case xs of
      Nil -> Nil
      (None :! xs) -> catOptions xs
      (Some x :! xs) -> x :! (catOptions xs)

take :: Int -> StrictList a -> StrictList a
take _ Nil = Nil
take n _ | n <= 0 = Nil
take n (x :! xs) =
    (x :! take (n-1) xs)

sort :: (Ord a) => StrictList a -> StrictList a
sort = sortBy compare

reverse :: StrictList a -> StrictList a
reverse l =  rev l Nil
  where
    rev Nil a = a
    rev (x :! xs) a = rev xs (x :! a)

sortBy :: (a -> a -> Ordering) -> StrictList a -> StrictList a
sortBy cmp = mergeAll . sequences
  where
    sequences (a :! (b :! xs))
      | a `cmp` b == GT = descending b (a :! Nil) xs
      | otherwise       = ascending  b (a :!) xs
    sequences xs = xs :! Nil
    descending a as (b :! bs)
      | a `cmp` b == GT = descending b (a :! as) bs
    descending a as bs  = (a :! as) :! sequences bs
    ascending a as (b:!bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a :! ys)) bs
    ascending a as bs   = as (a :! Nil) :! sequences bs
    mergeAll (x :! Nil) = x
    mergeAll xs  = mergeAll (mergePairs xs)
    mergePairs (a :! (b :! xs)) = (merge a b) :! mergePairs xs
    mergePairs xs       = xs
    merge as@(a :! as') bs@(b :! bs')
      | a `cmp` b == GT = b :! merge as  bs'
      | otherwise       = a :! merge as' bs
    merge Nil bs         = bs
    merge as Nil         = as

prop_take l lst =
    let me :: [Int]
        me = toLazyList $ take l (fromLazyList lst)
    in P.take l lst == me

test_headOpt =
    do assertEqual (Some "B") $ headOpt $ fromLazyList ["B","C"]
       assertEqual None $ headOpt (Nil :: StrictList ())

test_lastOpt =
    do assertEqual (Some 5) $ lastOpt $ fromLazyList [2,4,5]
       assertEqual None $ lastOpt $ (Nil :: StrictList ())

test_findIndex =
    do assertEqual None $ findIndex (== "A") $ fromLazyList ["B","C"]
       assertEqual None $ findIndex (== "A") Nil
       assertEqual (Some 1) $ findIndex (== "C") $ fromLazyList ["B","C","D"]
       assertEqual (Some 0) $ findIndex (/= "C") $ fromLazyList ["B","C","D"]

test_reverse =
    do assertEqual (fromLazyList ["D","C","B"]) (reverse $ fromLazyList ["B","C","D"])
       assertEqual (Nil :: StrictList ()) $ reverse Nil

test_dropWhile =
    do assertEqual (fromLazyList [5]) $ dropWhile even $ fromLazyList [2,4,5]
       assertEqual (fromLazyList [2,4,5]) $ dropWhile odd $ fromLazyList [2,4,5]
       assertEqual Nil $ dropWhile (>=1) $ fromLazyList [2,4,5]

test_safecopy =
    do let list = fromLazyList ['a','b','c']
           list' = Cons1 'a' $ Cons1 'b' $ Cons1 'c' Nil1
           roundtrip :: (SafeCopy a, SafeCopy b) => a -> Either String b
           roundtrip = runGet safeGet . runPut . safePut
       assertEqual (Right list) $ roundtrip list
       assertEqual (Right list) $ roundtrip list'

instance SafeCopy a => SafeCopy (StrictList a) where
    kind = extension
    version = 2
    putCopy list = contain $
        do putElem <- getSafePut
           safePut $ length list
           forM_ list putElem
    getCopy = contain $
        do getElem <- getSafeGet
           let getList len prefix
                 | len < 0 = safeFail $ "Parsed negative length for StrictList: "
                                        ++ show len
                 | len == 0 = return $ prefix Nil
                 | otherwise =
                     do x <- getElem
                        getList (len-1) $ prefix . (x :!)
           len <- safeGet
           getList (len :: Int) id

instance SafeCopy a => Migrate (StrictList a) where
    type MigrateFrom (StrictList a) = (StrictListV1 a)
    migrate (Cons1 x y) = x :! (migrate y)
    migrate Nil1 = Nil

data StrictListV1 a
    = Cons1 !a !(StrictListV1 a)
    | Nil1
    deriving (Typeable)

deriveSafeCopy 1 'base ''StrictListV1
