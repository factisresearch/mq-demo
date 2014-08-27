{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
module Mgw.Util.ConClasses
    ( Eq'(..), Eq''(..)
    , Show'(..), Show''(..)
    , Ord'(..), Ord''(..)
    , Read'(..)
    , Hashable'(..), HashableNoCtx(..)
    , Serialize'(..)
    , NFData'(..)
    , Arbitrary'(..)
) where

----------------------------------------
-- STDLIB
----------------------------------------
import GHC.Read (list)
import Text.ParserCombinators.ReadPrec (readPrec_to_S, readS_to_Prec)

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.DeepSeq (NFData, rnf)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize(..), Get, Putter)
import Test.QuickCheck (Gen, Arbitrary)

class Eq' t where
    eq' :: Eq a => t a -> t a -> Bool

class Eq' t => Ord' t where
    compare' :: Ord a => t a -> t a -> Ordering

class Eq'' t where
    eq'' :: Eq' r => t r -> t r -> Bool

class Eq'' t => Ord'' t where
    compare'' :: Ord' r => t r -> t r -> Ordering

class Show' t where
    showsPrec'  :: Show a => Int -> t a -> ShowS
    show'       :: Show a => t a -> String
    showList'   :: Show a => [t a] -> ShowS
    showsPrec' _ x s = show' x ++ s
    show' x          = showsPrec' 0 x ""
    showList' ls   s = showList__ (showsPrec' 0) ls s

class Show'' (t :: (* -> *) -> *) where
    showsPrec'' :: Show' r => Int -> t r -> ShowS
    show'' :: Show' r => t r -> String
    showList''  :: Show' r => [t r] -> ShowS
    showsPrec'' _ x s = show'' x ++ s
    show'' x          = showsPrec'' 0 x ""
    showList'' ls   s = showList__ (showsPrec'' 0) ls s

showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)

class Read' t where
    readsPrec' :: Read a => Int -> ReadS (t a)
    readList' :: Read a => ReadS [t a]
    readList' = readPrec_to_S (list (readS_to_Prec readsPrec')) 0

class Hashable' t where
    hashWithSalt' :: Hashable r => Int -> t r -> Int

class HashableNoCtx t where
    hashWithSaltNoCtx :: Int -> t r -> Int

class Serialize' t where
    put' :: Serialize a => Putter (t a)
    get' :: Serialize a => Get (t a)

class NFData' (k :: * -> *) where
    rnf' :: NFData a => k a -> ()

class Arbitrary' (k :: * -> *) where
    arbitrary' :: Arbitrary a => Gen (k a)

instance Eq' Maybe where
    eq' = (==)

instance Eq' [] where
    eq' = (==)

instance Ord' Maybe where
    compare' = compare

instance Show' Maybe where
    showsPrec' p m = showsPrec p m

instance Read' Maybe where
    readsPrec' = readsPrec

instance Serialize' Maybe where
    put' = put
    get' = get

instance NFData' Maybe where
    rnf' = rnf

instance Show' [] where
    showsPrec' p m = showsPrec p m

instance Read' [] where
    readsPrec' = readsPrec
