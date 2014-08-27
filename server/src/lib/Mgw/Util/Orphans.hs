{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.Orphans where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Test.QuickCheck

----------------------------------------
-- STDLIB
----------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set


--
-- Arbitrary instances
--

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = arbitrary >>= return . Map.fromList
    shrink m = let l = Map.toList m
               in [Map.fromList l' | l' <- shrink l]

instance (Ord k, Arbitrary k) => Arbitrary (Set.Set k) where
    arbitrary = arbitrary >>= return . Set.fromList
    shrink m = let l = Set.toList m
               in [Set.fromList l' | l' <- shrink l]
