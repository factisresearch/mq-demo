{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Mgw.Util.List
    ( groupOn
    , groupOn'
    , groupUnsortedOn
    , groupUnsortedOn'
    , extractLast
    , lastElems
    , find
    , ungroupMay
    , makeMapping
    , monotone
    , sconcatBy
    , stripSuffix
    , htf_thisModulesTests
    )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Misc

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Hashable (Hashable)
import Test.Framework
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NL
import qualified Data.Semigroup as S

----------------------------------------
-- STDLIB
----------------------------------------
import Data.Ord
import Data.Function
import Control.Arrow (second)
import Data.Foldable (Foldable)
import qualified Data.List as L
import qualified Data.Foldable as F

-- O(n) requires a list sorted by the group key
groupOn :: Eq b => (a -> b) -> [a] -> [(b,[a])]
groupOn _  [] =  []
groupOn proj (x:xs) = (x', (x:ys)) : groupOn proj zs
    where
      x' = proj x
      (ys,zs) = span ((==x') . proj) xs

groupUnsortedOn :: Ord b => (a -> b) -> [a] -> [(b,[a])]
groupUnsortedOn proj = groupOn proj . L.sortBy (comparing proj)

-- O(n) requires a list sorted by the group key
groupOn' :: Eq b => (a -> (b,c)) -> [a] -> [(b,[c])]
groupOn' proj = map (second (map (snd . proj))) . groupOn (fst . proj)

groupUnsortedOn' :: Ord b => (a -> (b,c)) -> [a] -> [(b,[c])]
groupUnsortedOn' proj = groupOn' proj . L.sortBy (comparing (fst . proj))

sconcatBy :: (Ord b, Foldable f, S.Semigroup s) => (a -> b) -> (a -> s) -> f a -> [(b,s)]
sconcatBy p1 p2 =
    fmap proj
    . NL.groupBy ((==) `on` p1)
    . L.sortBy (comparing p1)
    . F.toList
    where
      proj gr = (p1 $ NL.head gr, S.sconcat $ NL.map p2 gr)

extractLast :: a -> [a] -> ([a], a)
extractLast x xs =
    case reverse xs of
      [] -> ([], x)
      y:ys -> (x : reverse ys, y)

lastElems :: Int -> [a] -> [a]
lastElems n =
    reverse . take n . reverse

find :: (Monad m, Foldable f) => (a -> Bool) -> f a -> m a
find f xs = maybeToFail "findM: no match" (F.find f xs)

ungroupMay :: [(a,[b])] -> Maybe [(a,b)]
ungroupMay [] = Just []
ungroupMay ((_,[]):_) = Nothing
ungroupMay ((a,bs):rest) =
    do r <- ungroupMay rest
       return (map ((,) a) bs ++ r)

-- Returns false if and only if there are elements in decreasing order in the list.
monotone :: (Ord a) => [a] -> Bool
monotone (x0:x1:xs)
    | x0 <= x1 = monotone (x1:xs)
    | otherwise = False
monotone _ = True

-- makeMapping takes a list of pairs and create a list of key-value pairs
-- such that each key appears only once in the result list. Moreover,
-- the result list contains the pairs in the same order as the input list.
-- Example: [(k1, v1), (k2, v2), (k1, v3)] --> [(k2, v2), (k1, v3)]
makeMapping :: (Eq a, Hashable a) => [(a, b)] -> [(a, b)]
makeMapping l =
    go (reverse l) HashSet.empty []
    where
      go [] _ acc = acc
      go (x@(k, _) : xs) done acc =
          if k `HashSet.member` done
          then go xs done acc
          else go xs (HashSet.insert k done) (x:acc)

test_ungroup =
    do assertEqual (Just [("a","1"),("a","2"),("a","3"),("b","4")])
                 $ ungroupMay [("a",["1","2","3"]),("b",["4"])]

test_ungroupGroup =
    do let list = [("x","1"),("y","1"),("y","3"),("y","1")]
       assertEqual (Just list) (ungroupMay $ groupOn' id list)

test_stripSuffix =
    do assertEqual (Just "foo") $ stripSuffix "bar" "foobar"
       assertEqual (Just "") $ stripSuffix "bar" "bar"
       assertEqual Nothing $ stripSuffix "bar" "foobars"

stripSuffix :: (Eq a) =>  [a] -> [a] -> Maybe [a]
stripSuffix s = (fmap reverse) . L.stripPrefix (reverse s) . reverse

test_monotone =
    do assertEqual True $ monotone [1,2,3]
       assertEqual False $ monotone [-1,0,3,2]
       assertEqual True $ monotone [1]
       assertEqual True $ monotone ([] :: [Int])

test_lastElems =
    do assertEqual ([]::[Int]) (lastElems 100 [])
       assertEqual [1,2,3] (lastElems 5 [1,2,3])
       assertEqual [1,2,3] (lastElems 3 [1,2,3])
       assertEqual [2,3] (lastElems 2 [1,2,3])


prop_lastElems :: [Int] -> Int -> Bool
prop_lastElems l n =
    lastElems n l `L.isSuffixOf` l

test_makeMapping :: IO ()
test_makeMapping =
    do assertEqual [] (makeMapping ([]::[(Int, String)]))
       let l = [(1::Int, "one"), (2, "two")] in assertEqual l (makeMapping l)
       assertEqual [(2::Int, "two"), (1, "three")]
                   (makeMapping [(1, "one"), (2, "two"), (1, "three")])
       assertEqual [(1::Int, "x")] (makeMapping [(1,"x"),(1,"x")])

prop_makeMappingConcat :: [(Int, String)] -> Bool
prop_makeMappingConcat l =
    makeMapping l == makeMapping (l ++ l)

prop_makeMappingKeysUnique :: [(Int, String)] -> Bool
prop_makeMappingKeysUnique l =
    length (map fst (makeMapping l)) == Set.size (Set.fromList (map fst l))

prop_makeMappingKeyValsOk :: [(Int, String)] -> Bool
prop_makeMappingKeyValsOk l =
    Map.fromList (makeMapping l) == Map.fromList l

prop_makeMappingOrderingOk :: [(Int, String)] -> Bool
prop_makeMappingOrderingOk l =
    checkOrder (makeMapping l) l
    where
      checkOrder [] [] = True
      checkOrder (x:xs) (y:ys)
                     | x == y = checkOrder xs (dropWhile (x==) ys)
                     | otherwise = checkOrder (x:xs) ys
      checkOrder _ _ = False

