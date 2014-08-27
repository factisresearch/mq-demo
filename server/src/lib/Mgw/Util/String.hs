{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mgw.Util.String
    ( fixed, fixed', strip, splitWs, replace, split
    , shorten
    , htf_thisModulesTests
    )
where

import Test.Framework

import Data.List (intersperse, isPrefixOf)
import Text.Regex (mkRegex, splitRegex)

fixed :: Int -> String -> String
fixed = fixed' '0'

fixed' :: Char -> Int -> String -> String
fixed' ch i s =
    let n = i - length s
    in replicate n ch ++ s

wschars :: String
wschars = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s =
    case s of
      [] -> []
      (x:xs)
          | elem x wschars -> lstrip xs
          | otherwise -> s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

splitWs :: String -> [String]
splitWs = filter (\x -> x /= []) . splitRegex (mkRegex "[ \t\n\r\v\f]+")

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l
    where
      join :: [a] -> [[a]] -> [a]
      join delim l = concat (intersperse delim l)


split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

shorten' :: String -> Int -> String -> String
shorten' cont maxLen s =
    let (prefix, suffix) = splitAt (maxLen - length cont) s
    in if length suffix <= length cont
          then s
          else prefix ++ cont

shorten :: Int -> String -> String
shorten = shorten' "[...]"

test_shorten =
    do assertEqual "Hallo" (shorten' "..." 5 "Hallo")
       assertEqual "H..." (shorten' "..." 4 "Hallo")
       assertEqual "Hallo" (shorten' "..." 8 "Hallo")
