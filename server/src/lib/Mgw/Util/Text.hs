{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mgw.Util.Text
    ( module Data.Text
    , putText
    , getText
    , showText
    , withoutTags
    , showTable
    , filename
    , splitOnNoEmpty
    , htf_thisModulesTests
    , sep, unsep
    )
where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Text
import Data.Serialize (Serialize(..))
import Test.QuickCheck
import Test.Framework

import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.PrettyPrint.HughesPJ as P
import qualified Data.Serialize as SER

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Preview

instance Ppr T.Text where
    ppr = P.text . T.unpack

instance Serialize T.Text where
    put = putText
    get = getText

putText :: T.Text -> SER.Put
putText t = put (TE.encodeUtf8 t)

getText :: SER.Get T.Text
getText = get >>= return . TE.decodeUtf8

instance Arbitrary T.Text where
    arbitrary = arbitrary >>= return . T.pack
    shrink t = fmap T.pack $ shrink (T.unpack t)

showText :: Show a => a -> T.Text
showText = T.pack . show

-- |Removes HTML Tags
withoutTags :: T.Text -> T.Text
withoutTags =
    let betweenTags ('<':xs) = inTag xs
        betweenTags (x:xs) = x:(betweenTags xs)
        betweenTags [] = []
        inTag ('>':xs) = betweenTags xs
        inTag ('\'':xs) = inSingQuot xs
        inTag ('"':xs) = inDoubleQuot xs
        inTag (_:xs) = inTag xs
        inTag [] = [] -- incorrect HTML
        inSingQuot ('\'':xs) = inTag xs
        inSingQuot (_:xs) = inSingQuot xs
        inSingQuot [] = [] -- incorrect HTML
        inDoubleQuot ('\"':xs) = inTag xs
        inDoubleQuot (_:xs) = inDoubleQuot xs
        inDoubleQuot [] = [] -- incorrect HTML
    in T.pack . betweenTags . T.unpack

test_withoutTags =
    do assertEqual "Hello laboratory report!"
                   (withoutTags $ "<p class=\"x\">Hello <pre>laboratory</pre> report!</p>")
       assertEqual "Hello laboratory report!"
                   (withoutTags $ "<p class=\"x>x<\">Hello <pre>laboratory</pre> report!</p>")
       assertEqual "Hello report!"
                   (withoutTags $ "<p class='s>x<'>Hello report!</p>")


showTable :: [[T.Text]] -> T.Text
showTable rows =
    let colWidths = F.foldl' maxList [] $ fmap (fmap T.length) rows
        maxList :: [Int] -> [Int] -> [Int]
        maxList [] ws = ws
        maxList vs [] = vs
        maxList (v:vs) (w:ws) = (max v w):(maxList ws vs)
        showRow :: [T.Text] -> T.Text
        showRow r = T.concat [" ",(T.intercalate " | ") $ fixWidths colWidths r,"\n"]
        fixWidths :: [Int] -> [T.Text] -> [T.Text]
        fixWidths (w:ws) (s:ss) = (T.append s (T.replicate (w - T.length s) " "))
                                  :(fixWidths ws ss)
        fixWidths _ _ = []
    in T.concat $ fmap showRow rows

test_showTable =
    let table = [["Col1","Col2","Col3"],["longfield","-",""],["short","longfield",""]]
    in do assertEqual (showTable table) $ T.concat
              [" Col1      | Col2      | Col3\n"
              ," longfield | -         |     \n"
              ," short     | longfield |     \n"]


sep :: T.Text -> Char -> T.Text -> T.Text
sep prefix ch suffix
    | T.any (==ch) prefix =
        safeError ("Oh dear!  Won't separate `" ++ T.unpack prefix ++ "' with `" ++ show ch
                   ++ "' because it contains that character!")
    | otherwise = T.concat [prefix, T.singleton ch, suffix]

unsep :: Char -> T.Text -> (T.Text, T.Text)
unsep ch full =
    case T.span (/=ch) full of
      (prefix, T.uncons -> Just (ch', suffix)) | ch == ch' -> (prefix, suffix)
      _ -> safeError ("Can't unsep `" ++ T.unpack full ++ "' using `" ++ show ch ++ "'.")

filename :: T.Text -> T.Text
filename = T.replace "?" "_" . T.replace "/" "_" . T.replace "." "_" . T.replace " " "_"

splitOnNoEmpty :: T.Text -> T.Text -> [T.Text]
splitOnNoEmpty break t =
    Prelude.filter (/= "") $ T.splitOn break t

test_filename =
    do assertEqual "Foo" (filename "Foo")
       assertEqual "Foo_Bar" (filename "Foo Bar")
       assertEqual "Foo_Bar" (filename "Foo/Bar")
       assertEqual "Foo_Bar" (filename "Foo.Bar")
       assertEqual "Foo_Bar" (filename "Foo?Bar")

test_sepUnsep =
    do assertEqual ("foo|bar") (sep "foo" '|' "bar")
       assertEqual ("foo", "bar") (unsep '|' "foo|bar")
       assertEqual ("foo", "bar|baz") (unsep '|' "foo|bar|baz")
