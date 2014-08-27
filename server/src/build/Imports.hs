module Imports (

    Import(..), ImportKind(..), hsImports, extForKind

) where

import Types

import Data.Char
import Data.Maybe
import qualified Data.List as List

data Import
    = HsImport ModuleName ImportKind
    | CppImport FilePath
      deriving (Show, Eq)

data ImportKind = RegularImport | SourceImport
      deriving (Show, Eq)

extForKind :: String -> ImportKind -> String
extForKind s k =
    case k of
      RegularImport -> s
      SourceImport -> s ++ "-boot"

hsImports :: String -> ([Import], Bool)
hsImports s = let l = lines s in (mapMaybe parseLine l, or $ map usesTemplateHaskell l)
    where
      parseLine ('i':'m':'p':'o':'r':'t':' ':rest) =
          let (comment1, afterComment1) = skipComment (dropWhile isSpace rest)
              afterQual = skipPkg (skipQual afterComment1)
              (comment2, afterComment2) = skipComment afterQual
          in case takeWhile isHsIdChar afterComment2 of
               [] -> Nothing
               mod ->
                   let kind = if isSourcePragma comment1 || isSourcePragma comment2
                                 then SourceImport
                                 else RegularImport
                   in Just (HsImport mod kind)
      parseLine ('#':'i':'n':'c':'l':'u':'d':'e':rest) =
          case dropWhile (/= '"') rest of
            '"':rest2 ->
                case takeWhile (/= '"') rest2 of
                  [] -> Nothing
                  fp -> Just (CppImport fp)
            _ -> Nothing
      parseLine _ = Nothing
      skipComment ('{':'-':rest) =
          let loop ('-':'}':rest) = ("", dropWhile isSpace rest)
              loop (x:xs) = let (text, more) = loop xs in (x:text, more)
          in loop rest
      skipComment rest = ("", dropWhile isSpace rest)
      isSourcePragma ('#':rest) =
          case dropWhile isSpace rest of
            'S':'O':'U':'R':'C':'E':rest2 -> dropWhile isSpace rest2 == "#"
            _ -> False
      isSourcePragma _ = False
      skipSource s = Just (RegularImport, s)
      skipQual ('q':'u':'a':'l':'i':'f':'i':'e':'d':rest) = dropWhile isSpace rest
      skipQual s = s
      skipPkg ('"':rest) = dropWhile isSpace $ drop 1 $ dropWhile (/='"') rest
      skipPkg s = s
      isHsIdChar c = isAlphaNum c || c == '\'' || c == '_' || c == '.'
      usesTemplateHaskell ('m':'o':'d':'u':'l':'e':_) = False
      usesTemplateHaskell s = "TemplateHaskell" `List.isInfixOf` s

test_hsImports =
    let given = hsImports src
    in if (expected, False) /= given
          then fail ("Expected: " ++ show expected ++ ", given: " ++ show given)
          else putStrLn ("test passed")
    where
      src = unlines ["module Foo where"
                    ,"#include \"src/macros.h\""
                    ,"import Data.Bar"
                    ,"-- import X"
                    ,"import qualified Control.Concurrent"
                    ,"import Data.Map (Map)"
                    ,"import qualified Data.Set as Set"
                    ,"import {-#SOURCE #-} Data.Src"
                    ,"import {-#SOURCE #-} qualified Data.SrcQ"
                    ,"import \"mypkg\" Data.SrcPkg"
                    ,"import Control.DeepSeq(NFData(..))"
                    ,"import {-@ HTF_TESTS @-}DocIdValue"
                    ,"import qualified {- Foo -}Control.Exception"
                    ,"importDataFoo :: Int"
                    ,"importDataFoo = 42"
                    ,""
                    ,"#include \"src/macros.h\""
                    ,""
                    ,"#include \"src/lib/Mgw/Model/CoreTypes.hsi\""
                    ,"#include \"src/lib/Mgw/Model/Accessors.hsi\""
                    ,""
                    ]
      expected = [CppImport "src/macros.h"
                 ,HsImport "Data.Bar" RegularImport
                 ,HsImport "Control.Concurrent" RegularImport
                 ,HsImport "Data.Map" RegularImport
                 ,HsImport "Data.Set" RegularImport
                 ,HsImport "Data.Src" SourceImport
                 ,HsImport "Data.SrcQ" SourceImport
                 ,HsImport "Data.SrcPkg" RegularImport
                 ,HsImport "Control.DeepSeq" RegularImport
                 ,HsImport "DocIdValue" RegularImport
                 ,HsImport "Control.Exception" RegularImport
                 ,CppImport "src/macros.h"
                 ,CppImport "src/lib/Mgw/Model/CoreTypes.hsi"
                 ,CppImport "src/lib/Mgw/Model/Accessors.hsi"
                 ]
