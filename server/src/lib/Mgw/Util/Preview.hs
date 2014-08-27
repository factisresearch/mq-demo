module Mgw.Util.Preview
    (Preview(..), preview, previewNamedSet, previewNamedList, PreviewList(..), previewList
    ,previewRec ,previewRec', previewKv, showKv
    ,previewElems, previewsElems, pprMapping, previewList'
    ,Ppr(..), Ppr'(..), Doc, pretty, docFromStr, shortPreviewStr
    )
where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.List

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified Data.Text as T

----------------------------------------
-- STDLIB
----------------------------------------
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Data.List (intersperse)
import Data.Foldable (Foldable)
import Text.PrettyPrint.HughesPJ (Doc, (<>), (<+>))
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word64)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as F
import qualified Text.PrettyPrint.HughesPJ as P

class Preview a where
    previewsPrec :: Int -> a -> String -> String

instance Preview () where previewsPrec = showsPrec
instance Preview Char where previewsPrec = showsPrec
instance Preview Int where previewsPrec = showsPrec
instance Preview a => Preview [a] where previewsPrec = previewList
instance Preview T.Text where  previewsPrec = previewsText

instance (Preview a, Preview b) => Preview (Either a b) where
    previewsPrec prec eAB s =
        case eAB of
          Left a -> previewsPrec prec a s
          Right b -> previewsPrec prec b s

instance (Preview a) => Preview (Maybe a) where
    previewsPrec prec mA s =
        case mA of
          Nothing -> showString "Nothing" s
          Just a -> previewsPrec prec a s

instance (Preview a, Preview b) => Preview (a, b) where
    previewsPrec prec (a, b) s = previewsPrec prec a (", " ++ (previewsPrec prec b s))

instance (Preview a, Preview b, Preview c) => Preview (a, b, c) where
    previewsPrec prec (a, b, c) =
        showString "(" .
        previewsPrec prec a .
        showString ", " .
        previewsPrec prec b .
        showString ", " .
        previewsPrec prec c .
        showString ")"

instance Preview Word8 where
    previewsPrec = showsPrec

instance Preview Word64 where
    previewsPrec = showsPrec

instance Preview Int32 where
    previewsPrec = showsPrec

instance Preview Int64 where
    previewsPrec = showsPrec

class Ppr a where
    ppr :: a -> Doc
    pprMany :: (Foldable f, Ppr a) => f a -> Doc
    pprMany xs = P.brackets (P.sep $ P.punctuate P.comma $ fmap ppr (F.toList xs))

class Ppr' k where
    ppr' :: Ppr a => k a -> Doc

instance Ppr () where
    ppr () = P.text "()"

instance Ppr Int64 where
    ppr i = P.text (show i)

instance Ppr Char where
    ppr = P.char
    pprMany xs = P.char '"' <> F.foldl' (\d x -> d <> ppr x) P.empty xs <> P.char '"'

instance Ppr a => Ppr [a] where
    ppr = pprMany

instance Ppr a => Ppr (Set a) where
    ppr = pprMany

instance (Ppr a, Ppr b) => Ppr (Map a b) where
    ppr = pprMapping . Map.toList

pprMapping xs =
    P.braces (P.sep $ P.punctuate P.comma $ fmap pprTuple xs)
    where
      pprTuple (a, b) = P.sep [ppr a <+> P.text "->", P.nest 4 $ ppr b]

instance (Ppr a, Ppr b) => Ppr (a, b) where
    ppr (a, b) = P.parens (P.sep [ppr a <> P.semi, ppr b])

instance Ppr Int where
    ppr = docFromStr . show

instance Ppr Integer where
    ppr = docFromStr . show

instance Ppr Int32 where
    ppr = docFromStr . show

instance Ppr Word8 where
    ppr = docFromStr . show

instance Ppr Double where
    ppr = docFromStr . show

instance Ppr a => Ppr (Maybe a) where
    ppr (Just x) = ppr x
    ppr Nothing = P.text "Nothing"

instance Ppr Word64 where
    ppr = docFromStr . show

pretty :: Ppr a => a -> String
pretty = show . ppr

docFromStr :: String -> Doc
docFromStr = P.text

preview :: Preview a => a -> String
preview x = previewsPrec 5 x ""

previewRec :: Int -> String -> [(String, Int -> String -> String)] -> String -> String
previewRec prec tyName fields =
    previewRec' prec tyName (map mapField fields)
    where
      mapField (n,f) = showString n . showString "=" . f 11

previewRec' :: Int -> String -> [String -> String] -> String -> String
previewRec' prec tyName fields =
    showParen (prec > 10) $
    showString tyName .
    showString " { " .
    foldl (.) id (intersperse (showString ", ") fields) .
    showString " }"

previewKv :: Preview a => String -> a -> String -> String
previewKv name x =
    showString name . showString "=" . (previewsPrec 5 x)

showKv :: Show a => String -> a -> String -> String
showKv name x =
    showString name . showString "=" . (showsPrec 5 x)

previewNamedSet :: String -> t -> Set a -> String -> String
previewNamedSet name _prec set =
    showString "|" . showString name . showString "|=" . showString (show (Set.size set))

previewNamedList :: String -> t -> [a] -> String -> String
previewNamedList name _prec xs =
    showString "|" . showString name . showString "|=" . showString (show (length xs))

data PreviewList a
    = PreviewListFromFront Int [a]
    | PreviewListFromEnd Int [a]
    | PreviewListAll [a]

instance Preview a => Preview (PreviewList a) where
    previewsPrec prec pl =
        case pl of
          PreviewListFromFront n l -> previewList' Nothing prec (take n l)
          PreviewListFromEnd n l -> previewList' Nothing prec (lastElems n l)
          PreviewListAll l -> previewList' Nothing prec l

previewList' :: Preview a => Maybe Int -> Int -> [a] -> String -> String
previewList' maxElems _prec xs
    | maxElems == Nothing || length xs <= fromMaybe 0 maxElems =
        showString "[" .
        foldl (.) id (intersperse (showString ", ") (map (previewsPrec 11) xs)) .
        showString "]"
    | otherwise =
        showString "(" .
        showsPrec 5 (length xs) .
        showString " elems)"

previewList :: Preview a => Int -> [a] -> String -> String
previewList = previewList' (Just 3)

previewsElems :: (Foldable f, Preview a) => Int -> f a -> String -> String
previewsElems _prec xs =
    showString "[" .
    foldl (.) id (intersperse (showString ", ") (map (previewsPrec 5) (F.toList xs))) .
    showString "]"

previewElems :: (Foldable t, Preview a) => t a -> String
previewElems xs = previewsElems 5 xs ""

previewsText :: Int -> T.Text -> ShowS
previewsText _ t
    | T.length t < 65 = showString (T.unpack t)
    | otherwise = showString (T.unpack (T.take 65 t)) . showString "..."

shortPreviewStr :: Int -> String -> String
shortPreviewStr n s =
    if length s <= n
    then s
    else take n s ++ "..."
