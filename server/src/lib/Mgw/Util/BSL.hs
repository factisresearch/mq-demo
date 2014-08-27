{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.BSL
    ( module Data.ByteString.Lazy
    , hPutStr
    )
where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.ThreadActivity (bracketThreadActivity)

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Test.QuickCheck
import Data.ByteString.Lazy hiding ( hPutStr )
import qualified Data.ByteString.Lazy as BSL

hPutStr h s =
    bracketThreadActivity ("hPutStr (" ++ show (BSL.length s) ++ " bytes)") (BSL.hPutStr h s)

instance Arbitrary BSL.ByteString where
    arbitrary = do n :: Int <- choose (0,30)
                   words <- mapM (\_ -> arbitrary) [0..n]
                   return (BSL.pack words)
