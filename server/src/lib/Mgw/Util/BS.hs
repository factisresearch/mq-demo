{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.BS
    ( module Data.ByteString
    , hGetContents
    )
where

import Test.QuickCheck
import Mgw.Util.ThreadActivity (bracketThreadActivity)
import Data.ByteString hiding ( hGetContents )
import qualified Data.ByteString as BS

hGetContents = bracketThreadActivity "hGetContents" . BS.hGetContents

instance Arbitrary BS.ByteString where
    arbitrary = do n :: Int <- choose (0,30)
                   words <- mapM (\_ -> arbitrary) [0..n]
                   return (BS.pack words)
