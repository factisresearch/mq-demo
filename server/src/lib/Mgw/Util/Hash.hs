{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mgw.Util.Hash
    ( Hash(..), sha512, sha512Lazy, sha256, sha256Lazy, md5Lazy, md5, hashFile, htf_thisModulesTests
    , sha256s, concatHash, hashToHexText
    , SecHash(..), secHash
    )
where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.SafeCopy hiding (Version)
import Mgw.Util.BaseCoding
import Mgw.Util.QcPropHelper
import Mgw.Util.Preview

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Hashable (Hashable(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.MD5 as MD5
import Test.Framework

----------------------------------------
-- STDLIB
----------------------------------------
import Data.Char (isSpace)
import Data.Typeable
import GHC.Generics (Generic)
import qualified Text.PrettyPrint.HughesPJ as P

newtype Hash = Hash { unHash :: BS.ByteString } deriving (Eq, Ord, Typeable, Generic, SafeCopy)

class SecHash a where
    toSecHashRepr :: a -> BSL.ByteString

instance SecHash T.Text where
    toSecHashRepr x = BSL.fromChunks [T.encodeUtf8 x]

instance SecHash a => SecHash [a] where
    toSecHashRepr xs = BSL.concat (map toSecHashRepr xs)

instance SecHash Char where
    toSecHashRepr ch = BSL.fromStrict (T.encodeUtf8 (T.singleton ch))

instance SecHash BS.ByteString where
    toSecHashRepr = BSL.fromStrict

instance Show Hash where
    showsPrec _ (Hash bs) = showString (bsToBase16String bs)

instance Read Hash where
    readsPrec _prec s =
        let (hex, nohex) = span (`elem` "0123456789abcdefABCDEF") (dropWhile isSpace s)
        in case base16StringToBs hex of
             Left _err -> []
             Right bs -> [(Hash bs, nohex)]

instance Hashable Hash

instance Ppr Hash where
    ppr h = P.char '#' P.<> P.text (take 8 (show h))

secHash :: SecHash a => a -> Hash
secHash = sha256Lazy . toSecHashRepr

concatHash :: [Hash] -> Hash
concatHash = sha256 . BS.concat . map unHash

sha512 :: BS.ByteString -> Hash
sha512 = Hash . SHA256.hash
{-# INLINE sha512 #-}

sha512Lazy :: BSL.ByteString -> Hash
sha512Lazy = Hash . SHA256.hashlazy
{-# INLINE sha512Lazy #-}

sha256 :: BS.ByteString -> Hash
sha256 = Hash . SHA256.hash
{-# INLINE sha256 #-}

sha256Lazy :: BSL.ByteString -> Hash
sha256Lazy = Hash . SHA256.hashlazy
{-# INLINE sha256Lazy #-}

sha256s :: String -> Hash
sha256s = sha256 . T.encodeUtf8 . T.pack
{-# INLINE sha256s #-}

md5Lazy :: BSL.ByteString -> Hash
md5Lazy = Hash . MD5.hashlazy
{-# INLINE md5Lazy #-}

md5 :: BS.ByteString -> Hash
md5 = Hash . MD5.hash
{-# INLINE md5 #-}

hashToHexText :: Hash -> T.Text
hashToHexText = bsToBase16Text . unHash

hashFile :: (BSL.ByteString -> Hash) -> FilePath -> IO Hash
hashFile hashFun file =
    do bsl <- BSL.readFile file
       let h = hashFun bsl
       h `seq` return h

test_showHash =
    do assertEqual hashString (show (Hash (SHA512.hashlazy (BSLC.pack "hello\n"))))
    where
      hashString = ("e7c22b994c59d9cf2b48e549b1e24666636045930d3da7c1acb299d1c" ++
                    "3b7f931f94aae41edda2c2b207a36e10f8bcb8d45223e54878f5b316e7ce3b6bc019629")

prop_readShowHash = mkReadShowProp . Hash . BSC.pack
