{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mgw.Util.MD5
    ( MD5Digest(..), md5, md5', md5s, md5t
    , md5DigestToByteString, md5DigestToBase64Text, md5DigestToBase16Text
    , parseMD5DigestFromText
    ) where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Preview (Ppr(..))
import Mgw.Util.Encoding (encodeStringToBslUtf8)
import Mgw.Util.BaseCoding (bsToBase64Text, base64TextToBs, bsToBase16Text)

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.DeepSeq (NFData(..))
import Data.Serialize (Serialize(put, get))

import qualified Crypto.Hash.MD5 as ChMD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Serialize as SER
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.PrettyPrint.HughesPJ as P

import Test.QuickCheck (Arbitrary(..))

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (liftM)
import Data.Char (isSpace)
import Data.Hashable
import Data.Typeable (Typeable)

newtype MD5Digest = MD5Digest { unMD5Digest :: BS.ByteString }
    deriving (Eq, Ord, Hashable, Typeable)

md5 :: BS.ByteString -> MD5Digest
md5 = MD5Digest . ChMD5.hash

md5' :: BSL.ByteString -> MD5Digest
md5' = MD5Digest . ChMD5.hashlazy

md5s :: String -> MD5Digest
md5s = md5' . encodeStringToBslUtf8

md5t :: T.Text -> MD5Digest
md5t = md5 . T.encodeUtf8

md5DigestToByteString :: MD5Digest -> BS.ByteString
md5DigestToByteString = unMD5Digest

md5DigestToBase64Text :: MD5Digest -> T.Text
md5DigestToBase64Text = bsToBase64Text . unMD5Digest

md5DigestToBase16Text :: MD5Digest -> T.Text
md5DigestToBase16Text = bsToBase16Text . unMD5Digest

parseMD5DigestFromText :: Monad m => T.Text -> m MD5Digest
parseMD5DigestFromText t =
    case base64TextToBs t of
      Right bs -> return (MD5Digest bs)
      Left msg -> fail msg

instance NFData MD5Digest where
    rnf (MD5Digest bs)  = rnf bs

instance Show MD5Digest where
    showsPrec _ (MD5Digest s) = showString $ BSC.unpack (Base16.encode s)

instance Read MD5Digest where
    readsPrec _ s = if BS.null rest then [(MD5Digest digest, suf)] else []
        where (_pref,suf) = splitAt 32 (dropWhile isSpace s)
              (digest, rest) = Base16.decode (BSC.pack (take 32 s))

instance Ppr MD5Digest where
    ppr = P.text . show

instance Serialize MD5Digest where
    put = putMD5Digest
    get = getMD5Digest

putMD5Digest :: MD5Digest -> SER.Put
putMD5Digest (MD5Digest bs) = put bs

getMD5Digest :: SER.Get MD5Digest
getMD5Digest = liftM MD5Digest get

instance Arbitrary MD5Digest where
    arbitrary =
        do s <- arbitrary
           return $ md5s s
