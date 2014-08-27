module Mgw.Util.BaseCoding
    ( bsToBase64String, bsToBase64Text, base64StringToBs, base64TextToBs
    , bsToBase64Bs, base64BsToBs
    , readBase16String, bsToBase16String, base16StringToBs, base16TextToBs
    , bsToBase16Text, bsToBase16Bs, base16BsToBs
    )
where

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

bsToBase64String :: BS.ByteString -> String
bsToBase64String = BSC.unpack . Base64.encode

bsToBase64Text :: BS.ByteString -> T.Text
bsToBase64Text = T.decodeUtf8 . Base64.encode

base64StringToBs :: String -> Either String BS.ByteString
base64StringToBs = Base64.decode . BSC.pack

base64TextToBs :: T.Text -> Either String BS.ByteString
base64TextToBs = Base64.decode . T.encodeUtf8

bsToBase64Bs :: BS.ByteString -> BS.ByteString
bsToBase64Bs = Base64.encode

base64BsToBs :: BS.ByteString -> Either String BS.ByteString
base64BsToBs = Base64.decode

readBase16String :: String -> (BS.ByteString, String)
readBase16String s =
    let (valid, invalid) = Base16.decode (BSC.pack s)
    in (valid, BSC.unpack invalid)

base16StringToBs :: String -> Either String BS.ByteString
base16StringToBs s =
    case readBase16String s of
      (x, "") -> Right x
      (_, err) -> Left ("Found invalid base16 data: " ++ take 16 err)

base16TextToBs :: T.Text -> Either String BS.ByteString
base16TextToBs = base16StringToBs . T.unpack

base16BsToBs :: BS.ByteString -> Either String BS.ByteString
base16BsToBs bs =
    let (result, rest) = Base16.decode bs
    in if BS.null rest
       then Right result
       else Left ("Failed to decode " ++ show bs ++ ".  Rest: " ++ show rest)

bsToBase16String :: BS.ByteString -> String
bsToBase16String = BSC.unpack . Base16.encode

bsToBase16Bs :: BS.ByteString -> BS.ByteString
bsToBase16Bs = Base16.encode

bsToBase16Text :: BS.ByteString -> T.Text
bsToBase16Text = T.decodeUtf8 . bsToBase16Bs
