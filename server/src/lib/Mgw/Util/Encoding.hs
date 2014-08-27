module Mgw.Util.Encoding (

    encodeStringToBslUtf8, decodeStringFromBslUtf8, decodeStringFromBsUtf8
  , encodeTextToBslIso88591, decodeTextFromBslIso88591
  , decodeTextFromBsIso88591

) where

import Mgw.Util.Fail
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char

encodeStringToBslUtf8 :: String -> BSL.ByteString
encodeStringToBslUtf8 = BSL.fromChunks . (:[]) . TE.encodeUtf8 . T.pack

decodeStringFromBslUtf8 :: BSL.ByteString -> Fail String
decodeStringFromBslUtf8 bsl = decodeStringFromBsUtf8 $ BSL.toStrict bsl

decodeStringFromBsUtf8 :: BS.ByteString -> Fail String
decodeStringFromBsUtf8 bs =
    case TE.decodeUtf8' bs of
      Right t -> Ok (T.unpack t)
      Left err -> Fail (show err)

decodeTextFromBslIso88591 :: BSL.ByteString -> T.Text
decodeTextFromBslIso88591 bsl =
    T.pack (BSLC.unpack bsl)

decodeTextFromBsIso88591 :: BS.ByteString -> T.Text
decodeTextFromBsIso88591 bsl =
    T.pack (BSC.unpack bsl)

encodeTextToBslIso88591 :: Monad m => TL.Text -> m BSL.ByteString
encodeTextToBslIso88591 t =
    let (safe, unsafe) = TL.span (\c -> ord c <= 0xFF) t
        bytes = BSLC.pack (TL.unpack safe)
    in if TL.null unsafe
          then return bytes
          else fail ("Could not convert text to ISO-8859-1")
