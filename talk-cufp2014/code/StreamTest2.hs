import qualified System.IO.Streams as Streams
import qualified Data.ByteString as BS
import System.IO

-- Already available
handleToInputStream :: Handle -> IO (Streams.InputStream BS.ByteString)
handleToInputStream h = Streams.makeInputStream produce
    where
      produce =
          do x <- BS.hGetSome h 32752
             return $! if BS.null x then Nothing else Just x
