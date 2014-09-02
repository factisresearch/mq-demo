import qualified System.IO.Streams as Streams
import qualified Data.ByteString as BS
import System.IO

-- Already available
handleToOutputStream :: Handle -> IO (Streams.OutputStream BS.ByteString)
handleToOutputStream h = Streams.makeOutputStream write
  where
    write mx =
        case mx of
          Nothing -> hFlush h
          Just x -> BS.hPut h x
