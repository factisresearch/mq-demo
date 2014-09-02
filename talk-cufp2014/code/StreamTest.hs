import qualified System.IO.Streams as Streams
import Data.Maybe

copy :: Streams.InputStream c -> Streams.OutputStream c -> IO ()
copy input output = loop
    where
      loop =
          do mx <- Streams.read input
             Streams.write mx output
             if isJust mx then loop else return ()

copyFile :: FilePath -> FilePath -> IO ()
copyFile inputFile outputFile =
    Streams.withFileAsInput inputFile $ \input ->
    Streams.withFileAsOutput outputFile $ \output ->
        copy input output
