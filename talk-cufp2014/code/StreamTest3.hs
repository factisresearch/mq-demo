import qualified System.IO.Streams as Streams

-- Already available
fromList :: [c] -> IO (Streams.InputStream c)
fromList l = Streams.fromGenerator (gen l)
    where
      gen = mapM_ Streams.yield
