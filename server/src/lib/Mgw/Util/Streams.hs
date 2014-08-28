{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.Streams (

    module System.IO.Streams
  , concatMapM, forM_

) where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import System.IO.Streams
import qualified System.IO.Streams as Streams

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad.IO.Class

concatMapM :: forall a b . (a -> IO [b]) -> InputStream a -> IO (InputStream b)
concatMapM f s =
    fromGenerator g
    where
      g :: Generator b ()
      g =
          do mx <- liftIO $ Streams.read s
             case mx of
               Nothing -> return ()
               Just x ->
                   do l <- liftIO $ f x
                      Prelude.mapM_ yield l
                      g

forM_ :: InputStream a -> (a -> IO ()) -> IO ()
forM_ s f =
    let loop =
            do mx <- Streams.read s
               case mx of
                 Nothing -> return ()
                 Just x -> f x >> loop
    in loop
