{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.Conduit
    ( module Data.Conduit
    , find, trace, merge, takeWhile, dropWhile, conduitMapOptionM
    , finallyP
    , htf_thisModulesTests
    )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Concurrent
import Mgw.Util.Option
import Mgw.Util.VarState
import Mgw.Util.STM
import Mgw.Util.Tx
import Mgw.Util.Logging (LogMonad(..))
import qualified Mgw.Util.Logging

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Prelude hiding (dropWhile, takeWhile)
import Control.Monad
import Data.Conduit
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Test.Framework
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as CS
import qualified Data.Heap as H
import qualified Data.Traversable as T
import qualified Data.Foldable as F

DeriveLogMonad((LogMonad m), (ConduitM a b m), lift)

find :: (a -> Bool) -> Consumer a IO (Maybe a)
find pred = loop
    where
      loop =
          do mX <- await
             case mX of
               Just x
                   | pred x -> return (Just x)
                   | otherwise -> loop
               Nothing -> return Nothing

takeWhile :: (a -> Bool) -> Conduit a IO a
takeWhile pred = loop
    where
      loop =
          do mX <- await
             case mX of
               Just x
                   | pred x -> yield x >> loop
                   | otherwise -> return ()
               Nothing -> return ()

dropWhile :: (a -> Bool) -> Consumer a IO ()
dropWhile pred = loop
    where
      loop =
          do mX <- await
             case mX of
               Just x
                   | pred x -> loop
                   | otherwise -> leftover x
               Nothing -> return ()

conduitMapOptionM :: Monad m => (a -> m (Option b)) -> ConduitM a b m ()
conduitMapOptionM f = awaitForever $ option (return ()) yield <=< lift . f

trace print = CL.mapM (\x -> print x >> return x)

merge :: forall a t. (Foldable t, Traversable t, Ord a) => t (String, Source IO a) -> Producer IO a
merge sources =
    liftIO setup >>= loop
    where
      loop heap =
          do next <- liftIO (runTx (takeFromHeap heap))
             case next of
               Nothing -> return ()
               Just (H.Entry value refill) ->
                   do () <- liftIO refill
                      yield value
                      loop heap
      takeFromHeap var =
          do curHeap <- readVar var
             case H.uncons curHeap of
               Just (value, !newHeap) ->
                   do writeVar var newHeap
                      return (Just value)
               Nothing -> return Nothing
      setup =
          do chans <- T.mapM startScan sources
             heap <- runTx (newVar H.empty)
             F.mapM_ (putNext heap) chans
             return heap
      putNext heap chan =
          runTx $
          do mValue <- readTBMChan chan
             case mValue of
               Nothing -> return ()
               Just value ->
                   modifyVar heap (H.insert (H.Entry value (putNext heap chan)))
      startScan :: (String, Source IO a) -> IO (TBMChan a)
      startScan (name, source) =
          do let chanName = "chan-" ++ name
                 threadName = "merge-" ++ name
             chan <- runTx (newTBMChan chanName 2)
             fork_ WorkerThread threadName (source $$ CS.sinkTBMChan (realTBMChan chan))
             return chan

test_merge =
    do xs <-
           merge [("first", CL.sourceList [H.Entry 1 'a', H.Entry 2 'c'])
                 ,("second", CL.sourceList [H.Entry 0 'x', H.Entry 2 'b', H.Entry 3 'd'])
                 ] $$
           CL.consume
       assertEqual [H.Entry 0 'x', H.Entry 1 'a', H.Entry 2 'c', H.Entry 2 'b', H.Entry 3 'd'] xs

finallyP :: MonadResource m => ConduitM i o m r -> IO () -> ConduitM i o m r
finallyP action cleanup = bracketP (return ()) (\() -> cleanup) (\() -> action)
