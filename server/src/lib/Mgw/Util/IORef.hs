{-# LANGUAGE BangPatterns #-}
module Mgw.Util.IORef
    ( R.IORef, R.newIORef, R.readIORef, R.writeIORef, atomicModifyIORef, atomicWriteIORef
    , atomicUpdateIORef
    , runStateIORef'
    )
where

import qualified Data.IORef as R
import qualified Control.Monad.State.Strict as StrictState

atomicWriteIORef :: R.IORef a -> a -> IO ()
atomicWriteIORef = R.writeIORef

atomicUpdateIORef :: R.IORef a -> (a -> a) -> IO ()
atomicUpdateIORef r f =
    do x' <- R.atomicModifyIORef r (\x -> let x' = f x in (x', x'))
       x' `seq` return ()

atomicModifyIORef :: R.IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef r f =
    do result <- R.atomicModifyIORef r f
       content <- R.readIORef r
       content `seq` return result

runStateIORef' :: R.IORef s -> StrictState.State s a -> IO a
runStateIORef' ref st =
    do old <- R.readIORef ref
       let (!res, !new) = StrictState.runState st old
       R.writeIORef ref new
       return res
