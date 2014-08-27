module Logging (

    LogLevel(..), doLog, debug, debugIO, note, noteIO, info, infoIO
  , setLogLevel, isVerbose, isDebug

) where

import Development.Shake

import qualified Data.List as List

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Exit

data LogLevel = DEBUG | INFO | NOTE | WARN
              deriving (Show, Eq, Ord)

logLevelMVar :: MVar LogLevel
logLevelMVar = unsafePerformIO (newMVar NOTE)
{-# NOINLINE logLevelMVar #-}

setLogLevel :: LogLevel -> IO ()
setLogLevel ll =
    modifyMVar_ logLevelMVar $ \_ -> return ll

isVerbose :: IO Bool
isVerbose =
    do ll <- readMVar logLevelMVar
       return $ ll <= INFO

isDebug :: IO Bool
isDebug =
    do ll <- readMVar logLevelMVar
       return $ ll <= DEBUG

doLog :: LogLevel -> String -> IO ()
doLog ll msg =
    withMVar logLevelMVar $ \curLevel ->
        when (ll >= curLevel) (hPutStrLn stderr msg >> hFlush stderr)

debug :: String -> Action ()
debug = liftIO . debugIO

debugIO :: String -> IO ()
debugIO msg = doLog DEBUG ("[DEBUG] " ++ msg)

note :: String -> Action ()
note = liftIO . noteIO

noteIO :: String -> IO ()
noteIO msg = doLog NOTE msg

info :: String -> Action ()
info = liftIO . infoIO

infoIO :: String -> IO ()
infoIO msg = doLog INFO msg
