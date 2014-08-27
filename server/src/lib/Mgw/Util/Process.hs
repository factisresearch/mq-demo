{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.Process
    ( readProcessWithExitCode'
    , readProcessWithExitCode, readShellCmdWithExitCode, createProcess, waitForProcess
    )
where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (when)

import System.Exit (ExitCode(..))
import System.Process (ProcessHandle,CreateProcess(..),StdStream(..),CmdSpec(..),shell,proc)

import qualified System.Process as P
import qualified Control.Exception as C

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified Data.ByteString.Char8 as BSC

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.ThreadActivity (bracketThreadActivity)
import Mgw.Util.Concurrent (ThreadType(..), fork)
import Mgw.Util.MVar
import Mgw.Util.IO (Handle, hFlush, hClose, hSetBinaryMode)
import Mgw.Util.Logging
import Mgw.Util.Exception (bracketOnError)

import Mgw.Util.TimeSpan
import Mgw.Util.Sleep

import qualified Mgw.Util.BS as BS
import qualified Mgw.Util.BSL as BSL

_DEBUG_ :: Bool
_DEBUG_ = False

readProcessWithExitCode' :: CreateProcess
                         -> (BS.ByteString -> IO ())
                         -> BSL.ByteString       -- ^ standard input
                         -> IO ( ExitCode
                               , BSL.ByteString
                               , BSL.ByteString) -- ^ (exitcode, stdout, stderr)
readProcessWithExitCode' p errorLogger input =
    C.catch (bracketOnError msg startProcess closeProcess withProcess)
    (\(exc::C.IOException) ->
         do logWarn (msg ++ " failed: " ++ show exc)
            C.throwIO exc)
    where
      msg = ("running process " ++ procname)
      startProcess =
          do (Just inh, Just outh, Just errh, pid) <-
                 createProcess $ p { std_in  = CreatePipe
                                   , std_out = CreatePipe
                                   , std_err = CreatePipe }
             return (inh, outh, errh, pid)
      closeProcess (_inh, _outh, _errh, pid) =
          do P.terminateProcess pid
             _ <- waitForProcess pid
             return ()
      withProcess (inh, outh, errh, pid) =
          do hSetBinaryMode inh True
             hSetBinaryMode outh True
             hSetBinaryMode errh True
             getOut <- startHandleReader "stdout" outh 120 (\_ -> return ())
             getErr <- startHandleReader "stderr" errh 1 errorLogger
             sleepTimeSpan (milliseconds 100)
             -- now write and flush any input
             when (not (BSL.null input)) $
                  do logDebug ("Writing " ++ show (BSL.length input) ++ " to stdin of process.")
                     dumpTrace "stdin" (Right input)
                     BSL.hPutStr inh input
                     logDebug ("Flushing stdin of process.")
                     hFlush inh
             logDebug ("Closing stdin of process.")
             hClose inh -- done with stdin
             logDebug ("Done closing stdin of process.")
             -- wait on the output
             out <- getOut
             logDebug ("Done waiting for out")
             err <- getErr
             logDebug ("Done waiting for err")
             ecode <- waitForProcess pid
             return (ecode, out, err)
      startHandleReader name h bufsize logger =
          do resMVar <- newEmptyNamedMVar ("readProcess result MVar for " ++ name)
             _ <- fork WorkerThread ("readProcess " ++ name ++ " reader thread") $
                  try ("reading " ++ name) $
                  if _DEBUG_
                     then let loopCheck prefix acc =
                                  do suf <- BS.hGetNonBlocking h bufsize
                                     let next = if BS.null suf then loopWait else loopCheck BS.empty
                                         bs = BS.append prefix suf
                                     logTrace (BSC.unpack bs)
                                     _ <- logger bs
                                     next (bs:acc)
                              loopWait acc =
                                  do bs <- BS.hGet h 1
                                     if BS.null bs
                                        then return (BSL.fromChunks (reverse acc))
                                        else loopCheck bs acc
                          in loopCheck BS.empty [] >>= putMVar resMVar
                     else BS.hGetContents h >>= putMVar resMVar . (BSL.fromChunks . (:[]))
             return (takeMVar resMVar)
      try :: String -> IO () -> IO ()
      try msg action =
          C.catch action $
               \(exc :: C.IOException) ->
                   do logWarn (msg ++ ": " ++ show exc)
                      return ()

      procname = showCmdSpec (P.cmdspec p)

readProcessWithExitCode :: FilePath             -- ^ command to run
                        -> [String]             -- ^ any arguments
                        -> BSL.ByteString       -- ^ standard input
                        -> IO ( ExitCode
                              , BSL.ByteString
                              , BSL.ByteString) -- ^ (exitcode, stdout, stderr)
readProcessWithExitCode cmd args =
    readProcessWithExitCode' (proc cmd args) (\_ -> return ())

readShellCmdWithExitCode :: String               -- ^ Shell command
                         -> BSL.ByteString       -- ^ standard input
                         -> IO ( ExitCode
                               , BSL.ByteString
                               , BSL.ByteString) -- ^ (exitcode, stdout, stderr)
readShellCmdWithExitCode cmd =
    readProcessWithExitCode' (shell cmd) (\_ -> return ())

waitForProcess p =
    bracketThreadActivity "waiting for process" (P.waitForProcess p)

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp@(CreateProcess { cmdspec = c }) =
    bracketThreadActivity ("creating process " ++ showCmdSpec c) (P.createProcess cp)

showCmdSpec :: CmdSpec -> String
showCmdSpec cs =
    case cs of
      ShellCommand s -> s
      RawCommand bin args -> bin ++ show args
