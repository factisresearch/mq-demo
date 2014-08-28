{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Mgw.Util.Network (

  NetworkApp(..), ServerSettings, serverSettingsTCP, runTCPServer
 ,ClientSettings, clientSettingsTCP, runTCPClient,

  Net.PortID(..), Net.PortNumber,

  NetSock.Socket, SafeSocket,

  StunnelServerConfig(..), mkStunnelServerConfig, startStunnel, listenOnWithSsl,
  StunnelClientConfig(..), mkStunnelClientConfig, connectWithSsl,

  Net.withSocketsDo, Net.listenOn, NetSock.socket,
  NetBsd.getHostName, NetBsd.getHostByName, NetBsd.hostAddress,

  accept, connect, recv, recvWithTimeout, send, send', rawClose, sClose, rawSocket,
  checkPort

) where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (unless, when)
import Control.Concurrent.MSem (MSem)

import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))
import System.IO.Error (mkIOError, eofErrorType)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.IO.Streams as Streams
import System.Process (proc, std_in, std_out, std_err,
                       StdStream(CreatePipe, UseHandle), terminateProcess)
import System.Exit (ExitCode(..))
import System.Time
import System.Directory (createDirectoryIfMissing)

import qualified Data.List as List
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Int
import qualified Data.Foldable as F
import qualified Control.Concurrent.MSem as MSem

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified Network as Net
import qualified Network.Socket as NetSock
import qualified Network.BSD as NetBsd
-- import qualified Network.Socket.ByteString as NetBS
import qualified Network.Socket.ByteString.Lazy as NetBSL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import System.FilePath

import Text.Regex.Posix
import Safe (readMay)
import Data.Streaming.Network hiding (runTCPServer, runTCPClient)

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Exception (SomeException, throw, catch, throwIO, bracket, bracketOnError)
import Mgw.Util.IORef (newIORef, writeIORef, readIORef)
import Mgw.Util.Concurrent
    ( KillableThreadIdIO, ThreadType(..), KillException(..)
    , forkKillableThreadIO, killThreadIO
    , fork_
    )
import Mgw.Util.ThreadActivity (bracketThreadActivity)
import Mgw.Util.STM
import Mgw.Util.Tx
import Mgw.Util.Process (createProcess, waitForProcess, readShellCmdWithExitCode)
import Mgw.Util.TimeSpan hiding (diffClockTimes)
import Mgw.Util.Logging
import Mgw.Util.Logging.LogWriter (getLoggingRootDir)
import Mgw.Util.ExitAction
import Mgw.Util.Temp
import Mgw.Util.Sleep
import Mgw.Util.String (strip)

data NetworkApp
    = NetworkApp
      { na_inputStream :: Streams.InputStream BS.ByteString
      , na_outputStream :: Streams.OutputStream BS.ByteString
      , na_sockAddr :: NetSock.SockAddr
      , na_localAddr :: Maybe NetSock.SockAddr
      }

runTCPServer :: ServerSettings -> (NetworkApp -> IO ()) -> IO ()
runTCPServer settings handler =
    runTCPServerWithHandle settings $ \sock remoteAddr mLocalAddr ->
        do (istream, ostream) <- Streams.socketToStreams sock
           let app = NetworkApp
                     { na_inputStream = istream
                     , na_outputStream = ostream
                     , na_sockAddr = remoteAddr
                     , na_localAddr = mLocalAddr
                     }
           handler app

runTCPClient :: ClientSettings -> (NetworkApp -> IO ()) -> IO ()
runTCPClient settings handler =
    bracket ("runTCPClient(" ++ show (getHost settings) ++ ":" ++ show (getPort settings))
            (getSocketFamilyTCP (getHost settings) (getPort settings) (getAddrFamily settings))
            (Net.sClose . fst)
            (\(sock, addr) ->
                do (istream, ostream) <- Streams.socketToStreams sock
                   let app = NetworkApp
                             { na_inputStream = istream
                             , na_outputStream = ostream
                             , na_sockAddr = addr
                             , na_localAddr = Nothing
                             }
                   handler app)

bufSize :: Int64
bufSize = 4096

instance Read Net.PortNumber where
    readsPrec p s =
        do (p, rest) <- readsPrec p s
           return (fromInteger p, rest)

type SendPayload = [BSL.ByteString]

data SafeSocket
    = SafeSocket
      { ss_socket      :: NetSock.Socket
      , ss_receiveChan :: ReceiveChan
      , ss_receiveTid  :: KillableThreadIdIO
      , ss_sendChan    :: SendChan
      , ss_sendTid     :: KillableThreadIdIO
      , ss_closed      :: TVar Bool
      , ss_sockname    :: String
      }

instance Show SafeSocket where
    showsPrec _ ss = showString (ss_sockname ss)


data SendStatus = SendInProgress | SendFinished | SendFailed SomeException
data ReceiveStatus = ReceiveFinished BSL.ByteString | ReceiveFailed SomeException

rawSocket :: SafeSocket -> NetSock.Socket
rawSocket = ss_socket

mkSafeSocket :: NetSock.Socket -> IO SafeSocket
mkSafeSocket sock =
    do peer <- NetSock.getPeerName sock
       recvChan <- runTx $ newReceiveChan
       sendChan <- runTx $ newSendChan
       closed <- runTx $ newTVar False
       recvTid <- forkKillableThreadIO WorkerThread ("receive thread for " ++ show sock) $
                  receiveLoop sock recvChan
       sendTid <- forkKillableThreadIO WorkerThread ("send thread for " ++ show sock) $
                  sendLoop sock sendChan recvTid
       let sockname =
               ($ "") $
               showString "<" .
               showsPrec 5 peer .
               showString ", " .
               shows sock .
               showString ", " .
               shows recvTid .
               showString ", " .
               shows sendTid .
               showChar '>'

       return $ SafeSocket { ss_socket      = sock
                           , ss_receiveChan = recvChan
                           , ss_receiveTid  = recvTid
                           , ss_sendChan    = sendChan
                           , ss_sendTid     = sendTid
                           , ss_closed      = closed
                           , ss_sockname    = sockname
                           }

logTag :: Tags
logTag = tags ["net"]

type ReceiveChan = TVar (Maybe (TMVar ReceiveStatus))

newReceiveChan :: STM ReceiveChan
newReceiveChan =
    do realVar <- newEmptyTMVar
       poisonVar <- newTVar (Just realVar)
       return poisonVar

unpoison :: TVar (Maybe a) -> STM a
unpoison chan =
    do box <- readTVar chan
       case box of
         Nothing -> throwSTM KillException
         Just var -> return var

putReceiveChan :: ReceiveChan -> ReceiveStatus -> STM ()
putReceiveChan chan value =
    do var <- unpoison chan
       putTMVar var value

takeReceiveChan :: ReceiveChan -> STM ReceiveStatus
takeReceiveChan chan =
    do var <- unpoison chan
       takeTMVar var

tryTakeReceiveChan :: ReceiveChan -> STM (Maybe ReceiveStatus)
tryTakeReceiveChan chan =
    do var <- unpoison chan
       tryTakeTMVar var

receiveLoop sock chan =
    do let msg = "receiving on " ++ show sock
       dat <- bracketThreadActivity msg (do dat <- NetBSL.recv sock bufSize
                                            return $ ReceiveFinished dat)
              `catch` (\e -> return $ ReceiveFailed e)
       case dat of
         ReceiveFinished dat ->
             do logDebugWithTag logTag ("received " ++ show (BSL.length dat) ++ " bytes"
                                        ++ " on " ++ show sock)
                logTraceWithTag logTag ("received the following bytes on " ++ show sock ++ "\n" ++
                                        show dat)
         ReceiveFailed {} ->
             logInfoWithTag logTag ("receive on " ++ show sock ++ " failed")
       runTx'  $ putReceiveChan chan dat
       case dat of
            ReceiveFailed _ -> return ()
            _ -> receiveLoop sock chan

poisonAndKill pvar tid =
    do runTx (writeTVar pvar Nothing)
       killThreadIO tid

killReceiveThread sock = poisonAndKill (ss_receiveChan sock) (ss_receiveTid sock)

type SendChan = TVar (Maybe (TMVar (SendPayload, TVar SendStatus)))

newSendChan :: STM SendChan
newSendChan =
    do var <- newEmptyTMVar
       newTVar (Just var)

takeSendChan chan =
    do var <- unpoison chan
       takeTMVar var

putSendChan chan value = unpoison chan >>= flip putTMVar value

sendLoop sock chan recvTid =
    do (bss, finishedSignal) <-  runTxWithName "waiting for data" (takeSendChan chan)
       let size = F.foldl' (\s bs -> s + BSL.length bs) 0 bss
           msg = "sending " ++ show size ++ " on " ++ show sock
       sig <- bracketThreadActivity msg (do mapM_ (NetBSL.sendAll sock) bss
                                            return SendFinished)
              `catch` (\e -> return $ SendFailed e)
       logDebugWithTag logTag ("sent " ++ show size ++ " bytes on " ++ show sock)
       runTxWithName "signalling write finished" $ writeTVar finishedSignal sig
       case sig of
         SendFailed _ -> return ()
         _ -> sendLoop sock chan recvTid

killSendThread sock = poisonAndKill (ss_sendChan sock) (ss_sendTid sock)


accept :: NetSock.Socket -> IO (SafeSocket, NetSock.SockAddr)
accept sock =
    do let msg = "accepting on " ++ show sock
       (clsSock, addr) <- bracketThreadActivity msg (NetSock.accept sock)
       safeSock <- mkSafeSocket clsSock
       return $ (safeSock, addr)

connect :: Net.HostName -> Net.PortNumber -> IO SafeSocket
connect host port =
    bracketOnError ("connecting to " ++ show host ++ ":" ++ show port) openSock closeSock withSock
    where
      openSock = NetSock.socket NetSock.AF_INET NetSock.Stream NetSock.defaultProtocol
      closeSock = NetSock.sClose
      withSock sock =
          do hostEntry <- NetBsd.getHostByName host
             let addr = NetSock.SockAddrInet port (NetBsd.hostAddress hostEntry)
                 msg = "connecting " ++ show sock ++ " to " ++ show addr
             bracketThreadActivity msg (NetSock.connect sock addr)
             logInfo (show sock ++ " connected to " ++ show addr)
             mkSafeSocket sock

recv :: SafeSocket -> IO BSL.ByteString
recv sock =
    runTxWithName "waiting for received bytes" $
    do checkClosed sock
       status <- takeReceiveChan (ss_receiveChan sock)
       case status of
            ReceiveFinished dat -> return dat
            ReceiveFailed e -> throw e

recvWithTimeout :: TimeSpan -> SafeSocket -> IO (Maybe BSL.ByteString)
recvWithTimeout ts sock =
    do -- do a cheap check first...
       mstatus <- checkData
       mstatus' <-
           case mstatus of
             Just _ -> return mstatus
             Nothing ->
                 do mvar <- runTx newEmptyTMVar
                    _ <- timeout ts $
                         runTxWithName "waiting for receive" $
                         do status <- takeReceiveChan (ss_receiveChan sock)
                            checkClosed sock
                            putTMVar mvar status
                    runTx $ tryTakeTMVar mvar
       case mstatus' of
         Just status -> withStatus status
         Nothing -> return Nothing
    where
      checkData =
          runTxWithName "checking receive chan" $
          do checkClosed sock
             tryTakeReceiveChan (ss_receiveChan sock)
      withStatus status =
             case status of
               ReceiveFinished dat -> return (Just dat)
               ReceiveFailed e -> throw e


checkClosed :: SafeSocket -> STM ()
checkClosed sock =
    do isClosed <- readTVar (ss_closed sock)
       when isClosed $ throw $
           mkIOError eofErrorType ("Socket " ++ show sock ++ " closed") Nothing Nothing

send ::  SafeSocket -> BSL.ByteString -> IO ()
send sock dat = send' sock [dat]

send' ::  SafeSocket -> SendPayload -> IO ()
send' sock dat =
    do finished <-  runTxWithName "sending data" $
                    do checkClosed sock
                       finished <- newTVar SendInProgress
                       putSendChan (ss_sendChan sock) (dat, finished)
                       return finished
       mExc <- runTxWithName "waiting for send completion" $
         do checkClosed sock
            isFinished <- readTVar finished
            case isFinished of
                 SendFinished -> return Nothing
                 SendFailed e -> return $ Just e
                 SendInProgress -> retry
       case mExc of
            Nothing -> return ()
            Just exc -> throwIO exc

sClose :: SafeSocket -> IO ()
sClose sock =
    do wasClosed <- runTxWithName "closing connection" $
         do wasClosed <- readTVar (ss_closed sock)
            writeTVar (ss_closed sock) True
            return wasClosed
       if wasClosed
          then logInfo "Connection was already closed."
          else
              do logInfo ("Closing safe socket " ++ show sock ++ "...")
                 killReceiveThread sock
                 killSendThread sock
                 logInfo ("Killed send and receive thread (" ++ show (ss_sendTid sock) ++
                          " and " ++ show (ss_receiveTid sock) ++ ")")
                 rawClose (ss_socket sock)
                 logNote ("Safe socket " ++ show sock ++ " closed.")

rawClose :: NetSock.Socket -> IO ()
rawClose sock = bracketThreadActivity ("closing " ++ show sock) (NetSock.sClose sock)

setupStunnelLogDir :: IO FilePath
setupStunnelLogDir =
    do baseLogDir <- getLoggingRootDir
       let logDir = baseLogDir </> "stunnel"
       createDirectoryIfMissing True logDir
       return logDir

data StunnelServerConfig
    = StunnelServerConfig
      { -- Certificate Authority directory
        -- This is the directory in which stunnel will look for
        -- certificates when using the verify. Note that the certificates in this
        -- directory should be named XXXXXXXX.0 where XXXXXXXX is the hash value of
        -- the DER encoded subject of the cert (the first 4 bytes of the MD5 hash in
        -- least significant byte order).
        ssc_certificateAuthorityDirectory :: FilePath
        -- The CA's certificate chain
      , ssc_certificateAuthorityCertificate :: FilePath
        -- Certificate Revocation Lists directory
        -- This is the directory in which stunnel will look for CRLs when using the verify.
        -- Note that the CRLs in this directory should be named XXXXXXXX.0 where XXXXXXXX
        -- is the hash value of the CRL.
      , ssc_revocationDirectory :: FilePath
        -- certificate chain PEM file name
      , ssc_certFile :: FilePath
        -- private key for certificate specified with cert option
      , ssc_keyFile :: FilePath
      , ssc_logDir :: Maybe FilePath
        -- path of the stunnel executable (defaults to /usr/bin/stunnel)
      , ssc_executable :: Maybe FilePath
      }
      deriving (Show, Read, Eq)

mkStunnelServerConfig :: Maybe FilePath -> FilePath -> IO StunnelServerConfig
mkStunnelServerConfig mexec baseDir =
    do logDir <- setupStunnelLogDir
       return $ StunnelServerConfig
                  { ssc_certificateAuthorityDirectory = baseDir </> "client-certs"
                  , ssc_revocationDirectory = baseDir </> "revocations"
                  , ssc_certFile = baseDir </> "stunnel-server.crt"
                  , ssc_keyFile = baseDir </> "stunnel-server.pem"
                  , ssc_logDir = Just logDir
                  , ssc_certificateAuthorityCertificate = baseDir </> "ipad-ca-chain.pem"
                  , ssc_executable = mexec
                  }

data StunnelClientConfig
    = StunnelClientConfig
      { scc_certFile :: FilePath
      , scc_logDir :: Maybe FilePath
      , scc_certificateAuthorityCertificate :: FilePath
      , scc_keyFile :: FilePath
      , scc_executable :: Maybe FilePath
      }
      deriving (Show)

mkStunnelClientConfig :: Maybe FilePath -> FilePath -> IO StunnelClientConfig
mkStunnelClientConfig mexec baseDir =
    do logDir <- setupStunnelLogDir
       return $ StunnelClientConfig { scc_keyFile = baseDir </> "stunnel-client.pem"
                                    , scc_certificateAuthorityCertificate = baseDir </>
                                                                            "ipad-ca-chain.pem"
                                    , scc_certFile = baseDir </> "client-certs" </> "d015febb.0"
                                    , scc_logDir = Just logDir
                                    , scc_executable = mexec
                                    }

genStunnelClientConfigString ::
       StunnelClientConfig
    -> Net.PortNumber
    -> Net.HostName
    -> Net.PortNumber
    -> (String, FilePath, FilePath, FilePath)
genStunnelClientConfigString cfg localPort remoteHost remotePort =
    (cfgStr, cfgFileDir, stdoutLog, stderrLog)
    where
      cfgStr = unlines ["foreground = yes"
                       ,"CAfile = " ++ scc_certificateAuthorityCertificate cfg
                       ,"cert = " ++ scc_certFile cfg
                       ,"key = " ++ scc_keyFile cfg
                       ,"pid ="
                       ,"output = " ++ logFile
                       ,"syslog = no"
                       ,"[stunnel-client-" ++ remote ++ "]"
                       ,"client = yes"
                       ,"connect = " ++ remote
                       ,"accept = " ++ show localPort
                       ]
      remote = remoteHost ++ ":" ++ show remotePort
      (cfgFileDir, logFile, stdoutLog, stderrLog) =
          case scc_logDir cfg of
            Nothing -> ("/tmp", "/dev/stderr", "/dev/stdout", "/dev/stderr")
            Just dir ->
                let f = dir </> ("client-" ++ remoteHost ++ "-" ++ show remotePort)
                in (dir, f, f <.> "out", f <.> "err")

_SSL_SERVER_PORT_OFFSET_ :: Net.PortNumber
_SSL_SERVER_PORT_OFFSET_ = 20000

_SSL_CLIENT_PORT_OFFSET_ :: Net.PortNumber
_SSL_CLIENT_PORT_OFFSET_ = 40000

-- TODO: Logging, killing

type Pid = Int
type ProcessName = String

stunnelExecutableDefault :: String
stunnelExecutableDefault = "/usr/bin/stunnel"

sccGetExecutable cfg = fromMaybe stunnelExecutableDefault (scc_executable cfg)

checkPort :: Net.PortNumber -> IO (Maybe Pid)
checkPort port =
    do fuserRes <- runCmd [1] ("fuser -n tcp " ++ show port)
       case fuserRes of
         "" -> return Nothing
         s ->
             case s =~ "[0-9]+/tcp:[[:space:]]+([0-9]+)" of
               ("", _::String, "", [pidStr]) ->
                   case readMay pidStr of
                     Nothing -> do logWarn ("Unexpected pid: " ++ pidStr ++
                                            ", output of fuser: " ++ s)
                                   return Nothing
                     Just pid -> return $ Just pid
               _ ->
                   case readMay s of
                     Nothing -> do logWarn ("Unexpected output of fuser: " ++ show s)
                                   return Nothing
                     Just pid -> return $ Just pid

resolvePid :: Pid -> IO (ProcessName, String)
resolvePid pid =
    do s <- runCmd [1] ("ps -p " ++ show pid ++ " -o command=")
       let (procName, args) = List.span (not . isSpace) s
       return (procName, strip args)

runCmd :: [Int] -> String -> IO String
runCmd okFailures cmd =
    do (ecode, out, err) <- bracketThreadActivity ("calling " ++ cmd) $
                            readShellCmdWithExitCode cmd BSL.empty
       case ecode of
         ExitFailure i | not (i `elem` okFailures)  ->
                           do logWarn ("Command " ++ show cmd ++ " failed with exit code " ++
                                       show ecode ++ ", stderr: " ++ BSLC.unpack err)
                              return ""
         _ -> return $ strip $ BSLC.unpack out

listenOnWithSsl :: StunnelServerConfig -> Net.PortNumber -> IO Net.Socket
listenOnWithSsl cfg publicPort =
    do privatePort <- startStunnel cfg publicPort
       Net.listenOn (Net.PortNumber privatePort)

startStunnel :: StunnelServerConfig -> Net.PortNumber -> IO Net.PortNumber
startStunnel _cfg publicPort =
    do let privatePort = _SSL_SERVER_PORT_OFFSET_ + publicPort
       logNote ("Listing on private port " ++ show privatePort ++
                " make sure stunnel redirects requests on port " ++ show publicPort ++
                " to this private port")
       return privatePort

waitForPort :: Net.PortNumber -> IO Bool
waitForPort port = loop 10 0
    where
      loop millis total =
          do if (millis > 1000)
                then do logError ("Port still not ready after " ++ show total ++
                                  " milliseconds, giving up")
                        return False
                else do sleep millis
                        mPid <- checkPort port
                        case mPid of
                          Nothing -> loop (10 * millis) (millis + total)
                          Just _ -> return True

runStunnel :: String -> String -> [String] -> (String, FilePath, FilePath, FilePath) -> IO (IO ())
runStunnel ident cmd extraArgs cfgWithLogs@(cfgString, cfgFileDir, out, err) =
    withPersistentTempFile cfgFileDir "stunnel.cfg" $ \(cfgFileName, cfgFileHdl) ->
    do hPutStrLn cfgFileHdl cfgString
       hClose cfgFileHdl
       logInfo ("Starting stunnel " ++ ident ++ " with command " ++ cmd ++
                " and configuration from file " ++ cfgFileName)
       logInfo ("Stunnel configuration is\n" ++ cfgString)
       let args = [cfgFileName] ++ extraArgs
       t0 <- getClockTime
       outh <- openFile out WriteMode
       errh <- openFile err WriteMode
       (Just inp, _, _, ph) <- createProcess (proc cmd args) { std_in  = CreatePipe
                                                             , std_out = UseHandle outh
                                                             , std_err = UseHandle errh }
       suicide <- newIORef False
       hClose inp
       let term =
               do writeIORef suicide True
                  logInfo "about to terminate stunnel"
                  hClose outh
                  hClose errh
                  terminateProcess ph
                  logInfo "terminated stunnel"
       exitActionId <-
           registerExitAction "terminate stunnel" (ExitActionImportant, ExitDelayShort) term
       fork_ WorkerThread "runStunnel" $
            do ecode <- waitForProcess ph
               hClose outh
               hClose errh
               isSuicide <- readIORef suicide
               logInfo ("stunnel terminated with exit code " ++ show ecode ++ ", suicide = "
                        ++ show isSuicide)
               t1 <- getClockTime
               let diff = t1 `diffClockTimes` t0
               case () of
                 _| isSuicide -> unregisterExitAction exitActionId
                  | diff > minDiff ->
                      do logWarn ("stunnel " ++ ident ++ " terminated with exit code " ++
                                  show ecode ++ " after " ++ show diff ++ ", restarting...")
                         _ <- runStunnel ident cmd extraArgs cfgWithLogs
                         return ()
                  | otherwise ->
                      logWarn ("stunnel " ++ ident ++ " terminated with exit code " ++
                               show ecode ++ " after " ++ show diff ++ ", not restarting!")
       return term
    where
      minDiff = noTimeDiff { tdSec = 1 }

clientLock :: MSem Int
clientLock = unsafePerformIO (MSem.new 1)
{-# NOINLINE clientLock #-}

connectWithSsl :: StunnelClientConfig -> Net.HostName -> Net.PortNumber -> IO SafeSocket
connectWithSsl cfg remoteHost remotePort =
    do let localPort = _SSL_CLIENT_PORT_OFFSET_ + remotePort
           cfgWithLogs = genStunnelClientConfigString cfg localPort remoteHost remotePort
           remote = remoteHost ++ ":" ++ show remotePort
       mPid <- withLock "starting stunnel" $
                 do mPid <- checkPort localPort
                    case mPid of
                      Nothing ->
                          do term <- runStunnel ("to " ++ remote) stunnelExecutable [remote]
                                                cfgWithLogs
                             ok <- waitForPort localPort
                             unless ok $ do term
                                            logAndFail ("Port not ready, giving up and failing")
                      _ -> return ()
                    return mPid
       case mPid of
         Nothing -> return ()
         Just pid ->
             do (procName, args) <- resolvePid pid
                when (procName /= stunnelExecutable || not (remote `List.isSuffixOf` args)) $
                     logAndFail ("Local port " ++ show localPort ++
                                 " is occupied by process " ++ procName ++ " with arguments " ++
                                 args ++ ". Cannot use this process to connect with SSL to " ++
                                 remote)
       connect "127.0.0.1" localPort
    where
      stunnelExecutable = sccGetExecutable cfg
      withLock desc action =
          bracket desc (MSem.wait clientLock) (\() -> MSem.signal clientLock) (\() -> action)
