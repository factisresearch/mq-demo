{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mgw.Util.Logging.LogWriter
    ( setupLoggingWithConfig, setupLoggingWithConfigAndErrorHook, setupLogging
    , setupLoggingWithLevel, teardownLogging
    , startLoggingThread, getDynamicLogConfig, setDynamicLogConfig, isLoggingActive
    , withDynamicLogConfig, withLogLevel
    , dumpBytesIO
    , getMainLogFile, getLoggingRootDir
    )
where

#include "src/macros.h"

import Prelude hiding (init, log,  sequence_, mapM_)

import Control.Exception
    ( Exception, Handler(..), SomeException, AsyncException(..), IOException
    , catch, catches, bracket, finally, evaluate
    )
import Control.Concurrent (threadDelay, forkIO, ThreadId, myThreadId, throwTo)
import Control.Monad (liftM, foldM, when, unless)
import Control.DeepSeq

import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.Foldable (mapM_, foldlM)
import Data.List ((\\))
import Data.Maybe (isJust, isNothing, mapMaybe, catMaybes)
import Data.Sequence (Seq)
import Data.Ord (comparing)
import Data.Typeable (Typeable)

import System.FilePath (dropExtensions, (</>), dropFileName)
import System.Time (ClockTime, getClockTime, CalendarTime(..), toCalendarTime)
import System.IO (stderr, stdin, stdout,
                  hPutStr, hPutStrLn, hSetBinaryMode, openFile, IOMode(..), hClose,
                  hSetBuffering, BufferMode(..), hFlush)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.IO.Unsafe (unsafePerformIO)

import System.Posix.Terminal
import System.Posix.IO

import Text.Printf (printf)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Map as Map

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Safe (fromJustNote)
import Data.Strict.Tuple hiding (fst, snd)
import qualified System.Log as Log
import qualified System.Log.Logger as Log

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Ansi
import Mgw.Util.DynConfig
import Mgw.Util.Logging.Core
import Mgw.Util.Logging.LogClient
import Mgw.Util.Logging.LogFormatting
import Mgw.Util.Misc (findNonexistingFile, milliSecsFromPicoSecs)
import Mgw.Util.Time hiding (getClockTime)
import Mgw.Util.TimeSpan
import Mgw.Util.Sleep (sleepTimeSpan)

import Mgw.Util.Locations (setupLogDirs)

_MAX_LOGMSG_DELAY_ :: TimeSpan
_MAX_LOGMSG_DELAY_ = seconds 10

_MSG_EVAL_TIME_WARN_ :: TimeSpan
_MSG_EVAL_TIME_WARN_ = seconds 1

_WITH_NEWLINE_ = False
_NUMBERED_DUMPS_ = True

type ErrorHook = IO ()

data KillLoggingLoop = KillLoggingLoop deriving (Show, Typeable)

instance Exception KillLoggingLoop

localLog file line level msg =
    doLog (adjustSourceFilePath file) line level noExtraLogInfo (Left msg)

setupLogging :: Bool -> IO ()
setupLogging verbose = setupLoggingWithLevel (if verbose then DEBUG else INFO)

setupLoggingWithLevel :: LogLevel -> IO ()
setupLoggingWithLevel level =
    let dynCfg = defaultDynamicLogConfig { lc_defaultLevel = level }
    in setupLoggingWithConfig (defaultLogConfig { lc_dynamic = dynCfg })

setupLoggingWithConfig :: LogConfig -> IO ()
setupLoggingWithConfig cfg = setupLoggingWithConfigAndErrorHook cfg (return ())

setupLoggingWithConfigAndErrorHook :: LogConfig -> ErrorHook -> IO ()
setupLoggingWithConfigAndErrorHook cfg errorHook =
    do mtid <- readIORef loggingThreadId
       case mtid of
         Just _ -> fail "setupLogging called more than once"
         Nothing -> return ()
       setDynamicLogConfig (lc_dynamic cfg)
       () <- atomicModifyIORef _LOGQUEUE_ (\(_,q) -> ((True,q), ()))
       tid <- startLoggingThread (lc_static cfg) errorHook
       writeIORef loggingThreadId (Just tid)
       writeIORef staticLogConfig (lc_static cfg)
       writeIORef loggingThreadShutdown False
       setupHsLogger
       localLog __FILE__ __LINE__ INFO $ "Started logging with this configuration: " ++ show cfg
    where
      setupHsLogger =
          do hsLogger <- Log.getRootLogger
             let hsLogger' = (Log.setHandlers [HsLoggerHandler] .
                              Log.setLevel Log.DEBUG)
                             hsLogger
             Log.saveGlobalLogger hsLogger'

closeAllLogHandles :: LogTargets -> IO ()
closeAllLogHandles (LogTargets defaultHandles specHandles tagHandleMap) =
    do mapM_ closeHandle (filterStdHandles (map lh_handle defaultHandles))
       mapM_ closeHandle (filterStdHandles (map (lh_handle . snd) specHandles))
       mapM_ closeHandle (filterStdHandles (map lh_handle (Map.elems tagHandleMap)))
       return ()
    where filterStdHandles handles = handles \\ [stdin,stderr,stdout]
          closeHandle h = do hClose h
                          `catch` (\(_::IOException) ->
                                       hPutStrLn stderr ("error while closing " ++ show h))

isLoggingActive :: IO Bool
isLoggingActive = liftM isJust (readIORef loggingThreadId)

teardownLogging :: IO ()
teardownLogging =
    do mtid <- readIORef loggingThreadId
       case mtid of
         Nothing -> hPutStrLn stderr "teardownLogging: setupLogging not called"
         Just tid ->
             do cfg <- readIORef staticLogConfig
                localLog __FILE__ __LINE__ NOTE "Terminating logging thread and closing files."
                -- disable logging of new messages
                () <- atomicModifyIORef _LOGQUEUE_ (\(_,q) -> ((False, q),()))
                -- wait until all log messages have been taken out
                let waitEmptyQ =
                        do done <- liftM (Seq.null . snd) (readIORef _LOGQUEUE_)
                           unless done $
                                  do delay <- getDynValue dynCfg_logWriterDelay
                                     threadDelay (asMilliseconds delay * 500)
                                     waitEmptyQ
                    waitThreadDone count
                        | count >= 0 =
                            do done <- liftM isNothing (readIORef loggingThreadId)
                               unless done $
                                      do threadDelay (100 * 1000)
                                         waitThreadDone (count - 1)
                        | otherwise = return ()
                waitEmptyQ
                writeIORef loggingThreadShutdown True
                waitThreadDone 30
                writeIORef loggingThreadId Nothing
                throwTo tid KillLoggingLoop
                when ("stderr" `elem` lc_defaultTargets cfg) $
                     hPutStrLn stderr "Stopped logging thread.  You're left to your own now."

loggingThreadId :: IORef (Maybe ThreadId)
loggingThreadId = unsafePerformIO (newIORef Nothing)
{-# NOINLINE loggingThreadId #-}

dynamicLogConfig :: IORef DynamicLogConfig
dynamicLogConfig = unsafePerformIO (newIORef defaultDynamicLogConfig)
{-# NOINLINE dynamicLogConfig #-}

staticLogConfig :: IORef StaticLogConfig
staticLogConfig = unsafePerformIO (newIORef defaultStaticLogConfig)
{-# NOINLINE staticLogConfig #-}

loggingThreadShutdown :: IORef Bool
loggingThreadShutdown = unsafePerformIO (newIORef False)
{-# NOINLINE loggingThreadShutdown #-}

getDynamicLogConfig :: IO DynamicLogConfig
getDynamicLogConfig =
    atomicModifyIORef dynamicLogConfig (\x -> (x, x))

setDynamicLogConfig :: DynamicLogConfig -> IO ()
setDynamicLogConfig cfg =
    do atomicModifyIORef dynamicLogConfig (\_ -> (sortedCfg, ()))
       atomicModifyIORef _MINLEVEL_ (const $ (minlevel, ()))
    where
      sortedCfg =
          cfg { lc_levelMap = reverse $ List.sortBy (comparing (length . fst)) (lc_levelMap cfg) }
      minlevel =
          minimum $ (lc_defaultLevel cfg : map snd (lc_levelMap cfg)) ++
                    map snd (lc_enabledTags cfg)

withDynamicLogConfig :: DynamicLogConfig -> IO a -> IO a
withDynamicLogConfig cfg action =
    bracket (do old <- getDynamicLogConfig
                setDynamicLogConfig cfg
                return old)
            setDynamicLogConfig
            (\_ -> action)

withLogLevel :: LogLevel -> IO a -> IO a
withLogLevel ll action =
    do cur <- getDynamicLogConfig
       withDynamicLogConfig  (cur { lc_defaultLevel = ll }) action

startLoggingThread :: StaticLogConfig -> ErrorHook -> IO ThreadId
startLoggingThread cfg errorHook =
    do (rootDir, targets) <- init cfg
       atomicModifyIORef rootDirVar (\_ -> (Just rootDir, ()))
       forkIO $ loggingLoop errorHook rootDir targets

init :: StaticLogConfig -> IO (FilePath, LogTargets)
init cfg =
    do (_baseDir, rootDir) <- setupLogDirs (Just $ lc_progName cfg) (lc_rootDir cfg)
       mDefaultHandles <- mapM (setupHandle rootDir)
                               (List.nub (lc_mainTarget cfg : lc_defaultTargets cfg))
       let defaultHandles = catMaybes mDefaultHandles
       specificHandleMap <- foldM (\mapping name ->
                                       case Map.lookup name mapping of
                                         Just _ -> return mapping
                                         Nothing -> do mh <- setupHandle rootDir name
                                                       return $
                                                         case mh of
                                                           Just h -> Map.insert name h mapping
                                                           Nothing -> mapping)
                                  Map.empty (map snd $ lc_targets cfg)
       let specificHandles =
               map (\(topic,file) -> (topic, fromJustNote ("no handle for " ++ file) $
                                             Map.lookup file specificHandleMap))
                   (lc_targets cfg)
       unless ("stderr" `elem` lc_defaultTargets cfg) $
              do hPutStrLn stderr ("Logging to directory " ++ rootDir)
       return (rootDir, LogTargets defaultHandles specificHandles Map.empty)

setupHandle :: FilePath -> String -> IO (Maybe LogHandle)
setupHandle rootDir name =
    do (h, isStderr) <- if name == "stderr"
                           then do b <- queryTerminal stdError
                                   return (stderr, b)
                           else do let fileName = rootDir </> name
                                   createDirectoryIfMissing True (dropFileName fileName)
                                   f <- openFile fileName AppendMode
                                   return (f, False)
       hSetBinaryMode h True
       hSetBuffering h LineBuffering
       return $ Just  (LogHandle {
                         lh_handle = h
                       , lh_colors = isStderr
                       , lh_beep = isStderr
                       , lh_isStderr = isStderr })
    `catch`
      (\(e::IOException) ->
           do hPutStrLn stderr ("Could not setup log file " ++ (rootDir </> name) ++
                                ": " ++ show e)
              return Nothing)

loggingLoop :: ErrorHook -> FilePath -> LogTargets -> IO ()
loggingLoop errorHook rootDir targets =
    flip finally (writeIORef loggingThreadId Nothing) $
    do tid <- myThreadId
       loop tid targets
    `catches`
    [ Handler $ \(e::AsyncException) ->
          case e of
            UserInterrupt ->
                do hPutStrLn stderr "Terminating logging loop because of Control-C"
                   shutdown
            _ ->
                do hPutStrLn stderr ("The logging loop was killed (" ++ show e ++
                                     "), trying to restart it ...")
                   _ <- forkIO $ loggingLoop errorHook rootDir targets
                   return ()
    , Handler $ \(_::KillLoggingLoop) -> shutdown
    ]
    where
      shutdown = closeAllLogHandles targets
      loop tid targets =
          do seq <- atomicModifyIORef _LOGQUEUE_ dequeue
             dynCfg <- readIORef dynamicLogConfig
             now <- getClockTime
             (tmpTargets :!: skipped) <-
                  foldlM (\ts lm -> printLogMessage now errorHook dynCfg rootDir ts lm
                                    `catch` excHandler ts) (targets :!: 0) seq
             newTargets <-
                 if (skipped > 0)
                    then do t <- getClockTime
                            let msg = LogMessage
                                      { lm_time = t
                                      , lm_threadId = tid
                                      , lm_insideStm = False
                                      , lm_file = adjustSourceFilePath __FILE__
                                      , lm_line = __LINE__
                                      , lm_level = WARN
                                      , lm_extraInfo = noExtraLogInfo
                                      , lm_msg = Left ("Skipped " ++ show skipped ++
                                                       " messages because they were at least " ++
                                                       displayTimeSpan _MAX_LOGMSG_DELAY_ ++
                                                       " delayed") }
                            reallyPrintLogMessage rootDir tmpTargets msg
                    else return tmpTargets
             getDynValue dynCfg_logWriterDelay >>= sleepTimeSpan
             done <- readIORef loggingThreadShutdown
             unless done (loop tid newTargets)

dequeue :: (Bool, Seq LogMessage) -> ((Bool, Seq LogMessage), Seq LogMessage)
dequeue (flag, seq) = ((flag,Seq.empty), seq)

excHandler :: a -> IOException -> IO a
excHandler targetsAndSkipped e =
    do hPutStrLn stderr ("Exception in logging thread: " ++ show e)
       return targetsAndSkipped

printLogMessage timeFromQueue errorHook dynCfg rootDir (targets :!: skipped) logMsg =
    {-# SCC "printLogMessage/Logging" #-}
    do res <- ifTimeOk timeFromQueue logMsg targets $
              ifLevelOk dynCfg src level extra targets $
                {-# SCC "action/printLogMessage/Logging" #-}
                case eli_dumpId extra of
                  Just (dumpId, dumpMode) ->
                      do let fileMode = if overwrite then WriteMode else AppendMode
                             doPrefix = dumpMode == DumpAppendWithPrefix
                             overwrite = dumpMode == DumpOverwrite
                         fname <- getDumpFileName (not overwrite) rootDir logMsg dumpId
                         bracket (openFile fname fileMode)
                                 (hClose)
                                 (\h -> do hSetBinaryMode h True
                                           when doPrefix $ getLogMsgPrefix logMsg True >>= hPutStr h
                                           case lm_msg logMsg of
                                             Left s ->
                                                 (if overwrite then hPutStr else hPutStrLn) h s
                                             Right bs -> BSL.hPut h bs)
                         ifLevelOk dynCfg src level noExtraLogInfo targets (logDumpMsg fname)
                  _ -> reallyPrintLogMessage rootDir targets logMsg
       when (lm_level logMsg >= ERROR) $
            errorHook `catch`
            (\(e::SomeException) -> hPutStrLn stderr ("Exception in error hook: " ++ show e))
       return (fst res :!: skipped + if snd res then 1 else 0)
    where
      src = lm_file logMsg
      level = lm_level logMsg
      extra = lm_extraInfo logMsg
      logDumpMsg fname =
          reallyPrintLogMessage rootDir targets msg
          where
            msg = logMsg { lm_msg = Left $ "dumped to " ++ fname, lm_extraInfo = noExtraLogInfo }

color level = {-# SCC "color/Logging" #-}
    case level of
      TRACE -> _DARK_GRAY_
      DEBUG -> _BLUE_
      INFO  -> _LIGHT_BLUE_
      NOTE  -> _LIGHT_GREEN_
      WARN  -> _BROWN_
      ERROR -> _LIGHT_RED_
      ALERT -> _YELLOW_

reallyPrintLogMessage rootDir targets logMsg =
    do prefix <- getLogMsgPrefix logMsg _WITH_NEWLINE_
       (tagHandles, targets') <- getTagHandles rootDir (lm_extraInfo logMsg) targets
       let level = lm_level logMsg
           handles = List.nub $
                     lt_defaultHandles targets ++
                     tagHandles ++
                     mapMaybe (\(s,h) -> if s `List.isPrefixOf` lm_file logMsg
                                            then Just h
                                            else Nothing)
                              (lt_specificHandles targets)
           hasStderr = any lh_isStderr handles
       msg <-
           case lm_msg logMsg of
             Left s ->
                 do (t, sEvaluated) <- measure (evaluate ({-# SCC "msg/printLogMessage/Logging" #-}
                                                          (s `deepseq` s)))
                    return $ Left $
                       if t >= _MSG_EVAL_TIME_WARN_
                          then (sEvaluated ++ " [!! evaluation of this log message took "
                                ++ displayTimeSpan t ++ " !!]")
                          else sEvaluated
             Right bs -> return $ Right bs
       let printToHandle (LogHandle h doColors doBeep isStderr) =
               unless (isStderr && ignoreStderr) $
               do when doColors $ hPutStr h (color level)
                  when (doBeep && level >= ERROR) (hPutStr h "\a")
                  ({-# SCC "hPutStr1/printLogMessage/Logging" #-}
                   hPutStr h ({-# SCC "prefix/printLogMessage/Logging" #-} prefix))
                  hFlush h
                  ({-# SCC "hPutStr2/printLogMessage/Logging" #-}
                   case msg of
                     Left s -> hPutStrLn h s
                     Right bs -> BSL.hPut h bs)
                  when doColors $ hPutStr h _RESET_
                  ({-# SCC "hFlush/printLogMessage/Logging" #-}
                   hFlush h)
       mapM_ (\h -> printToHandle h `catch`
                    (\(e::IOException) -> hPutStrLn stderr ("Error printing to handle " ++
                                                              show h ++ ": " ++ show e)))
             handles
       when (lm_level logMsg >= WARN && not hasStderr) $
            printToHandle (LogHandle stderr False False True)
       return targets'
    where
      ignoreStderr = eli_ignoreStderr (lm_extraInfo logMsg)

getDumpFileName append rootDir logMsg dumpId =
    genericGetDumpFileName append rootDir (dropExtensions file)
                           dumpId (dumpId ++ "_" ++ show line) "" ts
    where  ts = lm_time logMsg
           file = lm_file logMsg
           line = lm_line logMsg

genericGetDumpFileName append rootDir subDir dumpId filePrefix ext ts =
    do let dumpsDir = rootDir </> "dumps"
           srcFileDir = dumpsDir </> subDir
       if append
         then do exists <- doesDirectoryExist dumpsDir
                 unless exists (createDirectoryIfMissing True dumpsDir)
                 return (dumpsDir </> (dumpId ++ ext))
         else do createDirectoryIfMissing True srcFileDir
                 CalendarTime _ _ _ hour min sec picoSec _ _ _ _ _ <- toCalendarTime ts
                 let msec = milliSecsFromPicoSecs picoSec
                     tsStr = printf "%02d-%02d-%02d-%03d" hour min sec msec
                     fileName = srcFileDir </> (filePrefix ++ "_" ++ tsStr ++ ext)
                 mFp <- findNonexistingFile _NUMBERED_DUMPS_ fileName
                 case mFp of
                   Just fp -> return fp
                   Nothing ->
                       do hPutStrLn stderr ("Giving up finding non existing file name based on "
                                            ++ fileName)
                          return fileName

dumpBytesIO :: BS.ByteString -> String -> String -> String -> IO (Maybe FilePath)
dumpBytesIO bytes subDir fileNamePart ext =
    do ts <- getClockTime
       cfg <- readIORef staticLogConfig
       case lc_rootDir cfg of
         Just rootDir ->
             do fname <- genericGetDumpFileName False rootDir subDir fileNamePart
                                                fileNamePart ext ts
                BS.writeFile fname bytes
                return (Just fname)
         Nothing ->
             do hPutStrLn stderr ("Cannot dump bytes because not root dir configured for logging")
                return Nothing

getTagHandles rootDir extraInfo targets =
    foldM addHandle ([], targets) (unTags (eli_tags extraInfo))
    where
      addHandle (handles, targets) t =
          case Map.lookup t (lt_tagHandleMap targets) of
            Just h -> return (h : handles, targets)
            Nothing ->
                do mh <- setupHandle rootDir ("tagged/" ++ t ++ ".log")
                   case mh of
                     Just h ->
                         let targets' = targets { lt_tagHandleMap =
                                                      Map.insert t h (lt_tagHandleMap targets) }
                         in return (h : handles, targets')
                     Nothing -> return (handles, targets)

ifLevelOk dynCfg src level extraInfo def action =
    case unTags (eli_tags extraInfo) of
      [] -> if {-# SCC "walkLevelMap/ifLevelOk/Logging" #-}
              walkLevelMap (lc_levelMap dynCfg) then action else return def
      l -> if any isTagLevelOk l then action else return def
    where walkLevelMap [] = level >= lc_defaultLevel dynCfg
          walkLevelMap ((prefix,minlevel):xs) =
              if {-# SCC "isPrefixOf/ifLevelOk/Logging" #-} prefix `List.isPrefixOf` src
                 then level >= minlevel
                 else walkLevelMap xs
          isTagLevelOk t =
              case lookup t (lc_enabledTags dynCfg) of
                Just taglevel -> level >= taglevel
                _ -> False

ifTimeOk :: ClockTime -> LogMessage -> LogTargets -> IO LogTargets -> IO (LogTargets, Bool)
ifTimeOk now logMsg def action =
    if lm_level logMsg < WARN && (now `diffClockTimes` (lm_time logMsg)) > _MAX_LOGMSG_DELAY_
       then return (def, True)
       else action >>= \x -> return (x, False)

getLoggingRootDir :: IO FilePath
getLoggingRootDir =
    do mRootDir <- readIORef rootDirVar
       case mRootDir of
         Just rd -> return rd
         Nothing -> safeError "Logging: Didn't call `setupLogging', yet."

getMainLogFile :: LogConfig -> IO FilePath
getMainLogFile cfg =
    do rd <- getLoggingRootDir
       return (rd </> (lc_mainTarget . lc_static) cfg)
