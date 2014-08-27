module Mgw.Util.Logging.LogFormatting (

    getLogMsgPrefix, adjustSourceFilePath, withLogId

) where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Misc
import Mgw.Util.Logging.Core

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Safe

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Concurrent
import System.Time
import Text.Printf
import qualified Data.List as List

_WITH_DATE_ = True

adjustSourceFilePath :: FilePath -> FilePath
adjustSourceFilePath = tailSafe . dropWhile (/='/') . drop 1 . dropWhile (/= '/')

withLogId :: String -> String -> String
withLogId logId msg = logId ++ ": " ++ msg

myFormatTimestamp :: ClockTime -> IO String
myFormatTimestamp = formatTimestamp _WITH_DATE_ (if _WITH_DATE_ then fmt1 else fmt2)
  where fmt1 = "%4d-%02d-%02d " ++ fmt2
        fmt2 = "%02d:%02d:%02d.%03d"

formatLogLevel :: LogLevel -> String
formatLogLevel level = {-# SCC "formatLogLevel/Logging" #-}
    case level of
      TRACE -> "TRACE"
      DEBUG -> "DEBUG"
      INFO  -> "INFO "
      NOTE  -> "NOTE "
      WARN  -> "WARN "
      ERROR -> "ERROR"
      ALERT -> "ALERT"

formatThreadId :: ThreadId -> Bool -> String
formatThreadId tid insideStm = {-# SCC "formatThreadId/Logging" #-}
    let s = formatTid tid
    in if insideStm then s ++ "(STM)" else s

formatTag :: ExtraLogInfo -> String
formatTag extraInfo =
    ' ' : (List.intercalate "," (unTags (eli_tags extraInfo)))

showLineNo :: Int -> String
showLineNo n | n > 0 = show n
             | otherwise = "?"

pprFileName :: String -> String
pprFileName = printf "%-20s" . takeWhile (/='.')

getLogMsgPrefix :: LogMessage -> Bool -> IO String
getLogMsgPrefix logMsg withNewline =
    do tsStr <- myFormatTimestamp (lm_time logMsg)
       let level = lm_level logMsg
           prefix =
               printf "[%s %-9s %s %s:%3s%s] %s"
               (formatLogLevel level)
               (formatThreadId (lm_threadId logMsg) (lm_insideStm logMsg))
               tsStr
               (pprFileName (lm_file logMsg))
               (showLineNo (lm_line logMsg))
               (formatTag (lm_extraInfo logMsg))
               (if withNewline then ("\n"::String) else "")
       return prefix
