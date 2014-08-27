module Mgw.Util.Logging.Core
    ( LineNo, Tags, DumpId, LogMessage(..), LogTargets(..), ExtraLogInfo(..), DumpMode(..)
    , LogConfig(..), StaticLogConfig(..), DynamicLogConfig(..), LogLevel(..), LogHandle(..)
    , LogMonad(..), LogFun
    , _LOGQUEUE_, _MINLEVEL_, rootDirVar
    , hsLoggerPriority, logLevelFromHsLoggerPriority
    , defaultLogConfig, defaultDynamicLogConfig, defaultStaticLogConfig
    , interactiveLogConfig
    , mkTag, mkDump, mkTagDump, noExtraLogInfo, tags, unTags, noTags
    )
where

import Data.IORef (IORef, newIORef)
import Data.Sequence (Seq)
import Data.String

import Control.Concurrent (ThreadId)

import System.Time (ClockTime)
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)

import qualified System.Log as Log
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

data LogLevel = TRACE | DEBUG | INFO | NOTE | WARN | ERROR | ALERT
              deriving (Show, Read, Eq, Ord, Bounded, Enum)

hsLoggerPriority :: LogLevel -> Log.Priority
hsLoggerPriority level =
    case level of
      TRACE -> Log.DEBUG
      DEBUG -> Log.INFO
      INFO -> Log.NOTICE
      NOTE -> Log.NOTICE
      WARN -> Log.WARNING
      ERROR -> Log.ERROR
      ALERT -> Log.ALERT

logLevelFromHsLoggerPriority :: Log.Priority -> LogLevel
logLevelFromHsLoggerPriority prio =
    case prio of
      Log.DEBUG -> TRACE
      Log.INFO -> DEBUG
      Log.NOTICE -> INFO
      Log.WARNING -> WARN
      Log.ERROR -> ERROR
      Log.CRITICAL -> ALERT
      Log.ALERT -> ALERT
      Log.EMERGENCY -> ALERT

type LineNo = Int
type Tag = String
type DumpId = String

newtype Tags = Tags { unTags :: [String] }

instance IsString Tags where
    fromString s = Tags [s]

tags :: [Tag] -> Tags
tags = Tags

noTags :: Tags
noTags = Tags []

data ExtraLogInfo
    = ExtraLogInfo
    { eli_tags :: Tags
    , eli_dumpId :: Maybe ({- base file name -} DumpId, {- append? -} DumpMode)
    , eli_ignoreStderr :: Bool
    }

data DumpMode
    = DumpOverwrite
    | DumpAppend
    | DumpAppendWithPrefix
    deriving (Read, Show, Eq)

noExtraLogInfo :: ExtraLogInfo
noExtraLogInfo = ExtraLogInfo (Tags []) Nothing False

mkTag :: Tags -> ExtraLogInfo
mkTag x = ExtraLogInfo x Nothing False

mkDump :: DumpId -> DumpMode -> ExtraLogInfo
mkDump dumpId append = ExtraLogInfo (Tags []) (Just (dumpId, append)) False

mkTagDump :: Tags -> DumpId -> DumpMode -> ExtraLogInfo
mkTagDump x dumpId append = ExtraLogInfo x (Just (dumpId, append)) False

data LogMessage
    = LogMessage
    { lm_time      :: !ClockTime
    , lm_threadId  :: !ThreadId
    , lm_insideStm :: !Bool
    , lm_file      :: !FilePath
    , lm_line      :: !LineNo
    , lm_level     :: !LogLevel
    , lm_extraInfo :: !ExtraLogInfo
    , lm_msg       :: Either String BSL.ByteString
    }

data LogHandle
    = LogHandle
    { lh_handle   :: Handle
    , lh_colors   :: Bool
    , lh_beep     :: Bool
    , lh_isStderr :: Bool
    }
    deriving (Eq, Show)

data LogTargets
    = LogTargets
    { lt_defaultHandles  :: [LogHandle]
    , lt_specificHandles :: [(String, LogHandle)]
    , lt_tagHandleMap    :: Map.Map String LogHandle }

data StaticLogConfig
    = StaticLogConfig
      { lc_progName       :: String
      , lc_targets        :: [(String, String)]
      , lc_mainTarget     :: String
      , lc_defaultTargets :: [String]
      , lc_rootDir        :: Maybe FilePath }
    deriving (Eq, Show)

data DynamicLogConfig
    = DynamicLogConfig
      { lc_levelMap       :: [(String, LogLevel)]
      , lc_defaultLevel   :: LogLevel
      , lc_enabledTags    :: [(String, LogLevel)] }
    deriving (Eq, Show)

data LogConfig
    = LogConfig
      { lc_static   :: StaticLogConfig
      , lc_dynamic  :: DynamicLogConfig }
    deriving (Eq, Show)


type LogFun m = FilePath
              -> LineNo
              -> LogLevel
              -> ExtraLogInfo
              -> Either String BSL.ByteString
              -> m ()

class Monad m => LogMonad m where
    doLog :: LogFun m

_LOGQUEUE_ :: IORef (Bool, Seq LogMessage)
_LOGQUEUE_ = unsafePerformIO (newIORef (False, Seq.empty))
{-# NOINLINE _LOGQUEUE_ #-}

-- minimal loglevel for untagged messages
_MINLEVEL_ :: IORef LogLevel
_MINLEVEL_ = unsafePerformIO (newIORef DEBUG)
{-# NOINLINE _MINLEVEL_ #-}

rootDirVar :: IORef (Maybe FilePath)
rootDirVar = unsafePerformIO (newIORef Nothing)
{-# NOINLINE rootDirVar #-}

defaultStaticLogConfig :: StaticLogConfig
defaultStaticLogConfig = StaticLogConfig { lc_progName = ""
                                         , lc_targets = []
                                         , lc_mainTarget = "main.log"
                                         , lc_defaultTargets = ["stderr"]
                                         , lc_rootDir = Nothing }

defaultDynamicLogConfig :: DynamicLogConfig
defaultDynamicLogConfig = DynamicLogConfig { lc_levelMap = []
                                           , lc_defaultLevel = INFO
                                           , lc_enabledTags = [] }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig defaultStaticLogConfig defaultDynamicLogConfig

interactiveLogConfig :: LogConfig
interactiveLogConfig =
    LogConfig (defaultStaticLogConfig { lc_defaultTargets = [] })
              defaultDynamicLogConfig
