{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mgw.Util.DynConfig
    ( AbstractDynConfig, DynConfig(..), DynConfigItem, DynConfigSpec(..), HasDynCfg(..)
    , dynCfgItem_value, dynConfigSpecs, defaultDynConfig
    , unsafeGetDynConfig, dynConfigHelp, setDynConfig
    , StaticDynCfgT, runDefaultStaticDynCfgT, OpenUrlSpec(..)
) where

#include "src/macros.h"

#define ITEM(key, help, label, defaultVal, conv) label = DynConfigItem { \
      dynCfgItem_key = key \
    , dynCfgItem_value = defaultVal \
    , dynCfgItem_conv = conv \
    , dynCfgItem_setter = (\c x -> c { label = x }) \
    , dynCfgItem_help = help \
    }

#define MK_ITEM(label) label = mkItem (\c x -> c { label = x })

import Prelude

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Error (ErrorT, Error)
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT(..), runReaderT, asks)
import Control.Monad.Writer (WriterT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Exception (SomeException, catch)

import Data.Monoid (Monoid)

import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import System.IO.Unsafe (unsafePerformIO)
import Safe (readMay)

import Mgw.Util.TimeSpan
import Mgw.Util.Fail
import Mgw.Util.Logging
import Mgw.Util.Tx
import Mgw.Util.STM

data OpenUrlSpec
    = OpenUrlSpec
    { ous_device             :: Maybe T.Text
    , ous_maxVersion         :: T.Text
    , ous_title              :: T.Text
    , ous_message            :: T.Text
    , ous_okLabel            :: T.Text
    , ous_cancelLabel        :: T.Text
    , ous_cancelOk           :: Bool
    , ous_clear              :: Bool
    , ous_updateOnReconnect  :: Bool
    , ous_url                :: T.Text
    } deriving (Show, Eq, Ord)

-- How to add a new dynamic configuration value?
-- 1. Add a new field to the DynConfig record
-- 2. Set the new field in the defaultDynConfig value
-- 3. Complete dynConfigSpecs with the new field

data DynConfig = DynConfig {
      dynCfg_newsPollInterval :: DynConfigItem TimeSpan
    , dynCfg_maxIPadConnections :: DynConfigItem Int
    , dynCfg_ackSaveInterval :: DynConfigItem TimeSpan
    , dynCfg_waitTimeAfterWorldAddition :: DynConfigItem TimeSpan
    , dynCfg_blockedDevices :: DynConfigItem [String]
    , dynCfg_openUrlSpecs :: DynConfigItem [OpenUrlSpec]
    , dynCfg_ackTimeout :: DynConfigItem TimeSpan
    , dynCfg_maxUnackedMessages :: DynConfigItem Int
    , dynCfg_httpTimeout :: DynConfigItem TimeSpan
    , dynCfg_mobSyncGrep :: DynConfigItem String
    , dynCfg_mobSyncTimerInterval :: DynConfigItem TimeSpan
    , dynCfg_syncInfoGranularity :: DynConfigItem Int
    , dynCfg_mobSyncSendAtOnce :: DynConfigItem Int
    , dynCfg_maxDsRetryInterval :: DynConfigItem TimeSpan
    , dynCfg_logWriterDelay :: DynConfigItem TimeSpan
    , dynCfg_logWriterMaxDelay :: DynConfigItem TimeSpan
    }
    deriving (Show)

defaultDynConfig :: AbstractDynConfig
defaultDynConfig = AbstractDynConfig $ DynConfig {
                     MK_ITEM(dynCfg_newsPollInterval)
                       "news-poll-interval"
                       "The interval of the newswatch polling the DS."
                       (minutes 5) parseTimeSpan
                   , MK_ITEM(dynCfg_maxIPadConnections)
                       "max-ipad-connections"
                       ("The maximum number of simultaneous iPad connections. New connections " ++
                        "beyond this limit are blocked. If you set the value to a number " ++
                        "below the number of current connections, nothing happens, i.e. the " ++
                        "server does not close existing connections to reach the new limit.")
                        10000 readMay
                   , MK_ITEM(dynCfg_ackSaveInterval)
                       "ack-save-interval"
                       "The interval for saving acks to disk."
                       (minutes 60) parseTimeSpan
                   , MK_ITEM(dynCfg_waitTimeAfterWorldAddition)
                       "wait-time-after-world-addition"
                       "The time to wait after adding a new item to the source world."
                       zeroTime parseTimeSpan
                   , MK_ITEM(dynCfg_blockedDevices)
                       "blocked-devices"
                       ("The UDIDs of devices which are not allowed to connect. New " ++
                        "connections from such devices are refused. If a currently connected " ++
                        "iPad becomes blocked, the server closes the corresponding connection.")
                       [] (Just . words)
                   , MK_ITEM(dynCfg_openUrlSpecs)
                       "open-url-specs"
                       ("Specifications of what URL to open for which device. Syntax: " ++
                        "UDID|APP-VERSION|TITLE|MESSAGE|OK-LABEL|CANCEL-LABEL|CAN-CANCEL|" ++
                        "CLEAR_DATA|UPDATE_ON_RECON|URL " ++
                        "If either UDID is '*' or it matches the device Udid and the app version "++
                        "is smaller than APP-VERSION a dialog with the given TITLE, MESSAGE and "++
                        "an OK button with the OK-LABEL will be shown.  If CAN-CANCEL is 'true' "++
                        "a cancel button the CANCEL-LABEL will be shown. "++
                        "If the OK button is clicked URL will be opened." ++
                        "Multiple hash-separated specifications are supported.")
                       [] parseUrlSpecs
                   , MK_ITEM(dynCfg_ackTimeout)
                       "ack-timeout"
                       "The timeout after which messages without corresponding ACKs are resent."
                       (minutes 5) parseTimeSpan
                   , MK_ITEM(dynCfg_maxUnackedMessages)
                       "max-unacked-messages"
                       ("The server stops sending messages if there are more then " ++
                        "this number of messages without ACKs.")
                       30 readMay
                   , MK_ITEM(dynCfg_httpTimeout)
                       "http-timeout"
                       "The timeout for HTTP requests."
                       (seconds 90) parseTimeSpan
                   , MK_ITEM(dynCfg_mobSyncGrep)
                       "mob-sync-grep"
                       "Expression for searching MobSync data such as ack, needed, ..."
                       "" (Just . id)
                   , MK_ITEM(dynCfg_mobSyncTimerInterval)
                       "mob-sync-time-interval"
                       "Interval betwenn MobSync timer events"
                       (minutes 2) parseTimeSpan
                   , MK_ITEM(dynCfg_syncInfoGranularity)
                       "sync-info-granularity"
                       "Configures how often SyncInfos are sent"
                       500 readMay
                   , MK_ITEM(dynCfg_mobSyncSendAtOnce)
                       "mob-sync-send-at-once"
                       "Configures how many messages are sent \"at once\""
                       30 readMay
                   , MK_ITEM(dynCfg_maxDsRetryInterval)
                       "max-ds-retry-interval"
                       "Maximum interval between retries of DS requests"
                       (minutes 6) parseTimeSpan
                   , MK_ITEM(dynCfg_logWriterDelay)
                       "log-writer-delay"
                       "Delay between log writing loop steps"
                       (milliseconds 50) parseTimeSpan
                   , MK_ITEM(dynCfg_logWriterMaxDelay)
                       "log-writer-max-delay"
                       "Maximum allowed delay between logging call and writing of log message"
                       (seconds 10) parseTimeSpan
                   }

-- could generate this list with template haskell
dynConfigSpecs :: [DynConfigSpec]
dynConfigSpecs = [ mkSpec dynCfg_newsPollInterval
                 , mkSpec dynCfg_maxIPadConnections
                 , mkSpec dynCfg_ackSaveInterval
                 , mkSpec dynCfg_waitTimeAfterWorldAddition
                 , mkSpec dynCfg_blockedDevices
                 , mkSpec dynCfg_openUrlSpecs
                 , mkSpec dynCfg_ackTimeout
                 , mkSpec dynCfg_maxUnackedMessages
                 , mkSpec dynCfg_httpTimeout
                 , mkSpec dynCfg_mobSyncGrep
                 , mkSpec dynCfg_mobSyncTimerInterval
                 , mkSpec dynCfg_syncInfoGranularity
                 , mkSpec dynCfg_mobSyncSendAtOnce
                 , mkSpec dynCfg_maxDsRetryInterval
                 , mkSpec dynCfg_logWriterDelay
                 , mkSpec dynCfg_logWriterMaxDelay
                 ]

newtype AbstractDynConfig = AbstractDynConfig { unAbstractDynConfig :: DynConfig }
    deriving (Show)

unsafeGetDynConfig :: AbstractDynConfig -> DynConfig
unsafeGetDynConfig (AbstractDynConfig x) = x

data DynConfigItem a = DynConfigItem {
      dynCfgItem_key :: String
    , dynCfgItem_value :: a
    , dynCfgItem_conv :: String -> Maybe a
    , dynCfgItem_setter :: DynConfig -> DynConfigItem a -> DynConfig
    , dynCfgItem_help :: String
    }

mkItem :: (DynConfig -> DynConfigItem a -> DynConfig)
       -> String -> String -> a -> (String -> Maybe a) -> DynConfigItem a
mkItem setter key help val conv = DynConfigItem {
                                    dynCfgItem_key = key
                                  , dynCfgItem_value = val
                                  , dynCfgItem_conv = conv
                                  , dynCfgItem_setter = setter
                                  , dynCfgItem_help = help
                                  }

instance Show a => Show (DynConfigItem a) where
    showsPrec d item = showsPrec d (dynCfgItem_value item)

mkSpec :: (DynConfig -> DynConfigItem a)
       -> DynConfigSpec
mkSpec getter =
    let defaultItem = getter (unAbstractDynConfig defaultDynConfig)
        key = dynCfgItem_key defaultItem
        conv = dynCfgItem_conv defaultItem
        setter = dynCfgItem_setter defaultItem
        help = dynCfgItem_help defaultItem
    in DynConfigSpec {
             dynCfgSpec_setter = \adc str ->
                                 case conv str of
                                   Just x -> Ok $ AbstractDynConfig $
                                             setter (unAbstractDynConfig adc)
                                                    (defaultItem { dynCfgItem_value = x })
                                   Nothing -> Fail $ "Illegal value " ++ show str ++
                                                     " for dynamic config key " ++ key
           , dynCfgSpec_key = key
           , dynCfgSpec_help = help
           }

data DynConfigSpec = DynConfigSpec {
      dynCfgSpec_setter  :: AbstractDynConfig -> String -> Fail AbstractDynConfig
    , dynCfgSpec_key     :: String
    , dynCfgSpec_help    :: String
    }

dynConfigHelp :: String
dynConfigHelp =
    concatMap (\s -> "* " ++ dynCfgSpec_key s ++ ": " ++ dynCfgSpec_help s ++ "\n")
              dynConfigSpecs

data DynConfigState = DynConfigState {
      dynCfgState_current :: !DynConfig
      -- The "lastAccess" part stores the most recently accessed value of a configuration
      -- variable. This allows to output a log message whenever a different value
      -- is accessed
    , dynCfgState_lastAccess :: !DynConfig
    }

dynConfigState :: TVar (Maybe DynConfigState)
dynConfigState = unsafePerformIO (newTVarIO Nothing)
{-# NOINLINE dynConfigState #-}

dynConfigHooks :: TVar (Map String (IO ()))
dynConfigHooks = unsafePerformIO (newTVarIO Map.empty)
{-# NOINLINE dynConfigHooks #-}

class Monad m => HasDynCfg m where
    getDynValue :: (Show a, Eq a) => (DynConfig -> DynConfigItem a) -> m a
    registerDynCfgChangeHook :: String -> IO () -> m ()
    unregisterDynCfgChangeHook :: String -> m ()

instance HasDynCfg IO where
    getDynValue f = runTx $ getDynValueSTM f
    registerDynCfgChangeHook name io = runTx $ registerDynCfgChangeHookSTM name io
    unregisterDynCfgChangeHook name = runTx $ unregisterDynCfgChangeHookSTM name

instance (Error e, HasDynCfg m) => HasDynCfg (ErrorT e m) where
    getDynValue = lift . getDynValue
    registerDynCfgChangeHook name = lift . registerDynCfgChangeHook name
    unregisterDynCfgChangeHook = lift . unregisterDynCfgChangeHook

instance HasDynCfg m => HasDynCfg (StateT s m) where
    getDynValue = lift . getDynValue
    registerDynCfgChangeHook name = lift . registerDynCfgChangeHook name
    unregisterDynCfgChangeHook = lift . unregisterDynCfgChangeHook

instance (Monoid s, HasDynCfg m) => HasDynCfg (WriterT s m) where
    getDynValue = lift . getDynValue
    registerDynCfgChangeHook name = lift . registerDynCfgChangeHook name
    unregisterDynCfgChangeHook = lift . unregisterDynCfgChangeHook

instance HasDynCfg m => HasDynCfg (ReaderT s m) where
    getDynValue = lift . getDynValue
    registerDynCfgChangeHook name = lift . registerDynCfgChangeHook name
    unregisterDynCfgChangeHook = lift . unregisterDynCfgChangeHook

newtype StaticDynCfgT m a = StaticDynCfgT { _unStaticDynCfg :: ReaderT DynConfig m a }
    deriving (Functor, Applicative, Monad)

instance Monad m => HasDynCfg (StaticDynCfgT m) where
    getDynValue f = StaticDynCfgT (asks (dynCfgItem_value . f))
    registerDynCfgChangeHook _ _ = return ()
    unregisterDynCfgChangeHook _ = return ()

instance MonadTrans StaticDynCfgT where
    lift = StaticDynCfgT . lift

instance LogMonad m => LogMonad (StaticDynCfgT m) where
    doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg)

runDefaultStaticDynCfgT :: Monad m => StaticDynCfgT m a -> m a
runDefaultStaticDynCfgT (StaticDynCfgT r) = runReaderT r (unAbstractDynConfig defaultDynConfig)

getDynValueSTM :: (Show a, Eq a) => (DynConfig -> DynConfigItem a) -> STM a
getDynValueSTM f =
    do ms <- readTVar dynConfigState
       case ms of
         Nothing ->
             do let name = dynCfgItem_key (f (unAbstractDynConfig defaultDynConfig))
                fail ("Accessing `" ++ name ++ "' failed: Dynamic configuration not initialized.")
         Just s ->
             let cur = f (dynCfgState_current s)
                 curVal = dynCfgItem_value cur
                 lastVal = dynCfgItem_value $ f (dynCfgState_lastAccess s)
                 key = dynCfgItem_key cur
             in do when (curVal /= lastVal) $
                        do logInfo ("New value for dynamic configuration key " ++
                                    key ++ ": " ++ show curVal ++
                                    " (previous: " ++ show lastVal ++ ")")
                           let setter = dynCfgItem_setter cur
                               lastAccess' = setter (dynCfgState_lastAccess s) cur
                           writeTVar dynConfigState $
                                     Just (s { dynCfgState_lastAccess = lastAccess' })
                   return curVal

registerDynCfgChangeHookSTM :: String -> IO () -> STM ()
registerDynCfgChangeHookSTM name action =
    do old <- readTVar dynConfigHooks
       let !new = Map.insert name action old
       writeTVar dynConfigHooks new

unregisterDynCfgChangeHookSTM :: String -> STM ()
unregisterDynCfgChangeHookSTM name =
    do old <- readTVar dynConfigHooks
       let !new = Map.delete name old
       writeTVar dynConfigHooks new

setDynConfig :: AbstractDynConfig -> IO Bool
setDynConfig (AbstractDynConfig dynCfg) =
    do x <- runTx doUpdate
       hooks <- runTx $ readTVar dynConfigHooks
       mapM_ runHook (Map.assocs hooks)
       return x
    where
      doUpdate =
        do ms <- readTVar dynConfigState
           case ms of
             Nothing ->
                 do writeTVar dynConfigState $ Just $ DynConfigState dynCfg dynCfg
                    return True
             Just s ->
                 do writeTVar dynConfigState $ Just $ s { dynCfgState_current = dynCfg }
                    return False
      runHook (name, io) =
          io `catch` (\(e::SomeException) ->
                          logWarn ("Caught exception while running DynCfgChangeHook " ++ name ++
                                   ": " ++ show e))


parseUrlSpecs :: String -> Maybe [OpenUrlSpec]
parseUrlSpecs s = mapM parseSpec specs
    where
      specs = T.splitOn "#" (T.pack s)
      parseSpec t =
          case T.splitOn "|" t of
            (udidSpec : ver : title : msg : okLab : cancelLab : cancelOk' : clear' : rec' : url') ->
                let url = T.concat (List.intersperse " " url')
                    cancelOk = cancelOk' == "true"
                    clear = clear' == "true"
                    rec = rec' == "true"
                    udid = if udidSpec == "*" then Nothing else Just udidSpec
                in Just (OpenUrlSpec udid ver title msg okLab cancelLab cancelOk clear rec url)
            _ -> Nothing
