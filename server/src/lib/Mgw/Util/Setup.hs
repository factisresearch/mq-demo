{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.Setup
    ( setupLoggingForGhci, setupDynConfigFromServiceCfg, setupDynConfigIfNotDone
    , setupDynConfig
    , runMain
      -- reexports
    , setupLoggingWithConfig
    )
where

#include "src/macros.h"

import Mgw.Util.Config
import Mgw.Util.DynConfig
import Mgw.Util.Signals
import Mgw.Util.ExitAction (withExitActions)
import Mgw.Util.Exception (catch)
import Mgw.Util.ThreadActivity
import Mgw.Util.Logging
import Mgw.Util.Logging.LogWriter
import Mgw.Util.Logging.LogConfig

import Control.Exception (SomeException)
import Control.Monad (unless, when)

import System.Environment (getArgs)
import qualified System.IO

setupLoggingForGhci :: String -> IO ()
setupLoggingForGhci levelStr =
    do cfg <- updateLogLevels defaultLogConfig levelStr
       setupDynConfigForGhci
       setupLoggingWithConfig cfg

setupDynConfigIfNotDone :: AbstractDynConfig
                        -> (Maybe (IO (AbstractDynConfig, LogConfig)))
                        -> IO Bool
setupDynConfigIfNotDone dynCfg rereadAction =
    do ok <- setDynConfig dynCfg
       when ok $ installSignalHandler sigHUP (Catch $ rereadDynConfig rereadAction)
       return ok

setupDynConfigForGhci :: IO ()
setupDynConfigForGhci =
    do _ <- setupDynConfigIfNotDone defaultDynConfig Nothing
       return ()

setupDynConfig :: AbstractDynConfig -> (Maybe (IO (AbstractDynConfig, LogConfig))) -> IO ()
setupDynConfig dynCfg rereadAction =
    do ok <- setupDynConfigIfNotDone dynCfg rereadAction
       unless ok $ fail "Dynamic configuration already initialized"

rereadDynConfig :: (Maybe (IO (AbstractDynConfig, LogConfig))) -> IO ()
rereadDynConfig mRereadConfig =
    do logInfoOnStderr ("caught SIGHUP")
       logThreadActivities
       case mRereadConfig of
         Nothing ->
             do logInfoOnStderr "don't know how to reread config"
         Just rereadConfig ->
             do logInfoOnStderr "reloading configuration..."
                (dynCfg, logCfg) <- rereadConfig
                let dynLogCfg = lc_dynamic logCfg
                logInfoOnStderr ("done reloading configuration, new dynamic logging " ++
                                 "configuration:\n" ++ show dynLogCfg ++
                                 "\nnew dynamic configuration:\n" ++
                                 show dynCfg)
                setDynamicLogConfig dynLogCfg
                _ <- setDynConfig dynCfg
                logDebugWithTag "dyncfg" ("Help for dynamic configuration values:\n" ++
                                          dynConfigHelp)
                return ()
             `catch` (\(e::SomeException) ->
                          logErrorOnStderr ("Error reloading configuration: " ++ show e))

setupDynConfigFromServiceCfg :: ServiceCfg -> (Maybe (IO ServiceCfg)) -> IO ()
setupDynConfigFromServiceCfg svcCfg mf =
    setupDynConfig (svcCfg_dynCfg svcCfg) (fmap (\f -> do c <- f
                                                          return (svcCfg_dynCfg c, svcCfg_logCfg c))
                                                mf)

runMain optDefs defOpts main =
    withExitActions $ withConfigNameAndDir $ \cfgName mSearchDir ->
    do (warpCfg, esConfig) <- getConfig cfgName mSearchDir
       let logCfg = svcCfg_logCfg warpCfg
       setupDynConfigFromServiceCfg warpCfg (Just (do (cfg, _) <- getConfig cfgName mSearchDir
                                                      return cfg))
       setupLoggingWithConfig logCfg
       main warpCfg esConfig
    where
      getConfig cfgName mSearchDir =
          do args <- getArgs
             (cfg, [warpCfg']) <- getConfigWithSections mSearchDir [cfgName]
             (opts, warpCfg) <- updateSvcCfgWithArgs optDefs (restArgsHelp, parseRest)
                                defOpts cfg warpCfg' args
             return (warpCfg, opts)
          where
            restArgsHelp = ""
            parseRest _args s = return (Right s)
