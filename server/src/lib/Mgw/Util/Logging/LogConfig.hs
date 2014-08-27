{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, ScopedTypeVariables #-}
module Mgw.Util.Logging.LogConfig where

import Control.Monad (foldM)
import Data.Char (toLower, toUpper)
import Safe (readMay)

import qualified Data.List as List

import Test.Framework

import Mgw.Util.String (strip, splitWs)
import Mgw.Util.Logging.Core

updateStatic :: LogConfig -> (StaticLogConfig -> StaticLogConfig) -> LogConfig
updateStatic cfg f = cfg { lc_static = f (lc_static cfg) }

updateDynamic :: LogConfig -> (DynamicLogConfig -> DynamicLogConfig) -> LogConfig
updateDynamic cfg f = cfg { lc_dynamic = f (lc_dynamic cfg) }

-- | The log priorities are given by space-separated strings of the
--   form "topic:level".
updateLogLevels :: Monad m => LogConfig -> String -> m LogConfig
updateLogLevels cfg str =
    do (defs, levels) <- updateTopicMapping parseLogLevel "log level" str Nothing
       let dynCfg = lc_dynamic cfg
           def = case defs of
                   (x:_) -> x
                   [] -> lc_defaultLevel dynCfg
           levels' = foldl mergeTopicLevels levels (lc_levelMap dynCfg)
           dynCfg' = dynCfg { lc_defaultLevel = def, lc_levelMap = levels' }
       return $ cfg { lc_dynamic = dynCfg' }

mergeTopicLevels newList x@(oldTopic, _oldLevel) =
    case List.lookup oldTopic newList of
      Nothing -> x : newList
      Just _ -> newList

parseLogLevel :: Monad m => String -> m LogLevel
parseLogLevel level =
    case readMay $ map toUpper level of
      Just p -> return p
      _ -> fail $ "Failed to parse log-level `" ++ level ++ "'!"

updateTopicMapping :: forall a m . Monad m =>
                      (String -> m a)
                   -> String
                   -> String
                   -> Maybe a
                   -> m ([a], [(String, a)])
updateTopicMapping parseFun kind str mDefault =
    do (xs, ys) <- foldM update ([], []) (splitWs str)
       return (reverse xs, reverse ys)
    where
      update :: ([a], [(String, a)]) -> String -> m ([a], [(String, a)])
      update (defs, list) str =
              case List.break (== ':') str of
                (topic, []) ->
                    case mDefault of
                      Just def -> return (defs, (topic, def) : list)
                      Nothing -> fail $ "empty " ++ kind ++ " for " ++ topic
                (topic', _:xStr) ->
                    do x <- parseFun (strip xStr)
                       let topic = strip topic'
                       if map toLower topic == "default"
                          then return (x : defs, list)
                          else return (defs, (topic, x) : list)

updateProgName :: Monad m => LogConfig -> String -> m LogConfig
updateProgName cfg str = return $ updateStatic cfg $ \statCfg -> statCfg { lc_progName = str}

updateRootDir :: Monad m => LogConfig -> FilePath -> m LogConfig
updateRootDir cfg str = return $ updateStatic cfg $ \statCfg -> statCfg { lc_rootDir = Just str }

-- | The enabled log tags are given by space-separated strings
updateEnabledLogTags :: Monad m => LogConfig -> String -> m LogConfig
updateEnabledLogTags cfg str =
    do (_defs, levels) <- updateTopicMapping parseLogLevel "log tag" str (Just def)
       return $ updateDynamic cfg
              $ \dynCfg -> dynCfg { lc_enabledTags =
                                      foldl mergeTopicLevels levels (lc_enabledTags dynCfg) }
    where
      def = (lc_defaultLevel . lc_dynamic) cfg

-- | The log targets are given by space-separated strings of the
--   form "topic:file".
updateLogTargets :: Monad m => LogConfig -> String -> m LogConfig
updateLogTargets cfg str =
    do (defs, targets) <- updateTopicMapping return "log target" str Nothing
       return $ updateStatic cfg $
                  \statCfg -> statCfg { lc_defaultTargets = lc_defaultTargets statCfg ++ defs
                                      , lc_targets = lc_targets statCfg ++ targets }

mkLogConfig :: Monad m => LogConfig -> String -> Maybe FilePath
            -> Maybe String -> Maybe String -> Maybe String -> m LogConfig
mkLogConfig cfg progName mRootDir mLevels mEnabledTags mLogTargets =
    do cfg1 <- maybe (return cfg) (updateRootDir cfg) mRootDir
       cfg2 <- maybe (return cfg1) (updateLogLevels cfg1) mLevels
       cfg3 <- maybe (return cfg2) (updateEnabledLogTags cfg2) mEnabledTags
       cfg4 <- maybe (return cfg3) (updateLogTargets cfg3) mLogTargets
       cfg5 <- updateProgName cfg4 progName
       return cfg5


test_mkLogConfig =
    do let cfg = LogConfig {
                   lc_static = StaticLogConfig { lc_progName = "prog"
                                               , lc_targets = [("blah", "blah.log")]
                                               , lc_mainTarget = "main.log"
                                               , lc_defaultTargets = ["def"]
                                               , lc_rootDir = Just "blub" }
                 , lc_dynamic = DynamicLogConfig { lc_levelMap = [("foo", DEBUG), ("bar", INFO)]
                                                 , lc_enabledTags = [("t1", INFO), ("t2", INFO)]
                                                 , lc_defaultLevel = INFO }
                 }
       cfg' <- mkLogConfig cfg "prog" (Just "blah") (Just "default:error bar:WARN egg:TRACE")
                           (Just "t2 t3")
                           (Just "default:anotherDefault.log blah:another-blah.log blub:blub.log")
       assertEqual (LogConfig {
                      lc_static =
                          StaticLogConfig { lc_progName = "prog"
                                          , lc_mainTarget = "main.log"
                                          , lc_defaultTargets = ["def", "anotherDefault.log"]
                                          , lc_targets = [ ("blah","blah.log")
                                                         , ("blah","another-blah.log")
                                                         , ("blub","blub.log")]
                                          , lc_rootDir = Just "blah" }
                    , lc_dynamic = DynamicLogConfig { lc_levelMap = [ ("foo",DEBUG)
                                                                    , ("bar",WARN)
                                                                    , ("egg",TRACE)]
                                                    , lc_defaultLevel = ERROR
                                                    , lc_enabledTags = [ ("t1", INFO)
                                                                       , ("t2",ERROR)
                                                                       , ("t3",ERROR)]
                                                    }
                    })
                   cfg'
