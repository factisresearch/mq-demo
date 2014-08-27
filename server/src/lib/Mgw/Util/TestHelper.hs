module Mgw.Util.TestHelper
    ( withLogging, withLoggingAndLevel, withLoggingAndLevelInteractive, parseArgs )
where

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Setup
import Mgw.Util.Logging
import Mgw.Util.DynConfig
import Mgw.Util.Config
import Mgw.Util.ExitAction

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import System.Console.GetOpt

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity (runIdentity)
import Data.List (isPrefixOf)
import System.Exit
import System.IO
import qualified System.Environment as Env

setup :: [String] -> LogLevel -> Bool -> IO [String]
setup args ll isInteractive =
    do let logargs = filter isLoggingOpt args
           otherargs = filter (not . isLoggingOpt) args
           opts = logLevelOpts updateLogging
           updateLogging f s = modify (\cfg -> runIdentity (f cfg s))
           optNames = concat [map (('-':).(:[])) ss ++ map ("--"++) ls
                              | Option ss ls _ _ <- opts]
           isLoggingOpt s = any (\x -> x `isPrefixOf` s) optNames
       cfg <-
           case getOpt RequireOrder opts logargs of
             (actions, _nonOpts, []) ->
                 let cfg = execState (sequence_ actions) defcfg
                 in return cfg
             (_, _, msgs) -> fail (show msgs)
       _ <- setupDynConfigIfNotDone defaultDynConfig Nothing
       setupLoggingWithConfig cfg
       return otherargs
    where
      staticLogConfig =
          if isInteractive
          then defaultStaticLogConfig { lc_defaultTargets = [] }
          else defaultStaticLogConfig
      defcfg = defaultLogConfig { lc_dynamic = defaultDynamicLogConfig { lc_defaultLevel = ll }
                                , lc_static = staticLogConfig }


withLogging :: [String] -> ([String] -> IO a) -> IO a
withLogging args =
    withLoggingAndLevel args WARN

withLoggingAndLevel :: [String] -> LogLevel -> ([String] -> IO a) -> IO a
withLoggingAndLevel args ll action =
    withExitActions (setup args ll False >>= action)

withLoggingAndLevelInteractive :: [String] -> LogLevel -> ([String] -> IO a) -> IO a
withLoggingAndLevelInteractive args ll action =
    withExitActions (setup args ll True >>= action)

parseArgs :: [OptDescr (StateT s IO ())]
          -> s
          -> [String]
          -> (String -> String)
          -> IO (s, [String])
parseArgs options defaultCfg args usageHeader =
    do progName <- Env.getProgName
       when ("-h" `elem` args || "--help" `elem` args) $ usage progName []
       case getOpt RequireOrder options args of
         (actions, nonOpts, []) ->
             do cfg <- execStateT (sequence_ actions) defaultCfg
                return (cfg, nonOpts)
         (_, _, msgs) -> usage progName msgs
    where
      usage progName msgs =
          do hPutStrLn stderr $ concat msgs ++ usageInfo (usageHeader progName) options
             exitWith (ExitFailure 127)
