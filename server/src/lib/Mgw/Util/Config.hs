{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables, CPP #-}
module Mgw.Util.Config
    ( Cfg(..), ServiceCfg(..), TcpServiceCfg(..), WebServiceCfg(..), SslConfig(..), OptDef
    , PersistenceCfg(..), mkDefaultPersistenceCfg
    , getUrl, getUrl', getWebPort, getTcpPort, readConfig, parseConfig, getConfig
    , getConfigWithSections, updateSvcCfgWithArgs, noRestArgs
    , getReferencedService, withConfigNameAndDir
    , getProtHostPortPrefix, logLevelOpts, getHomeDir, homeDir
    , htf_thisModulesTests
) where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (liftM, foldM)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.Writer (runWriterT, tell)
import Control.Monad.State (StateT, runStateT, execStateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Fix (mfix)

import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.List (partition, isPrefixOf)
import qualified Data.List as List

import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import System.Console.GetOpt (ArgOrder(..), ArgDescr(..), OptDescr(..), usageInfo, getOpt)

import Text.Regex.Posix
import qualified System.Environment as Env
import qualified System.Posix.User as User

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.String.Utils (replace)
import Safe (fromJustNote, readMay, headNote)

import qualified Data.ConfigFile as Cfg
import qualified Data.ConfigFile.Parser as Cfg

import Text.ParserCombinators.Parsec.Error as P
import Text.ParserCombinators.Parsec as P

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Misc (errorToMaybe, errorToDefault, errorToEither)
import Mgw.Util.Locations (searchFile)
import Mgw.Util.Logging
import Mgw.Util.Logging.LogConfig
import Mgw.Util.DynConfig
import Mgw.Util.Fail
import qualified Mgw.Util.Misc

----------------------------------------
-- TEST
----------------------------------------
import Test.Framework

_DEFAULTS_SECTION_ = "DEFAULTS"
_LOG_DIR_ENV_NAME_ = "MGW_LOG_DIR"

type OptDef s = OptDescr (StateT s IO ())

--data CustomCfg
--                             , svcCfg_pollIntervalSeconds :: Maybe Int
--

data ServiceCfg = ServiceCfg { svcCfg_name      :: String
                             , svcCfg_dynCfg    :: AbstractDynConfig
                             , svcCfg_logCfg    :: LogConfig
                             , svcCfg_perCfg    :: Maybe PersistenceCfg
                             , svcCfg_webCfg    :: Maybe WebServiceCfg
                             , svcCfg_tcpCfg    :: Maybe TcpServiceCfg
                             , svcCfg_testmode  :: Bool
                             , svcCfg_services  :: [(String, ServiceCfg)]
                             } deriving (Show)
emptyServiceCfg :: ServiceCfg
emptyServiceCfg =
    ServiceCfg "emptyServiceCfg" defaultDynConfig defaultLogConfig Nothing Nothing Nothing False []

data PersistenceCfg = PersistenceCfg { perCfg_directory :: FilePath
                                     , perCfg_mgwIdFile :: FilePath
                                     , perCfg_mgwDataFile :: FilePath
                                     , perCfg_dynDsDataDir :: FilePath
                                     , perCfg_backupsDir :: FilePath
                                     , perCfg_stableIdsFile :: FilePath
                                     }
                      deriving (Show)

mkDefaultPersistenceCfg :: FilePath -> PersistenceCfg
mkDefaultPersistenceCfg dir =
    PersistenceCfg { perCfg_directory = dir
                   , perCfg_mgwIdFile = dir </> "server_id"
                   , perCfg_mgwDataFile = dir </> "data"
                   , perCfg_dynDsDataDir = dir </> "dynds_data"
                   , perCfg_backupsDir = dir </> "backups"
                   , perCfg_stableIdsFile = dir </> "stable_ids"
                   }

data TcpServiceCfg = TcpServiceCfg { tcpCfg_port      :: Int
                                   , tcpCfg_host      :: String
                                   , tcpCfg_sslConfig :: Maybe SslConfig
                                   } deriving (Show)

data SslConfig = SslConfig { sslCfg_dir     :: Maybe FilePath
                           , sslCfg_stunnel :: Maybe FilePath
                           }
               deriving (Show)

data WebServiceCfg = WebServiceCfg { wsCfg_prot :: Maybe String
                                   , wsCfg_host :: Maybe String
                                   , wsCfg_port :: Maybe Int
                                   , wsCfg_prefix :: Maybe String
                                   , wsCfg_path :: Maybe String
                             }
                  deriving (Show)

emptyWebServiceCfg :: WebServiceCfg
emptyWebServiceCfg = WebServiceCfg Nothing Nothing Nothing Nothing Nothing

data Cfg = Cfg { cfg_services :: [(String, ServiceCfg)]
               , cfg_defaults :: (String, String, Int, String, String)
               } deriving (Show)

defaultCfg :: Cfg
defaultCfg = Cfg [] ("http", "localhost", 80, "", "")

type CfgParser = ErrorT Cfg.CPError

parseConfig :: [(String, String)] -> [String] -> (String -> String) -> Cfg.ConfigParser
            -> Either Cfg.CPError (Cfg, [ServiceCfg])
parseConfig env serviceNames repl cp =
    runIdentity $
    runErrorT $
    do prot <- liftM repl $ errorToDefault "http" $ Cfg.get cp "DEFAULTS" "web-protocol"
       host <- liftM repl $ errorToDefault "localhost" $ Cfg.get cp "DEFAULTS" "web-host"
       mport <- errorToMaybe $ Cfg.get cp "DEFAULTS" "web-port"
       port <-
           case mport of
             Just portstr -> parsePort portstr
             Nothing -> return 80
       prefix <- liftM repl $ errorToDefault "" $ Cfg.get cp "DEFAULTS" "web-prefix"
       path <- liftM repl $ errorToDefault "" $ Cfg.get cp "DEFAULTS" "web-path"
       let defaults = (prot,host,port,prefix,path)
           serviceSects = filter (/=_DEFAULTS_SECTION_) (Cfg.sections cp)
       serviceMap <- mfix $
                     \serviceMap ->
                         do services <- mapM (parseServiceCfg serviceMap) serviceSects
                            return (zip serviceSects services)
       services <- mapM (getService serviceMap) serviceNames
       return (Cfg serviceMap defaults, services)
    where
      consume section name = Cfg.get cp section name >>= \x -> tell [name] >> return x
      parseServiceCfg :: Monad m => [(String,ServiceCfg)] -> String -> CfgParser m ServiceCfg
      parseServiceCfg serviceMap section =
          do let parsePredefined =
                     do webSvc <- parseWebCfg section
                        tcpSvc <- parseTcpCfg section
                        logCfg <- parseLogCfg section
                        perCfg <- parsePerCfg section
                        dynCfg <- parseDynCfg section
                        testmode <- liftM (boolOfString . fromMaybe "no") $
                                    getWithDefault section False "testmode"
                        return (ServiceCfg section dynCfg logCfg perCfg webSvc tcpSvc testmode)
             (mkServiceCfg, usedItems) <- runWriterT parsePredefined
             allItems <- Cfg.items cp section
             let svcPrefix = "service-"
                 leftItems = filter ((`notElem` usedItems) . fst) allItems
                 (serviceItems, customItems) =
                     partition ((svcPrefix `isPrefixOf`) . map toLower . fst) leftItems
                 mapImpl (cfgName, svcName) =
                     ( map toLower $ drop (length svcPrefix) cfgName
                     , fromJustNote ("unable to find service " ++ svcName) $
                                    lookup svcName serviceMap)
                 logFun =
                     if null customItems
                        then id
                        else pureInfo ("Unhandled configuration items: " ++ show customItems)
             logFun $ return (mkServiceCfg $ map mapImpl serviceItems)
      parsePerCfg section =
          do mPerDir <- getWithDefault section True "persistencedir"
             return (fmap mkDefaultPersistenceCfg mPerDir)
      parseLogCfg section =
          do let getLogVal = getWithDefault section
             logDir <- case List.lookup _LOG_DIR_ENV_NAME_ env of
                         Just dir -> return (Just dir)
                         Nothing -> getLogVal True "logdir"
             logLevels <- getLogVal False "loglevels"
             logTags <- getLogVal False "logtags"
             logTargets <- getLogVal False "logtargets"
             mkLogConfig defaultLogConfig section logDir logLevels logTags logTargets
      parseTcpCfg section =
          do host <- liftM (fromMaybe "localhost") $ errorToMaybe (consume section "tcp-host")
             mportstr <- errorToMaybe $ consume section "tcp-port"
             mSslDir :: Maybe String <- getWithDefault section True "tcp-ssldir"
             mSslStunnel :: Maybe String <- getWithDefault section True "tcp-stunnel"
             mSslString :: Maybe String <- getWithDefault section False "tcp-ssl"
             let mSslCfg =
                     case mSslString of
                       Just ssl | boolOfString ssl ->
                                    Just $ SslConfig { sslCfg_dir = mSslDir
                                                     , sslCfg_stunnel = mSslStunnel
                                                     }
                       _ -> Nothing
             case mportstr of
               Just portstr ->
                   do port <- parsePort portstr
                      return (Just $ TcpServiceCfg port host mSslCfg)
               Nothing -> return Nothing
      parseWebCfg section =
          do mprot <- errorToMaybe (liftM repl $ consume section "web-protocol")
             mhost <- errorToMaybe (liftM repl $ consume section "web-host")
             mport' <- errorToMaybe (liftM repl $ consume section "web-port")
             mport <-
                 case mport' of
                   Nothing -> return Nothing
                   Just portstr -> liftM Just $ parsePort portstr
             mprefix <- errorToMaybe (liftM repl $ consume section "web-prefix")
             mpath <- errorToMaybe $ consume section "web-path"
             if isJust mprot || isJust mhost || isJust mport || isJust mprefix || isJust mpath
                then return (Just $ WebServiceCfg mprot mhost mport mprefix mpath)
                 else return Nothing
      parseDynCfg section =
          do res <- foldM setValue (Right defaultDynConfig) dynConfigSpecs
             case res of
               Right dynCfg -> return dynCfg
               Left errors -> (fail . unlines . reverse) errors
          where
            setValue fDynCfg spec =
                let key = dynCfgSpec_key spec
                in if Cfg.has_option cp section key
                      then do eithStrVal <- errorToEither $ liftM repl $ consume section key
                              case eithStrVal of
                                Left err -> safeError ("Cannot happen: " ++ show err)
                                Right strVal -> return $
                                    case fDynCfg of
                                      Right dynCfg ->
                                          case dynCfgSpec_setter spec dynCfg strVal of
                                            Ok x -> Right x
                                            Fail msg -> Left [msg]
                                      Left errors ->
                                          case dynCfgSpec_setter spec defaultDynConfig strVal of
                                            Ok _ -> Left errors
                                            Fail msg -> Left (msg : errors)
                      else return fDynCfg
      parsePort :: Monad m => String -> m Int
      parsePort portstr =
          case readMay portstr of
            Just port -> return port
            Nothing -> fail $ "Not a valid port number: " ++ portstr
      getService serviceMap svcName =
          case lookup svcName serviceMap of
            Just svc -> return svc
            Nothing -> fail ("Service `" ++ svcName ++ "' needed but not defined anywhere.")
      boolOfString s =
          map toLower s `elem` ["true", "1", "on", "yes"]
      getWithDefault section doRepl key =
          do let replFun = liftM (if doRepl then repl else id)
             x <- errorToMaybe (replFun $ consume section key)
             case x of
               Just y -> return $ Just y
               Nothing -> errorToMaybe (replFun $ Cfg.get cp "DEFAULTS" key)

getConfig mSearchDir =
    do (cfg, []) <- getConfigWithSections mSearchDir []
       return cfg

getConfigWithSections :: Maybe FilePath -> [String] -> IO (Cfg, [ServiceCfg])
getConfigWithSections mSearchDir sections =
    do env <- Env.getEnvironment
       readConfig mSearchDir "MobileGateway.ini" (Just "MobileGateway-local.ini")
                  (parseConfig env sections)

getUrl :: Cfg -> ServiceCfg -> String
getUrl = getUrl' True

getUrl' :: Bool -> Cfg -> ServiceCfg -> String
getUrl' omitPort80 cfg svcCfg =
    let (prot, host, port, prefix) = getProtHostPortPrefix cfg svcCfg
        portstr = if port == 80 && omitPort80 then "" else ':' : show port
    in prot ++ "://" ++ host ++ (if null prefix then portstr else portstr `slash` prefix)

getProtHostPortPrefix :: Cfg -> ServiceCfg -> (String, String, Int, String)
getProtHostPortPrefix (Cfg { cfg_defaults = (defProt,defHost,defPort,defPrefix,defPath)})
                      (ServiceCfg
                              { svcCfg_webCfg =
                                    Just (WebServiceCfg { wsCfg_prot = mprot, wsCfg_host = mhost
                                                        , wsCfg_prefix = mprefix, wsCfg_path = mpath
                                                        , wsCfg_port = mport })})
    = let prot = fromMaybe defProt mprot
          host = fromMaybe defHost mhost
          prefix = fromMaybe defPrefix mprefix
          path = fromMaybe defPath mpath
          port = fromMaybe defPort mport
      in (prot, host, port, prefix `slash` path)
getProtHostPortPrefix _ svcCfg =
    safeError $ "Configuration doesn't include web service configuration: " ++ show svcCfg

slash :: String -> String -> String
slash x "" = x
slash x y
    | take 1 (reverse x) == "/" = x ++ dropWhile (=='/') y
    | otherwise = x ++ "/" ++ dropWhile (=='/') y


getWebPort :: ServiceCfg -> Maybe Int
getWebPort svcCfg =
    do webCfg <- svcCfg_webCfg svcCfg
       wsCfg_port webCfg

getTcpPort :: ServiceCfg -> Maybe Int
getTcpPort svcCfg =
    do tcpCfg <- svcCfg_tcpCfg svcCfg
       return $ tcpCfg_port tcpCfg

-- == GENERAL FUNCTION ========================================================

homeDir :: FilePath
homeDir = unsafePerformIO getHomeDir

getHomeDir :: IO FilePath
getHomeDir =
    do uid <- User.getEffectiveUserID
       liftM User.homeDirectory (User.getUserEntryForID uid)

getUser :: IO String
getUser =
    do uid <- User.getEffectiveUserID
       liftM User.userName (User.getUserEntryForID uid)

instance Show Cfg.ConfigParser where
    show = Cfg.to_string

-- largely copied from Data.ConfigFile
myInterpolatingAccess predefines maxdepth cp s o =
    if maxdepth < 1
       then interError "maximum interpolation depth exceeded"
       else do x <- Cfg.simpleAccess cp s o
               case P.parse (Cfg.interpmain $ lookupfunc) (s ++ "/" ++ o) x of
                 Left y -> case safeHead (P.errorMessages y) of
                                P.Message z -> interError z
                                _ -> interError (show y)
                 Right y -> return y
    where
      lookupfunc o =
          case List.lookup o predefines of
            Just x -> Right x
            Nothing -> myInterpolatingAccess predefines (maxdepth - 1) cp s o
      interError x = throwError (Cfg.InterpolationError x, "interpolatingAccess")

readConfig mSearchDir confName mLocalConfName parseCfg =
    do eitherCfgFile <- searchFile (maybeToList mSearchDir) confName
       cfgFile <- case eitherCfgFile of
                    Right x -> return x
                    Left err -> fail err
       hPutStrLn stderr $ "Reading config file `"++cfgFile++"'..."
       env <- Env.getEnvironment
       let predefines =
               case List.lookup "DOCI_PORT_PREFIX" env of
                 Just x -> [("port-prefix", x)]
                 Nothing -> []
       let defaultCfg = Cfg.emptyCP { Cfg.accessfunc = myInterpolatingAccess predefines 10 }
       mcfg <-
           do mcfg <- Cfg.readfile defaultCfg cfgFile
              case (mcfg, mLocalConfName) of
                (Right cfg, Just confName') ->
                    do eitherCfgFile' <- searchFile (maybeToList mSearchDir) confName'
                       case eitherCfgFile' of
                         Right cfgFile' ->
                             do hPutStrLn stderr $ "Reading local config file `" ++ cfgFile'
                                                   ++ "'..."
                                Cfg.readfile cfg cfgFile'
                         Left _ -> return mcfg
                _ -> return mcfg
       let showErr ty arg src = fail ("Error parsing config file `" ++ cfgFile
                                      ++ "' at " ++ src ++ ":\n"
                                      ++ ty ++ ": " ++ arg)
       repls <- getReplacements
       case mcfg >>= parseCfg (applyRepls repls) of
         Left (Cfg.ParseError msg, src) -> showErr "ParseError" msg src
         Left (Cfg.NoSection sec, src) -> showErr "NoSection" sec src
         Left (Cfg.NoOption opt, src) -> showErr "NoOption" opt src
         Left (Cfg.SectionAlreadyExists msg, src) ->
             showErr "DuplicateSection" msg src
         Left (Cfg.OtherProblem msg, src) -> showErr "Error" msg src
         Left (Cfg.InterpolationError msg, src) -> showErr "InterpolationError" msg src
         Right cfg ->
             do hPutStrLn stderr $ "Got configuration."
                return cfg
    where
      applyRepls (home,user) = replace "$HOME" home . replace "$USER" user
      getReplacements =
          do home <- getHomeDir
             user <- getUser
             return (home,user)

getReferencedService :: ServiceCfg -> String -> IO ServiceCfg
getReferencedService sect@(ServiceCfg { svcCfg_services = serviceMap }) svcName =
    case lookup (map toLower svcName) serviceMap of
      Just svc -> return svc
      Nothing -> fail ("Service `" ++ svcCfg_name sect ++
                       "' doesn't have a service reference to `" ++ svcName ++
                       "'. Available service references: " ++ show (map fst serviceMap))

noRestArgs :: [String] -> s -> IO (Either String s)
noRestArgs args s =
    case args of
      [] -> return (Right s)
      _ -> return (Left "Didn't expect non-option arguments.")

_URL_REGEX_ :: String
_URL_REGEX_ = "([a-z]+)://(([^@]+)@)?([^@:/]+)(:([0-9]+))?(.*)"


logLevelOpts :: Monad m => ((LogConfig -> String -> m LogConfig) -> String -> a) -> [OptDescr a]
logLevelOpts updateLogging =
    [ (Option [] ["loglevels"] (ReqArg (updateLogging updateLogLevels) "LOGLEVELS")
       "override log-levels from config file")
    , (Option [] ["logtags"] (ReqArg (updateLogging updateEnabledLogTags) "LOGTAGS")
       "add additional log-tags")
    , (Option [] ["logtargets"] (ReqArg (updateLogging updateLogTargets) "LOGTARGETS")
       "override logtargets from config file")
    , (Option [] ["logdir"] (ReqArg (updateLogging updateRootDir) "LOGDIR")
       "override logdir from config file")
    ]

updateSvcCfgWithArgs :: forall s.
                        [OptDef s]
                     -> (String, [String] -> s -> IO (Either String s))
                     -> s
                     -> Cfg
                     -> ServiceCfg
                     -> [String]
                     -> IO (s, ServiceCfg)
updateSvcCfgWithArgs moreOpts (restArgsHelp, parseRestArgs) s cfg svcCfg args =
    do progName <- Env.getProgName
       case getOpt RequireOrder options args of
         (actions, nonOpts, []) ->
             do (svcCfg', s') <- runStateT (execStateT (sequence_ actions) svcCfg) s
                ms'' <- parseRestArgs nonOpts s'
                case ms'' of
                     Right s'' -> return (s'', svcCfg')
                     Left msg ->
                         let msgWithNl = if null msg then "" else msg ++ "\n"
                         in fail (msgWithNl ++ usageInfo (header progName) options)
         (_, _, msgs) -> safeError $ concat msgs ++ usageInfo (header progName) options
    where
      options :: [OptDescr (StateT ServiceCfg (StateT s IO) ())]
      options =
          [ (Option [] ["service"] (ReqArg updateService "SERVICENAME:SECTION")
             "override service reference from config file")
          , (Option [] ["testmode"] (NoArg $ modify (\s -> s { svcCfg_testmode = True }))
             "override testmode setting and activate it")
          ] ++ map mapOpt moreOpts ++ logLevelOpts updateLogging
      header progName = "Usage: " ++ progName ++ " [OPTION...] " ++ restArgsHelp ++
                        "\n\nOptions:" ++
                        "\n    --config-name=NAME" ++
                        "\n    --config-dir=DIR"
      updateService s =
          case List.span (/=':') s of
            (svcType, ':':svcInstance) ->
                case newSvcCfg of
                  Just svcCfg ->
                      let svcType' = map toLower svcType
                          upd svcs = (svcType', svcCfg) : filter ((/=svcType') . fst) svcs
                      in modify (\cfg -> cfg { svcCfg_services = upd (svcCfg_services cfg) })
                  Nothing ->
                      fail ("Service " ++ show svcInstance ++ " is not defined in config file "
                            ++ " and is no valid URL.")
                where
                  newSvcCfg =
                      case lookup svcInstance (cfg_services cfg) of
                        Just svcCfg -> Just svcCfg
                        Nothing
                          | (x,y,z,[prot,_,_au,_ho,_,port,path]) <- svcInstance =~ _URL_REGEX_
                            -> ([x,y,z] :: [String] {- just a type sig -}) `seq`
                               Just $
                               ServiceCfg
                               { svcCfg_name = svcType
                               , svcCfg_dynCfg = defaultDynConfig
                               , svcCfg_logCfg = defaultLogConfig
                               , svcCfg_perCfg = Nothing
                               , svcCfg_tcpCfg = Nothing
                               , svcCfg_testmode = False
                               , svcCfg_services = []
                               , svcCfg_webCfg =
                                   Just $
                                   WebServiceCfg
                                   { wsCfg_prot = Just prot
                                   , wsCfg_host = Nothing
                                   , wsCfg_port =
                                       if null port then Nothing else Just (safeRead port)
                                   , wsCfg_prefix = Nothing
                                   , wsCfg_path = Just path
                                   }
                               }
                          | otherwise -> Nothing
            _ -> fail "Invalid service argument.  Example use: --service DataServer:Warhol"
      updateLogging fun s =
          modify (\cfg -> cfg { svcCfg_logCfg = fun' (svcCfg_logCfg cfg) s })
          where
            fun' cfg s =
                case runIdentity (runErrorT (fun cfg s)) of
                  Right x -> x
                  Left err -> safeError err
      mapOpt (Option a b arg d) = Option a b (mapArg arg) d
      mapArg (NoArg a) = NoArg (lift a)
      mapArg (ReqArg f x) = ReqArg (\s -> lift (f s)) x
      mapArg (OptArg f x) = OptArg (\ms -> lift (f ms)) x

withConfigNameAndDir :: (String -> Maybe FilePath -> IO a) -> IO a
withConfigNameAndDir f =
    do args <- Env.getArgs
       (cfgName, args') <- configNameFromArgs args
       (mCfgDir, args'') <- mConfigDirDirFromArgs args'
       let msg = "Using configuration name `" ++ cfgName ++ "'" ++
                 (case mCfgDir of
                    Just d -> ", searching for configuration files in " ++ d
                    Nothing -> "")
       hPutStrLn stderr msg
       Env.withArgs args'' (f cfgName mCfgDir)
    where
      mConfigDirDirFromArgs :: [String] -> IO (Maybe FilePath, [String])
      mConfigDirDirFromArgs args =
          getValue args "config-dir" Just (return Nothing)
      configNameFromArgs args =
          getValue args "config-name" id configNameFromProg
      configNameFromProg =
          liftM (takeWhile (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])))
                Env.getProgName
      getValue args key f def =
          let prefix = "--" ++ key ++ "="
          in case List.find (prefix `List.isPrefixOf`) args of
               Just arg ->
                   let suf = drop (length prefix) arg
                   in if null suf
                         then do val <- def
                                 return (val, args)
                         else return (f suf, List.delete arg args)
               Nothing -> do val <- def
                             return (val, args)


test_url =
    do assertEqual "prot://host:42/prefix/path" (testUrl True defaults fullWebCfg)
       assertEqual "http://localhost" (testUrl True defaults emptyWebServiceCfg)
       assertEqual "http://localhost/" (testUrl True defaults
                                        emptyWebServiceCfg { wsCfg_prefix = Just "/" })
       assertEqual "http://localhost/" (testUrl True defaults
                                        emptyWebServiceCfg { wsCfg_prefix = Just "/"
                                                           , wsCfg_path = Just "/" })
    where
      testUrl b def w =
          getUrl' b (defaultCfg { cfg_defaults = def }) (emptyServiceCfg { svcCfg_webCfg = Just w })
      defaults = ("http", "localhost", 80, "", "")
      fullWebCfg =
          emptyWebServiceCfg
          { wsCfg_prot = Just "prot"
          , wsCfg_host = Just "host"
          , wsCfg_port = Just 42
          , wsCfg_prefix = Just "/prefix"
          , wsCfg_path = Just "/path"
          }
