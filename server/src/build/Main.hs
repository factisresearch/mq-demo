import Logging
import Types
import Rules
import BuildConfig

import Development.Shake hiding (getDirectoryContents)

import System.Console.GetOpt

import Safe

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.FilePath

import Control.Monad

data MyOptions
    = MyOptions
      { opts_shakeVerbosity :: Verbosity
      , opts_verbose :: Bool
      , opts_debug :: Bool
      , opts_quiet :: Bool
      , opts_way :: String
      , opts_deterministic :: Bool
      , opts_fakeGhc :: Bool
      , opts_jobs :: Int
      , opts_shakeProfile :: Bool
      , opts_listTargets :: Bool
      , opts_help :: Bool
      }

defaultOptions :: MyOptions
defaultOptions =
    MyOptions
      { opts_shakeVerbosity = Quiet
      , opts_verbose = False
      , opts_debug = False
      , opts_quiet = False
      , opts_way = ""
      , opts_shakeProfile = False
      , opts_jobs = defaultNumberOfJobs
      , opts_deterministic = False
      , opts_fakeGhc = False
      , opts_listTargets = False
      , opts_help = False
      }

defaultNumberOfJobs :: Int
defaultNumberOfJobs = 4

shakeProfileFile :: FilePath
shakeProfileFile = "shake-profile.html"

options :: [OptDescr (MyOptions -> MyOptions)]
options =
     [ Option ['l'] ["list-targets"]
         (NoArg (\opt -> opt { opts_listTargets = True }))
         "List common targets"
     , Option ['f'] ["fast"]
         (NoArg (\opt -> opt { opts_way = "f" }))
         "Build the fast way (but result is slower)"
     , Option ['p'] ["prof"]
         (NoArg (\opt -> opt { opts_way = "p" }))
         "Build profiling code"
     , Option [] ["way"]
         (ReqArg (\arg opt -> opt { opts_way = checkValidWay arg }) "WAY")
         ("Way to compile, one of " ++ List.intercalate ", " allWays)
     , Option ['j'] ["jobs"]
         (ReqArg (\arg opt -> opt { opts_jobs = readNote "invalid number of jobs" arg }) "N")
         ("Number of parallel jobs (default: " ++ show defaultNumberOfJobs ++ ")")
     , Option [] ["deterministic"]
         (NoArg (\opt -> opt { opts_deterministic = True }))
         "Run deterministic build, implies '-j 1'"
     , Option ['v'] ["verbose"]
         (NoArg (\opt -> opt { opts_verbose = True }))
         "Verbose output"
     , Option [] ["debug"]
         (NoArg (\opt -> opt { opts_debug = True }))
         "Debugging output"
     , Option [] ["quiet"]
         (NoArg (\opt -> opt { opts_quiet = True }))
         "Be quiet"
     , Option [] ["fake-ghc"]
         (NoArg (\opt -> opt { opts_fakeGhc = True }))
         "Fake GHC by only touching output files"
     , Option [] ["shake-profile"]
         (NoArg (\opt -> opt { opts_shakeProfile = True }))
         ("Generate profiling report in " ++ shakeProfileFile)
     , Option [] ["shake-silent"]
         (NoArg (\opt -> opt { opts_shakeVerbosity = Silent }))
         "Don't print any shake messages"
     , Option [] ["shake-quiet"]
         (NoArg (\opt -> opt { opts_shakeVerbosity = Quiet }))
         "Only print essential shake messages (typically errors)"
     , Option [] ["shake-normal"]
         (NoArg (\opt -> opt { opts_shakeVerbosity = Normal }))
         "Print normal shake messages (typically errors and warnings)."
     , Option [] ["shake-loud"]
         (NoArg (\opt -> opt { opts_shakeVerbosity = Loud }))
         "Print lots of shake messages (typically errors, warnings and status updates)"
     , Option [] ["shake-diagnostic"]
         (NoArg (\opt -> opt { opts_shakeVerbosity = Diagnostic }))
         "Print shake messages for virtually everything (for debugging the build system)"
     , Option ['h'] ["help"]
         (NoArg (\opt -> opt { opts_help = True }))
         "Print this help message"
     ]

parseArgs :: [String] -> IO (MyOptions, [String])
parseArgs argv =
    case getOpt Permute options argv of
      (fs, restArgs, []) ->
          let opts = foldl (flip id) defaultOptions fs
          in if opts_help opts then usage [] else return (opts, restArgs)
      (_, _, errs) -> usage errs
    where
      header = "Usage: ./shake [OPTION...] TARGET...\n"
      usage errs =
          do putStrLn (concat errs ++ usageInfo header options)
             exitWith (ExitFailure 1)

-- The version number of your build rules. Increment the version number to
-- force a complete rebuild, such as when making significant changes to
-- the rules that require a wipe. The version number should be set in the
-- source code, and not passed on the command line.
myShakeVersion :: Int
myShakeVersion = 1

checkValidWay way =
    if way `elem` allWays then way else error ("Unknown way: " ++ show way)

main :: IO ()
main =
    do args <- getArgs
       (opts, targets) <- parseArgs args
       let logLevel = if opts_debug opts
                         then DEBUG
                         else if opts_verbose opts
                                 then INFO
                                 else if opts_quiet opts then WARN else NOTE
       setLogLevel logLevel
       aliases <- targetAliases cfg (opts_way opts)
       let aliasesNoDoc = map (\(k,_,v) -> (k, v)) aliases
       when (opts_listTargets opts) $
            do documentTargets (map (\(k,d,_) -> (k,d)) aliases)
               exitWith ExitSuccess
       when (null targets) $
            do hPutStrLn stderr "No targets given, no work todo ;-)"
               exitWith ExitSuccess
       let cleanedTargets = filter (\t -> not (t `elem` cleanTargets)) targets
       noteIO ("Starting build, targets: " ++
               if null cleanedTargets then "-" else List.intercalate ", " cleanedTargets)
       linkRes <- newResource "ghc-link" 1
       let buildArgs = BuildArgs { ba_linkResource = linkRes
                                 , ba_fakeGhc = opts_fakeGhc opts
                                 }
       let resolvedTargets = concatMap (\t -> fromMaybe [t] (List.lookup t aliasesNoDoc)) cleanedTargets
       noteIO ("Resolved targets: " ++ show resolvedTargets)
       shake (mkShakeOptions opts) $
             do want resolvedTargets
                buildRules cfg buildArgs
    where
      cleanTargets = ["clean", "dist-clean"]
      documentTargets l =
          flip mapM (l ++ [("clean", Just "Remove all build artifacts")
                          ,("dist-clean", Just ("Remove all build artifacts, including the " ++
                                                "compiled build system itself"))]) $ \(t,md) ->
               do putStrLn (t ++ (fromMaybe "" (fmap (\d -> " (" ++ d ++ ")") md)))
      mkShakeOptions opts =
          shakeOptions { shakeVerbosity = opts_shakeVerbosity opts
                       , shakeVersion = myShakeVersion
                       , shakeReport = if opts_shakeProfile opts
                                          then Just shakeProfileFile
                                          else Nothing
                       , shakeDeterministic = opts_deterministic opts
                       , shakeThreads = if opts_deterministic opts then 1 else opts_jobs opts
                       , shakeLint = True
                       , shakeFiles = bc_outDir cfg </> ".shake"
                       }
      cfg = myBuildCfg
