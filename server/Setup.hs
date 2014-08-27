import Data.Maybe (maybeToList)
import Data.List (nub, (\\), intercalate, isSuffixOf)
import System.Process (system)
import System.Exit
import System.Time
import System.Directory (createDirectoryIfMissing)
import Control.Exception (throw)
import System.FilePath

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Text (disp)
import Distribution.PackageDescription hiding (Flag)
import Distribution.PackageDescription.Parse
import Distribution.Verbosity (normal)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils (findPackageDesc)

packagesOutputFile :: String
packagesOutputFile = ".packages"

outputPackages :: LocalBuildInfo -> IO ()
outputPackages lbi =
    do let packages =
               map (\(InstalledPackageId pid, _) -> pid) $ externalPackageDeps lbi
       writeFile packagesOutputFile (unlines packages)

-- generated .package file
myPostConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostConfHook _ _ descr lbi =
    do putStrLn "generating package info for shake..."
       outputPackages lbi

main = defaultMainWithHooks $ simpleUserHooks { postConf = myPostConfHook }
