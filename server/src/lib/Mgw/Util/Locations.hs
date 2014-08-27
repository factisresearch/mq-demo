{-# LANGUAGE CPP #-}
module Mgw.Util.Locations
    ( findFile, searchFile
    , findDirectory, searchDirectory
    , findDataFile, searchDataFile
    , findConfigDir, getConfigFilePath
    , findFromSearch
    , setupLogDirs, getLogDirs
    , findSslDir
    , findDataDir, findConfigsDir
    , getProgDir
    , _START_TIME_
) where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (mplus, liftM, when)

import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import System.FilePath ((</>), takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (createSymbolicLink, fileExist, removeLink)
import System.Time (ClockTime, getClockTime)
import System.Environment (getEnvironment)

import qualified System.FilePath as Fp
import qualified System.Directory as Dir
import qualified System.Posix.User as User

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Logging
import Mgw.Util.ProgName (getFullProgName)
import Mgw.Util.Misc (formatTimestamp)

--
-- Constants
--

_START_TIME_ :: ClockTime
_START_TIME_ = unsafePerformIO getClockTime
{-# NOINLINE _START_TIME_ #-}

_LOG_DIRS_ :: IORef (Maybe (FilePath, FilePath))
_LOG_DIRS_ = unsafePerformIO $ newIORef Nothing

--
-- Public functions
--

type SearchFun = [FilePath] -> FilePath -> IO (Either String FilePath)
type FindFun = [FilePath] -> FilePath -> IO FilePath

searchDataFile :: String -> IO (Either String FilePath)
searchDataFile name = searchFile [] ("data" </> name)

findDataFile :: String -> IO FilePath
findDataFile = findFromSearch (\_ -> searchDataFile) []

searchFile :: SearchFun
searchFile = searchFileOrDirectory File

findFile :: FindFun
findFile fp = findFromSearch searchFile fp

searchDirectory :: SearchFun
searchDirectory = searchFileOrDirectory Directory

findDirectory :: FindFun
findDirectory fp = findFromSearch searchDirectory fp

findSslDir :: Maybe FilePath -> IO FilePath
findSslDir mPath =
    let path = case mPath of
                 Nothing -> "ssl"
                 Just p -> p
    in findFromSearch (searchFileOrDirectory Directory) [] path

findDataDir :: IO FilePath
findDataDir = findFromSearch (searchFileOrDirectory Directory) [] "data"

findConfigsDir :: IO FilePath
findConfigsDir = findFromSearch (searchFileOrDirectory Directory) [] "configs"

findConfigDir :: IO FilePath
findConfigDir =
    do mr <- findRepo
       base <- case mr of
         Just x -> return x
         Nothing -> getProgDir
       let path = base </> "var-files" </> "etc"
       Dir.createDirectoryIfMissing True path
       return path

getConfigFilePath :: String -> IO FilePath
getConfigFilePath name =
    do dir <- findConfigDir
       return (dir </> name)

data FileOrDirectory = File | Directory
                     deriving (Eq, Show)

searchFileOrDirectory :: FileOrDirectory -> SearchFun
searchFileOrDirectory kind searchDirs fp
    | Fp.isAbsolute fp =
        do ex <- exists fp
           if ex
              then return $ Right fp
              else return $ Left (show kind ++ " with absolute path " ++ show fp ++
                                  " does not exist")
    | otherwise =
    do curDir <- Dir.getCurrentDirectory >>= Dir.canonicalizePath
       progDir <- getProgDir
       mRepoDir <- findRepo
       env <- getEnvironment
       let mHomeDir = List.lookup "HOME" env
           possibleLocs = List.nub (searchDirs ++ [curDir] ++ maybeToList mRepoDir
                                    ++ maybeToList mHomeDir ++ [progDir])
           loop locs =
               case locs of
                 [] -> return $ Left ("Did not find " ++ kindStr ++ " " ++ show fp ++
                                      " in any of the following locations: " ++
                                      List.intercalate ", " possibleLocs)
                 (x:xs) ->
                     do let y = x </> fp
                        ex <- exists y
                        if ex then return (Right y) else loop xs
       loop possibleLocs
    where
      kindStr = if kind == File then "file" else "directory"
      exists = if kind == File then Dir.doesFileExist else Dir.doesDirectoryExist

findFromSearch :: SearchFun -> FindFun
findFromSearch searchFun searchDirs fp =
    do ex <- searchFun searchDirs fp
       case ex of
         Left err -> fail $ "Didn't find " ++ show fp ++ ": " ++ err
         Right x -> return x

getLogDirs :: IO (FilePath, FilePath)
getLogDirs =
    do mLogDirs <- readIORef _LOG_DIRS_
       case mLogDirs of
         Nothing -> safeError "setupLogDirs has not yet been callled!"
         Just logDirs -> return logDirs

setupLogDirs :: Maybe String -> Maybe FilePath -> IO (FilePath, FilePath)
setupLogDirs mProgName mLogOrigin =
    do runDir <- formatTimestamp True "%4d-%02d-%02d/%02d-%02d-%02d-%03d" _START_TIME_
       userName <- User.getEffectiveUserName
       dir <- case mLogOrigin of
                Just s -> return s
                Nothing ->
                    do x <- findRepo
                       case x of
                         Just s -> return (s </> "logs")
                         Nothing -> return $ "/tmp" </> "DociGateway_" ++ userName
       realProgName <- getFullProgName
       let baseDir = dir </> fromMaybe realProgName mProgName
           rootDir = baseDir </> runDir
           symlinkName = baseDir </> "current"
       writeIORef _LOG_DIRS_ (Just (baseDir, rootDir))
       Dir.createDirectoryIfMissing True rootDir
       symlinkExists <- fileExist symlinkName
       when symlinkExists $ removeLink symlinkName
       createSymbolicLink runDir symlinkName
       return (baseDir, rootDir)

walkUpToFindFileInDir :: String -> FilePath -> IO (Maybe FilePath)
walkUpToFindFileInDir name fpath =
    do exists <- Dir.doesFileExist (fpath </> name)
       if exists
          then return (Just fpath)
          else if (fpath /= "" && fpath /= "/")
                  then do let (dirname, _filename) = Fp.splitFileName fpath
                          walkUpToFindFileInDir name (Fp.dropTrailingPathSeparator dirname)
                  else return Nothing

getProgDir :: IO FilePath
getProgDir = liftM takeDirectory getFullProgName >>= Dir.canonicalizePath

findRepo :: IO (Maybe FilePath)
findRepo =
    do curDir <- Dir.getCurrentDirectory >>= Dir.canonicalizePath
       progDir <- getProgDir
       let markerFile = ("."++reponame)
       mDir1 <- walkUpToFindFileInDir markerFile curDir
       mDir2 <- walkUpToFindFileInDir markerFile progDir
       let finalDir = mDir1 `mplus` mDir2
       when (finalDir == Nothing) $
            logError $ ("Base-Directory not found while searching for " ++ markerFile
                        ++ " starting from " ++ curDir ++ " and " ++ progDir ++ ".")
       return finalDir
    where reponame = "DociGateway"
