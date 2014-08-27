{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import Logging

import Development.Shake
import Development.Shake.FilePath

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Hashable
import qualified Data.List as List
import Data.List.Split
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe

import System.Process
import System.Exit
import qualified System.Directory as Dir
import qualified System.IO.Temp as Temp
import System.Posix.Directory
import System.Posix.User (getEffectiveUserName)
import System.IO.Error
import System.IO

instance (Hashable a, Hashable b) => Hashable (Map a b) where
    hashWithSalt salt m = hashWithSalt salt (Map.toList m)

instance (Hashable a) => Hashable (Set a) where
    hashWithSalt salt m = hashWithSalt salt (Set.toList m)

data PackageId
    = PackageId
      { pi_baseName :: String
      , pi_version :: String
      , pi_hash :: String
      }
    deriving (Eq, Show)

parsePackageId :: String -> Maybe PackageId
parsePackageId pkgId =
    case reverse (splitOn "-" pkgId) of
      hash:version:rRest -> Just $ PackageId (List.intercalate "-" (reverse rRest))
                                             version
                                             hash
      _ -> Nothing

encodeBS :: String -> BS.ByteString
encodeBS = T.encodeUtf8 . T.pack

myReadFile :: FilePath -> Action String
myReadFile fp =
    do need [fp]
       debug ("Reading file " ++ fp)
       bs <- liftIO $ BS.readFile fp
       debug ("Finished reading file " ++ fp)
       case T.decodeUtf8' bs of
         Right t -> return $ T.unpack t
         Left err -> fail ("File " ++ fp ++ " is not valid UTF-8: " ++ show err)

myReadFileLines :: FilePath -> Action [String]
myReadFileLines fp =
    do s <- myReadFile fp
       return $ lines s

myWriteFile :: FilePath -> String -> Action ()
myWriteFile fp s =
    do debug ("Writing file " ++ fp)
       let bs = encodeBS s
       liftIO $ BS.writeFile fp bs
       debug ("Finished writing file " ++ fp)

myWriteFileLines :: FilePath -> [String] -> Action ()
myWriteFileLines fp l =
    do myWriteFile fp (unlines l)

myWriteFileChanged :: FilePath -> String -> Action ()
myWriteFileChanged fp s =
    do debug ("Writing file if changed " ++ fp)
       exists <- liftIO $ Dir.doesFileExist fp
       let bsNew = encodeBS s
       mNew <- if exists
                  then do bsOld <- liftIO $ BS.readFile fp
                          return $ if (bsNew /= bsOld) then Just bsNew else Nothing
                  else return (Just bsNew)
       case mNew of
         Nothing -> return ()
         Just bs -> liftIO $ BS.writeFile fp bs
       debug ("Finished writing file if changed " ++ fp ++ ", changes=" ++ show (isJust mNew))

myWriteFileLinesChanged :: FilePath -> [String] -> Action ()
myWriteFileLinesChanged fp l =
    do myWriteFileChanged fp (unlines l)

showKv :: [String] -> String
showKv l = List.intercalate ", " (loop l)
    where
      loop [] = []
      loop [x] = [x]
      loop (k:v:rest) = (k ++ "=" ++ v) : loop rest

throwUserErr :: String -> IO a
throwUserErr msg = throwIO $ mkIOError userErrorType msg Nothing Nothing

systemIO :: LogLevel -> String -> [String] -> IO ()
systemIO ll prog args =
    do let cmd = unwords $ prog : args
       liftIO $ doLog ll cmd
       res <- rawSystem prog args
       case res of
         ExitFailure i -> throwUserErr $ "System command failed (code " ++ show i ++ "):\n" ++ cmd
         ExitSuccess -> return ()

mySystem :: LogLevel -> FilePath -> [String] -> Action ()
mySystem ll prog args =
    do let cmd = unwords $ prog : args
       liftIO $ doLog ll cmd
       res <- traced (takeBaseName prog) $ rawSystem prog args
       case res of
         ExitFailure i -> error $ "System command failed (code " ++ show i ++ "):\n" ++ cmd
         ExitSuccess -> return ()

myGetDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
myGetDirectoryFiles dir pat =
    do files <- getDirectoryFiles dir pat
       return $ map (\f -> dir </> f) files

myWhenNotExists :: FilePath -> Action () -> Action ()
myWhenNotExists fp action =
    do b <- liftIO $ (||) <$> Dir.doesFileExist fp <*> Dir.doesDirectoryExist fp
       when (not b) action

type URL = String

myDownloadAndExtractZip ::
       String                 -- | name of the downloaded file
    -> URL                    -- | the URL to download
    -> [(FilePath, FilePath)] -- | a mapping of files to extract
                              -- | (src relative to archive root -> target dir)
    -> Action ()
myDownloadAndExtractZip name url mapping = liftIO $
    Temp.withSystemTempDirectory ("Shake-" ++ name) $ \baseDir ->
       do login <- getEffectiveUserName
          let cacheDir = "/tmp/DociCache-" ++ login
              archive = cacheDir </> name
              cmd =
                  concat
                  [ "mkdir -p " ++ cacheDir ++ ";"
                  , "cd " ++ cacheDir ++ ";"
                  , "wget -N " ++ url
                  ]
          systemIO NOTE "bash" ["-c", cmd]
          systemIO NOTE "unzip" [archive, "-d", baseDir]
          forM_ mapping $ \(relPath, dir) -> do
              do Dir.createDirectoryIfMissing True dir
                 systemIO NOTE "cp" [ "-r", baseDir </> relPath, dir ++ "/" ]
