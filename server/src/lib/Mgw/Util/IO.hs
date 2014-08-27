{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Mgw.Util.IO
    ( hFlush, hClose, openFile, readFileStrict, readFileStrictBSL
    , withTempFd, fdWriteAll, fdWriteAllLazy
    , IO.Handle, IO.hSetBinaryMode, IO.stdout, IO.stderr
    , IO.IOMode(..), IO.hGetLine, IO.hSetEncoding, IO.utf8, IO.withFile
    , Posix.Fd, ioExcToFail
    , htf_thisModulesTests
    )
where

#include "src/macros.h"

import Prelude

import Mgw.Util.ThreadActivity
import Mgw.Util.Exception
import Mgw.Util.Logging
import Mgw.Util.STM
import Mgw.Util.Fail

import Test.Framework

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.Internal as BSLI

import qualified System.IO as IO
import System.IO.Error
import System.Directory
import System.FilePath

import qualified System.Posix.Types as Posix
import qualified System.Posix.IO as Posix
import qualified System.Posix.Internals as Posix

import qualified Control.Exception as Exc
import Control.Monad

import Data.Bits              ((.|.))
import Foreign.C
import Foreign.Ptr

hFlush = bracketThreadActivity "flushing handle" . IO.hFlush
hClose = bracketThreadActivity "closing handle" . IO.hClose

openFile :: FilePath -> IO.IOMode -> IO IO.Handle
openFile fp mode =
    modifyIOError modFun (IO.openFile fp mode)
    where
      modFun e =
          ioeSetErrorString e ("Error opening file " ++ fp ++ " in mode "
                               ++ show mode ++ ": " ++ show e)

readFileStrict :: FilePath -> IO String
readFileStrict fp =
    do h <- openFile fp IO.ReadMode
       s <- IO.hGetContents h
       _ <- Exc.evaluate (length s)
       hClose h
       return s

readFileStrictBSL :: FilePath -> IO BSL.ByteString
readFileStrictBSL fp =
    do bs <- BS.readFile fp
       return $ BSL.fromStrict bs

-- The following code is copied from System.IO
std_flags, output_flags, rw_flags :: CInt
std_flags    = Posix.o_NONBLOCK   .|. Posix.o_NOCTTY
output_flags = std_flags    .|. Posix.o_CREAT
rw_flags     = output_flags .|. Posix.o_RDWR

openTempFd :: FilePath -> String -> IO (FilePath, Posix.Fd)
openTempFd dir template = do
  pid <- Posix.c_getpid
  findTempName pid
  where
    (prefix,suffix) = splitExtension template

    oflags = rw_flags .|. Posix.o_EXCL .|. Posix.o_BINARY

    findTempName x = do
      fd <- Posix.withFilePath filepath $ \ f ->
              Posix.c_open f oflags 0o666
      if fd < 0
       then do
         errno <- getErrno
         if errno == eEXIST
           then findTempName (x+1)
           else do logError ("openTempFd " ++ show dir ++ " "
                             ++ show template ++ " --> errno = " ++ show (let Errno e = errno in e)
                             ++ " for filepath = " ++ show filepath)
                   ioError (errnoToIOError "openNewBinaryFile" errno Nothing (Just dir))
       else return (filepath, fromIntegral fd)
      where
        filename        = prefix ++ show x ++ suffix
        filepath        = dir `combine` filename

data TempFdState
    = TempFdState
      { tfds_closed :: Bool
      , tfds_clean :: Bool }
    deriving (Eq, Show)

withTempFd :: FilePath
           -> String
           -> (((Posix.Fd -> IO a) -> IO a)           -- write function
               -> ((FilePath -> IO b) -> IO b)        -- cleanup function
               -> IO c)
           -> IO c
withTempFd dir template action =
    do (path, fd) <- openTempFd dir template
       stateVar <- newTVarIO (TempFdState { tfds_closed = False
                                          , tfds_clean = False })
       let write f =
               do state <- atomically $ readTVar stateVar
                  when (tfds_closed state) $
                       throwIOException illegalOperationErrorType
                                        "withTempFd: write function invoked but FD already closed"
                                        Nothing
                                        (Just path)
                  f fd `finally`
                     do ignoreIOException (Posix.closeFd fd)
                        atomically $ modifyTVar stateVar (\x -> x { tfds_closed = True })
           cleanup f =
               do state <- atomically $ readTVar stateVar
                  when (tfds_clean state) $
                       throwIOException illegalOperationErrorType
                                        "withTempFd: cleanup function invoked more than once"
                                        Nothing
                                        (Just path)
                  (f path `onException` ignoreIOException (removeFile path))
                  `finally` (atomically $ modifyTVar stateVar (\x -> x { tfds_clean = True }))
       action write cleanup `finally`
              (do state <- atomically $ readTVar stateVar
                  unless (tfds_closed state) $ ignoreIOException (Posix.closeFd fd)
                  unless (tfds_clean state) $ ignoreIOException (removeFile path))

fdWriteAll :: Posix.Fd -> BS.ByteString -> IO ()
fdWriteAll fd bs =
    -- N.B., BSU.unsafeUseAsCStringLen does zero copying. Use
    -- BS.useAsCStringLen if there's any chance fdWriteBuf might
    -- alter the buffer.
    BSU.unsafeUseAsCStringLen bs $ \(buf, len) -> writeLoop (castPtr buf) (fromIntegral len) 0
    where
      writeLoop buf len counter =
        do n <- Posix.fdWriteBuf fd buf len
           if n >= len
              then return ()
              else if counter > 10
                      then throwIOException fullErrorType
                                            ("Could only write " ++
                                             show (fromIntegral (BS.length bs) - len) ++
                                             " bytes of bytestring of length " ++
                                             show (BS.length bs))
                                            Nothing
                                            Nothing
                      else writeLoop (plusPtr buf (fromIntegral n)) (len - n) (counter + 1)

fdWriteAllLazy :: Posix.Fd -> BSL.ByteString -> IO ()
fdWriteAllLazy fd bsl = go bsl
    where
      go BSLI.Empty = return ()
      go (BSLI.Chunk c cs) =
          do fdWriteAll fd c
             go cs

ioExcToFail :: IO a -> IO (Fail a)
ioExcToFail action =
    (action >>= (return . Ok)) `catch` (\(ex::Exc.IOException) -> return (Fail (show ex)))

--
-- Tests
--

testFromPrim doWrite doRead x =
    do y <- withTempFd "/tmp" "testFromPrim" $ \writeFun cleanFun ->
                do _ <- writeFun (\fd -> doWrite fd x)
                   cleanFun (\path -> do y <- doRead path
                                         removeFile path
                                         return y)
       assertEqual x y

testFromString pack write read s =
    do -- putStrLn s
       testFromPrim write read (pack s)

testFromStrings pack write read l = mapM_ (testFromString pack write read) l

testFromFile write read f =
    do -- putStrLn f
       x <- read f
       testFromPrim write read x

testFromFiles write read l = mapM_ (testFromFile write read) l

allDataFiles =
    do fs <- getDirectoryContents "data"
       let l = map ("data" </>) fs
       filterM doesFileExist l

test_fdWriteAll =
    do testFromStrings BSC.pack fdWriteAll BS.readFile ["", "1", "Hello World"]
       files <- allDataFiles
       testFromFiles fdWriteAll BS.readFile files

test_fdWriteAllLazy =
    do testFromStrings BSLC.pack fdWriteAllLazy BSL.readFile ["", "1", "Hello World"]
       files <- allDataFiles
       testFromFiles fdWriteAllLazy BSL.readFile files
