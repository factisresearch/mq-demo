{-# LANGUAGE CPP #-}
module Mgw.Util.Temp
    ( withPersistentTempFile, withPersistentSysTempFile
    , withSysTempFile, withSysTempDir
    , withTempFile, withTempDir
    , DeleteStrategy(..), deleteIfNoException, deleteAfterAction
    )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.ExitAction
import Mgw.Util.Exception
import Mgw.Util.Logging

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified System.IO.Temp as Temp
import System.FilePath

----------------------------------------
-- STDLIB
----------------------------------------
import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified Control.Exception as Exception

withPersistentSysTempFile :: String -> ((FilePath, IO.Handle) -> IO b) -> IO b
withPersistentSysTempFile = withSysTempFile DeleteAtExit

withPersistentTempFile :: FilePath -> String -> ((FilePath, IO.Handle) -> IO b) -> IO b
withPersistentTempFile = withTempFile DeleteAtExit

data DeleteStrategy a
    = DeleteAtExit
    | DeleteOnResult (SomeException -> Bool) {- delete on exception or not -}
                     (a -> Bool) {- predicate on result -}

deleteIfNoException :: DeleteStrategy a
deleteIfNoException = DeleteOnResult (const False) (const True)

deleteAfterAction :: DeleteStrategy a
deleteAfterAction = DeleteOnResult (const True) (const True)

withSysTempFile :: DeleteStrategy b -> String -> ((FilePath, IO.Handle) -> IO b) -> IO b
withSysTempFile strat template action =
    do tmpDir <- Dir.getTemporaryDirectory
       withTempFile strat tmpDir template action

withSysTempDir :: DeleteStrategy b -> String -> (FilePath -> IO b) -> IO b
withSysTempDir strat template action =
    do tmpDir <- Dir.getTemporaryDirectory
       withTempDir strat tmpDir template action

withTempFile :: DeleteStrategy b -> FilePath -> String -> ((FilePath, IO.Handle) -> IO b) -> IO b
withTempFile = withTempSomething create remove describe
    where
      create tmpDir template = Temp.openTempFile tmpDir template
      remove (name, hdl) = ignoringIOErrors (IO.hClose hdl >> Dir.removeFile name)
      describe (name, _) = "tmpfile " ++ name

withTempDir :: DeleteStrategy b -> FilePath -> String -> (FilePath -> IO b) -> IO b
withTempDir = withTempSomething create remove describe
    where
      create tmpDir template = Temp.createTempDirectory tmpDir template
      remove name = ignoringIOErrors (Dir.removeDirectoryRecursive name)
      describe name = "tmpdir " ++ name

withTempSomething :: (FilePath -> String -> IO a) -- ^ create
                  -> (a -> IO ())                 -- ^ remove
                  -> (a -> String)                -- ^ describe thing created
                  -> DeleteStrategy b             -- ^ delete strategy
                  -> FilePath                     -- ^ temporary directory
                  -> String                       -- ^ name template
                  -> (a -> IO b)                  -- ^ action
                  -> IO b
withTempSomething create remove describe strat tmpDir template action =
    bracketWithResult
      ("action with temporary template " ++ (tmpDir </> template))
      (create tmpDir template)
      cleanupAfter
      (\x ->
           do case strat of
                DeleteAtExit -> regExitAction x
                _ -> return ()
              action x
      )
    where
      regExitAction x =
          let eactName = "Remove " ++ describe x
              eactType = (ExitActionRegular, ExitDelayShort)
              eactAction = remove x
          in registerExitAction eactName eactType eactAction >> return ()
      cleanupAfter x res =
          case strat of
            DeleteOnResult onExc onRegular ->
                case res of
                  Left exc | onExc exc -> remove x
                  Right y | onRegular y -> remove x
                  Left exc ->
                      do mMsg <- tooManyOpenFilesMessage exc
                         let msg =
                                 "Not removing temporary " ++ describe x ++ " because of " ++
                                 show exc ++ (case mMsg of
                                                Nothing -> ""
                                                Just x -> "\n" ++ x)
                         logWarn msg
                  _ -> do logWarn ("Not removing temporary " ++ describe x)
                          return ()
            _ -> return ()

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe =
    ioe `Exception.catch` (\e -> do logDebug ("Ignoring IO error: " ++ show e)
                                    const (return ()) (e :: IOError))
