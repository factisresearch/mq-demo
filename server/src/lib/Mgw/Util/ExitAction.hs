{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mgw.Util.ExitAction
    ( ExitActionId, ExitActionBehavior
    , ExitActionDelay(..), ExitActionPriority(..)
    , registerExitAction, unregisterExitAction, runExitActions, withExitActions
    )
where

#include "src/macros.h"

import Prelude
import System.Exit (ExitCode(..), exitWith)

import Mgw.Util.ExitAction.Base
import Mgw.Util.Logging.LogWriter
import Mgw.Util.Signals

withExitActions :: IO a -> IO a
withExitActions io =
    do res <- withExitActions' io
       case res of
         Left code -> exitWith code
         Right value -> return value

withExitActions' :: IO a -> IO (Either ExitCode a)
withExitActions' io =
    do installDefaultSignalHandlers
       withExitActions'' io teardownLogging
