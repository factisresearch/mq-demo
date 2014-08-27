{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}

#include "src/macros.h"

import System.Environment
import System.Exit

import Test.Framework
import Test.Framework.TestManager
import Test.Framework.BlackBoxTest

import Mgw.Util.Misc
import Mgw.Util.Locations
import Mgw.Util.TestHelper

{- In Emacs sort block with M-x sort-lines #-}
import {-@ HTF_TESTS @-} Mgw.MessageQueue.BrokerStub
import {-@ HTF_TESTS @-} Mgw.MessageQueue.LocalBroker
import {-@ HTF_TESTS @-} Mgw.MessageQueue.Tests

allTests = htf_importedTests

main =
    do args <- getArgs
       (printSummary, ecode) <-
           withLogging args (\args -> runTestWithArgs' args allTests )
       printSummary
       exitWith ecode
