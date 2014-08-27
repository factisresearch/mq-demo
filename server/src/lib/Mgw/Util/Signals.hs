{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Mgw.Util.Signals (

    Signal, SignalHandler(..), installSignalHandler, installDefaultSignalHandlers
  , sigABRT
  , sigALRM
  , sigBUS
  , sigCHLD
  , sigCONT
  , sigFPE
  , sigHUP
  , sigILL
  , sigINT
  , sigKILL
  , sigPIPE
  , sigQUIT
  , sigSEGV
  , sigSTOP
  , sigTERM
  , sigTSTP
  , sigTTIN
  , sigTTOU
  , sigUSR1
  , sigUSR2
  , signalFromExitCode

) where

#include "src/macros.h"

import Prelude
import Control.Exception (SomeException, catch)
import qualified System.Posix.Signals as PS
import System.Exit
import System.IO

import Mgw.Util.ExitAction.Base
import Mgw.Util.Logging


newtype Signal = Signal { unSignal :: PS.Signal }
    deriving (Eq, Ord, Enum, Num, Real, Integral)

instance Show Signal where
    showsPrec _ (Signal s) =
        case () of
          _| s == PS.sigABRT -> showString "sigABRT"
           | s == PS.sigALRM -> showString "sigALRM"
           | s == PS.sigBUS  -> showString "sigBUS"
           | s == PS.sigCHLD -> showString "sigCHLD"
           | s == PS.sigCONT -> showString "sigCONT"
           | s == PS.sigFPE  -> showString "sigFPE"
           | s == PS.sigHUP  -> showString "sigHUP"
           | s == PS.sigILL  -> showString "sigILL"
           | s == PS.sigINT  -> showString "sigINT"
           | s == PS.sigKILL -> showString "sigKILL"
           | s == PS.sigPIPE -> showString "sigPIPE"
           | s == PS.sigQUIT -> showString "sigQUIT"
           | s == PS.sigSEGV -> showString "sigSEGV"
           | s == PS.sigSTOP -> showString "sigSTOP"
           | s == PS.sigTERM -> showString "sigTERM"
           | s == PS.sigTSTP -> showString "sigTSTP"
           | s == PS.sigTTIN -> showString "sigTTIN"
           | s == PS.sigTTOU -> showString "sigTTOU"
           | s == PS.sigUSR1 -> showString "sigUSR1"
           | s == PS.sigUSR2 -> showString "sigUSR2"
           | otherwise       -> showString "sig<" . showsPrec 0 s . showChar '>'


sigABRT = Signal PS.sigABRT
sigALRM = Signal PS.sigALRM
sigBUS  = Signal PS.sigBUS
sigCHLD = Signal PS.sigCHLD
sigCONT = Signal PS.sigCONT
sigFPE  = Signal PS.sigFPE
sigHUP  = Signal PS.sigHUP
sigILL  = Signal PS.sigILL
sigINT  = Signal PS.sigINT
sigKILL = Signal PS.sigKILL
sigPIPE = Signal PS.sigPIPE
sigQUIT = Signal PS.sigQUIT
sigSEGV = Signal PS.sigSEGV
sigSTOP = Signal PS.sigSTOP
sigTERM = Signal PS.sigTERM
sigTSTP = Signal PS.sigTSTP
sigTTIN = Signal PS.sigTTIN
sigTTOU = Signal PS.sigTTOU
sigUSR1 = Signal PS.sigUSR1
sigUSR2 = Signal PS.sigUSR2

termSignals :: [Signal]
termSignals = [sigHUP
              ,sigINT
              ,sigQUIT
              ,sigILL
              ,sigABRT
              ,sigFPE
              ,sigTERM
              ,sigUSR1
              ,sigUSR2]

allSignals :: [Signal]
allSignals = [sigABRT
             ,sigALRM
             ,sigBUS
             ,sigCHLD
             ,sigCONT
             ,sigFPE
             ,sigHUP
             ,sigILL
             ,sigINT
             ,sigKILL
             ,sigPIPE
             ,sigQUIT
             ,sigSEGV
             ,sigSTOP
             ,sigTERM
             ,sigTSTP
             ,sigTTIN
             ,sigTTOU
             ,sigUSR1
             ,sigUSR2]

installDefaultSignalHandlers :: IO ()
installDefaultSignalHandlers =
    mapM_ (\s -> installSignalHandler s Default) termSignals

exitCodeForSignal :: Signal -> ExitCode
exitCodeForSignal sig =
    if sig == sigTERM
       then ExitSuccess
       else ExitFailure $ fromInteger (128 + toInteger sig)

signalFromExitCode :: ExitCode -> Maybe Signal
signalFromExitCode ec =
    case ec of
      ExitSuccess -> Nothing
      ExitFailure i ->
          let sig = (fromInteger . toInteger) (i - 128)
          in if sig `elem` allSignals then Just sig else Nothing

data SignalHandler = Default
                   | Ignore
                   | Catch (IO ())

asPosixSignal :: Signal -> SignalHandler -> PS.Handler
asPosixSignal s h =
    case h of
      Default ->
          if s `elem` termSignals
             then PS.Catch (do let ecodeAdt = exitCodeForSignal s
                                   ecodeInt = case ecodeAdt of { ExitFailure i -> i; _ -> 0 }
                               logWarn ("Caught signal " ++ show s ++ ", terminating program!")
                               hPutStrLn stderr ("Caught signal " ++ show s ++
                                                 ", terminating program!")
                               runExitActions ecodeInt)
             else PS.Default
      Ignore -> PS.Ignore
      Catch io -> PS.Catch (do logInfo ("Caught signal " ++ show s ++ ", running signal handler")
                               ioSafe io)
    where
      ioSafe io = io `catch` (\(e::SomeException) ->
                                 logError ("Exception in signal handler: " ++ show e))

installSignalHandler :: Signal -> SignalHandler -> IO ()
installSignalHandler s h =
    do _ <- PS.installHandler (unSignal s) (asPosixSignal s h) Nothing
       return ()
