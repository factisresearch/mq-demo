{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mgw.Util.MVar
    ( IsMVar(..)
    , MVar, newMVar, newEmptyMVar
    , NamedMVar, newNamedMVar, newEmptyNamedMVar
    )
where

#include "src/macros.h"

import Control.Monad (liftM)
import Control.Exception (BlockedIndefinitelyOnMVar)
import qualified Control.Concurrent.MVar as Conc

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.ThreadActivity (bracketThreadActivity, logThreadActivities)
import Mgw.Util.HasExceptions (catchException, throwException)
import Mgw.Util.Logging

newtype MVar a = MVar (Conc.MVar a)

data NamedMVar a
    = NamedMVar
    { _nmv_name :: String
    , _nmv_var :: Conc.MVar a
    }

class IsMVar v where
    modifyMVar_ :: v a -> (a -> IO a) -> IO ()
    modifyMVar :: v a -> (a -> IO (a,b)) -> IO b
    putMVar :: v a -> a -> IO ()
    takeMVar :: v a -> IO a
    readMVar :: v a -> IO a
    withMVar :: v a -> (a -> IO b) -> IO b
    tryPutMVar :: v a -> a -> IO Bool

instance IsMVar MVar where
    modifyMVar_ (MVar v) f = bracket  "modifying MVar" (Conc.modifyMVar_ v f)
    modifyMVar (MVar v) f = bracket  "modifying MVar" (Conc.modifyMVar v f)
    takeMVar (MVar v) = bracket "taking MVar" (Conc.takeMVar v)
    readMVar (MVar v) = bracket "reading MVar" (Conc.readMVar v)
    putMVar (MVar v) x = bracket "putting MVar" (Conc.putMVar v x)
    withMVar (MVar v) act = bracket "withMVar" (Conc.withMVar v act)
    tryPutMVar (MVar v) x = bracket "trying to put MVar" (Conc.tryPutMVar v x)

instance IsMVar NamedMVar where
    modifyMVar_ (NamedMVar n mvar) f =
        bracket ("modifying MVar `" ++ n ++ "'") (Conc.modifyMVar_ mvar f)
    modifyMVar (NamedMVar n mvar) f =
        bracket ("modifying MVar `" ++ n ++ "'") (Conc.modifyMVar mvar f)
    takeMVar (NamedMVar n mvar) =
        bracket ("taking MVar `" ++ n ++ "'") (Conc.takeMVar mvar)
    readMVar (NamedMVar n mvar) =
        bracket ("reading MVar `" ++ n ++ "'") (Conc.readMVar mvar)
    putMVar (NamedMVar n mvar) x =
        bracket ("putting MVar `" ++ n ++ "'") (Conc.putMVar mvar x)
    withMVar (NamedMVar n mvar) act =
        bracket ("withMVar `" ++ n ++ "'") (Conc.withMVar mvar act)
    tryPutMVar (NamedMVar n v) x =
        bracket ("trying to put MVar `"++n++"'") (Conc.tryPutMVar v x)

bracket operation action = bracketThreadActivity operation (catchException action handler)
    where
      handler (e :: BlockedIndefinitelyOnMVar) =
          do logError $ "Blocked indefinitly while " ++ operation
             logThreadActivities
             throwException e

newMVar :: a -> IO (MVar a)
newMVar = liftM MVar . Conc.newMVar

newEmptyMVar :: IO (MVar a)
newEmptyMVar = liftM MVar Conc.newEmptyMVar

newNamedMVar :: String -> a -> IO (NamedMVar a)
newNamedMVar name = liftM (NamedMVar name) . Conc.newMVar

newEmptyNamedMVar :: String -> IO (NamedMVar a)
newEmptyNamedMVar name = liftM (NamedMVar name) Conc.newEmptyMVar
