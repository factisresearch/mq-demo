{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE TypeFamilies, FunctionalDependencies, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances, UndecidableInstances,
             GeneralizedNewtypeDeriving, ScopedTypeVariables, CPP, FlexibleContexts #-}

module Mgw.Util.HasConns (

    HasConns(..), ConnReader, readConnReader, readConnReaderWithTimeout, runConnReader

) where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (liftM)
import Control.Monad.Error (MonadError(..))
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Int (Int64)
import qualified Control.Monad.State as S

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified Data.ByteString.Lazy as BSL

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Logging
import Mgw.Util.TimeSpan
import Mgw.Util.ThreadActivity (bracketThreadActivity)
import qualified Mgw.Util.Network as Net

class (Show e, Show (SocketName m), MonadError e m) => HasConns m e | m -> e where
    type Conn m
    type Socket m
    type SocketName m
    type SslServerConfig m
    openSocket :: SocketName m -> m (Socket m)
    openSslSocket :: SslServerConfig m -> SocketName m -> m (Socket m)
    openSslSocket _ = openSocket
    acceptConn :: Socket m -> m (Maybe (Conn m))
    readConn :: Conn m -> m BSL.ByteString
    readConnWithTimeout :: TimeSpan -> Conn m -> m (Maybe BSL.ByteString)
    writeConn :: Conn m -> BSL.ByteString -> m ()
    writeConn' :: Conn m -> [BSL.ByteString] -> m ()
    closeConn :: Conn m -> m ()
    showConn :: Conn m -> m String
    closeSocket :: Socket m -> m ()

instance HasConns IO IOError where
    type Conn IO = Net.SafeSocket
    type Socket IO = Net.Socket
    type SocketName IO = Net.PortNumber
    type SslServerConfig IO = Net.StunnelServerConfig
    openSocket n =
        do logDebug $ "Listening on " ++ show n
           Net.listenOn (Net.PortNumber n)
    openSslSocket cfg n =
        do logDebug $ "Listening on " ++ show n ++ " with SSL"
           Net.listenOnWithSsl cfg n
    acceptConn s =
        do logDebug $ "Accepting connections on " ++ show s
           bracketThreadActivity ("accepting connection on " ++ show s) $
                                 liftM (Just . fst) (Net.accept s)
    readConn c =
        do logTrace $ "Receiving from " ++ show c
           bracketThreadActivity ("receiving from " ++ show c) (Net.recv c)
    readConnWithTimeout t c =
        do let msg = "receiving from " ++ show c ++ " with timeout " ++ show t
           logTrace msg
           bracketThreadActivity msg (Net.recvWithTimeout t c)
    writeConn c s =
        do let info = show (BSL.length s) ++ " bytes to " ++ show c
           logTrace $ "Writing " ++ info
           bracketThreadActivity ("sending " ++  info) (Net.send c s)
    writeConn' c s =
        do let info = show (sum (map BSL.length s)) ++ " bytes to " ++ show c
           logTrace $ "Writing " ++ info
           bracketThreadActivity ("sending " ++  info) (Net.send' c s)
    closeConn c =
        do logDebug $ "Closing connection " ++ show c
           bracketThreadActivity ("closing " ++ show c) (Net.sClose c)
    showConn = return . show
    closeSocket s =
        do logDebug $ "Closing socket " ++ show s
           bracketThreadActivity ("closing " ++ show s) (Net.rawClose s)

instance HasConns m e => HasConns (ReaderT r m) e where
    type Conn (ReaderT r m) = Conn m
    type Socket (ReaderT r m) = Socket m
    type SocketName (ReaderT r m) = SocketName m
    type SslServerConfig (ReaderT r m) = SslServerConfig m
    openSocket = lift . openSocket
    openSslSocket cfg p = lift $ openSslSocket cfg p
    acceptConn = lift . acceptConn
    readConn x = lift (readConn x)
    readConnWithTimeout t x = lift (readConnWithTimeout t x)
    writeConn x y = lift (writeConn x y)
    writeConn' x y = lift (writeConn' x y)
    closeConn x = lift (closeConn x)
    showConn x = lift $ showConn x
    closeSocket = lift . closeSocket

newtype ConnReader m e a = ConnReader (S.StateT (BSL.ByteString, Conn m) m a)
    deriving (Monad, LogMonad)

runConnReader :: HasConns m e => Conn m -> ConnReader m e a -> m (a, BSL.ByteString)
runConnReader conn (ConnReader s) =
    do (a, (s,_)) <- S.runStateT s (BSL.empty, conn)
       return (a, s)

readConnReader :: forall m e. (HasConns m e, Monad m)
               => Int
               -> MaybeT (ConnReader m e) BSL.ByteString
readConnReader = readConnReaderWithTimeout (microseconds (-1))

readConnReaderWithTimeout :: forall m e. (HasConns m e, Monad m)
                          => TimeSpan
                          -> Int
                          -> MaybeT (ConnReader m e) BSL.ByteString
readConnReaderWithTimeout t len =
    MaybeT $
    ConnReader $
    do (buf, conn) <- S.get
       if BSL.length buf >= len64
          then do let prefix = BSL.take len64 buf
                      postfix = BSL.drop len64 buf
                  S.put (postfix, conn)
                  return $ Just prefix
          else loop buf conn
    where
      len64 :: Int64
      len64 = fromIntegral len
      loop :: BSL.ByteString -> Conn m -> S.StateT (BSL.ByteString, Conn m) m (Maybe BSL.ByteString)
      loop buf conn =
          do mNew <- lift $ readConnWithTimeout t conn
             case mNew of
               Nothing -> return Nothing
               Just new ->
                 let buf' = BSL.append buf new
                 in if (BSL.length buf') >= len64
                       then do let ret = BSL.take len64 buf'
                                   buf'' = BSL.drop len64 buf'
                               S.put (buf'', conn)
                               return $ Just ret
                       else if BSL.null new
                               then return $ Nothing
                               else loop buf' conn
