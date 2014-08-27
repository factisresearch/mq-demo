{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, MultiParamTypeClasses, TypeFamilies,
             GeneralizedNewtypeDeriving, CPP, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Mgw.Util.Pb (

  mkSeq, mkTextSeq, fromSeq, textFromUtf8, fromUtf8, toUtf8, textToUtf8
 ,bsFromUtf8, unsafeBsToUtf8
 ,serializePb, serializePbWoLength, receivePb, sendPb, sendPbs, readPrologue
 ,mkUnknown
 ,PbUtf8, defaultValue
 ,pbufSerialize, pbufParse

 , receivePbFromBS

 ,SimConnId, SimConnState

) where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (MonadPlus, mplus, mzero, liftM)
import Control.Monad.Error (MonadError(throwError, catchError), ErrorT(runErrorT))
import Control.Monad.Maybe (MaybeT(runMaybeT))
import Control.Monad.State (get, put, State, evalState)
import Data.Foldable (Foldable, foldMap)
import Data.Int (Int64)
import Data.Word

import qualified Data.Map as Map
import qualified Data.List as List

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Binary (decode)
import Data.ByteString.Lazy.UTF8 (toString, fromString)

import Text.ProtocolBuffers
    ( Wire, ReflectDescriptor, utf8, messagePut, reflectDescriptorInfo
    , messageWithLengthGetM, runGet, defaultValue, runPut
    , messageWithLengthPutM
    , mName, haskellPrefix, descName, baseName)
import Text.ProtocolBuffers.Get (Result(..))
import Text.ProtocolBuffers.Basic (Utf8(..))
import Text.ProtocolBuffers.Unknown (UnknownField)
import Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.String

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.BaseCoding
import Mgw.Util.Fail (Fail(Ok, Fail))
import Mgw.Util.Logging
import Mgw.Util.TimeSpan
import Mgw.Util.HasConns (HasConns(..), runConnReader, readConnReaderWithTimeout)
import Mgw.Util.Text () -- instances
import Mgw.Util.Orphans ()
import Mgw.Util.Preview
import Mgw.Util.Conduit ()

_TAG_MESSAGES_ :: Bool
_TAG_MESSAGES_ = False

_OPEN_TAG_ :: BSL.ByteString
_OPEN_TAG_ = BSL.pack [0,0,0x42,0xff,0xff,0x42]

_CLOSE_TAG_ :: BSL.ByteString
_CLOSE_TAG_ = BSL.pack [0xff,0xff,0x42,1,1,0x42]

type PbUtf8 = Utf8

instance Data.String.IsString PbUtf8 where
    fromString = toUtf8

receivePb :: (LogMonad m, HasConns m e, Show w, ReflectDescriptor w, Wire w)
          => (w -> m ()) -> BSL.ByteString -> Conn m -> m ()
receivePb handleMsg buf socket = receivePbGen handleMsg buf (readConn socket)

receivePbFromBS :: (LogMonad m, HasConns m e, Show w, ReflectDescriptor w, Wire w)
                  => (w -> m ()) -> BSL.ByteString -> m ()
receivePbFromBS handleMsg buf = receivePbGen handleMsg buf (return BSL.empty)

receivePbGen :: (LogMonad m, Show w, ReflectDescriptor w, Wire w)
             => (w -> m ()) -> BSL.ByteString -> m BSL.ByteString -> m ()
receivePbGen handleMsg buf getData = loopNew buf BSL.empty
    where
      loopNew buf prefix = recvChunk buf
                                     (\_ -> return ())
                                     (parseAndHandleData . BSL.append prefix)
      loopPartial cont =
          recvChunk BSL.empty
                    (\dat -> handleResult dat $ cont Nothing)
                    (\dat -> handleResult dat $ cont $ Just dat)
      parseAndHandleData dat =
          let res = runGet messageWithLengthGetM dat
          in handleResult dat res
      handleResult dat res =
          case res of
            Failed _ errmsg -> do logError ("failed to parse message: " ++ errmsg ++
                                            " Raw data: " ++ show dat)
                                  return ()
            Partial cont ->
                do logTrace "received partial data; continuing"
                   loopPartial cont
            Finished rest _ msg ->
                do logTraceWithTag "receivePb" $ "Received PB message: " ++ show msg
                   logTraceWithTag "receivePb" $ "rest is: " ++ show rest
                   handleMsg msg
                   if BSL.null rest
                      then loopNew BSL.empty BSL.empty
                      else parseAndHandleData rest
      recvChunk buf whenEmpty withData =
          do bs <- if BSL.null buf then getData else return buf
             logTrace $ "received bytes: " ++ show bs
             if BSL.null bs
               then do logTrace "message empty"
                       whenEmpty bs
               else withData bs

serializePb :: (ReflectDescriptor w, Wire w) => w -> BSL.ByteString
serializePb msg = runPut (messageWithLengthPutM msg)

serializePbWoLength :: (ReflectDescriptor w, Wire w) => w -> BSL.ByteString
serializePbWoLength msg = messagePut msg

sendPbs :: (LogMonad m, HasConns m e, ReflectDescriptor w, Wire w, Show w)
        => Conn m -> [w] -> m Int64
sendPbs sock msgs =
    do let bs = runPut $ mapM_ messageWithLengthPutM msgs
       logDebugWithTag "sendPbs" ("Sending " ++ show (length msgs) ++ " messages with "
                                  ++ show (BSL.length bs) ++ " bytes:\n"
                                  ++ List.intercalate "\n" (map show msgs))
       logTraceWithTag "sendPbs" (bsToBase64String $ BS.concat $ BSL.toChunks bs)
       writeConn sock bs
       return (BSL.length bs)

sendPb :: (LogMonad m, HasConns m e, ReflectDescriptor w, Wire w, Show w) => Conn m -> w -> m Int64
sendPb sock msg =
    do let bs = serializePb msg
       logInfoWithTag "sendPb" ("sending " ++ show (name (reflectDescriptorInfo msg)) ++ " with "
                                ++ show (BSL.length bs) ++ " bytes")
       logDebugWithTag "sendPb" (show msg)
       logTraceWithTag "sendPb" (bsToBase64String $ BS.concat $ BSL.toChunks bs)
       writeConn sock bs
       return (BSL.length bs)
    where
      name di = foldl (\s n -> s ++ mName n ++ ".") "" (haskellPrefix (descName di))
                ++ mName (baseName (descName di))

mkSeq :: MonadPlus m => [a] -> m a
mkSeq [] = mzero
mkSeq (x:xs) = return x `mplus` mkSeq xs

mkTextSeq :: MonadPlus m => [T.Text] -> m Utf8
mkTextSeq = mkSeq . map textToUtf8

mkUnknown :: Wire m => (UnknownField -> m) -> m
mkUnknown m = m defaultValue

toUtf8 :: String -> Utf8
toUtf8 = Utf8 . fromString

textToUtf8 :: T.Text -> Utf8
textToUtf8 = Utf8 . (\bs -> BSL.fromChunks [bs]) . TE.encodeUtf8

-- | Converts a ByteString to a UTF8 protocol buffer value without checking for correct encoding.
-- Make sure your bytestring represents a valid UTF-8 byte seqeunce!
unsafeBsToUtf8 :: BS.ByteString -> Utf8
unsafeBsToUtf8 = Utf8 . BSL.fromChunks . (:[])

fromUtf8 :: Utf8 -> String
fromUtf8 = toString . utf8

textFromUtf8 :: Utf8 -> T.Text
textFromUtf8  = TE.decodeUtf8 . BS.concat . BSL.toChunks . utf8

bsFromUtf8 :: Utf8 -> BS.ByteString
bsFromUtf8 = BS.concat . BSL.toChunks . utf8

fromSeq :: Foldable t => t a -> [a]
fromSeq = foldMap (:[])

readPrologue :: (HasConns m e, LogMonad m)
             => TimeSpan
             -> BSL.ByteString
             -> Conn m
             -> m (Fail ( Word8           -- version
                        , BSL.ByteString  -- payload (unused at the moment)
                        , BSL.ByteString  -- unhandled bytes
                        )
                  )
readPrologue timeout magic conn =
    do connId <- showConn conn
       (mPrologue, carryover) <- runConnReader conn $ runMaybeT (action connId)
       case mPrologue of
            Nothing -> return (Fail $ "client did not sent enough bytes to read prologue, timeout: "
                                      ++ show timeout)
            Just (Fail msg) -> return (Fail msg)
            Just (Ok (version, unusedBytes)) -> return $ Ok (version, unusedBytes, carryover)
    where
      action connId =
          do startBytes <- readConnReader $ fromIntegral (BSL.length magic)
             if startBytes == magic
                then do logDebug ("Successfully read magic bytes " ++ show magic ++
                                 " from " ++ connId)
                        version <- liftM decode $ readConnReader 1
                        logDebug ("Successfully read version " ++ show version ++ " from " ++
                                 connId)
                        len <- liftM decode $ readConnReader 2
                        logDebug ("Successfully read length " ++ show len ++ " from " ++ connId)
                        unusedBytes <- readConnReader $ fromIntegral (len :: Word16)
                        logDebug ("Successfully read payload " ++ shorten unusedBytes ++
                                 " from " ++ connId)
                        logInfo ("Successfully read prologue with major version " ++ show version
                                 ++ ".")
                        return $ Ok (fromIntegral (version :: Word8), unusedBytes)
                else return $ Fail "could not find magic bytes"
      readConnReader = readConnReaderWithTimeout timeout
      shorten bs =
          let s = show bs
              n = length s
              limit = 20
          in if n < limit then s else take limit s ++ " ..."

pbufSerialize ::
    (ReflectDescriptor w, Wire w, Preview w, LogMonad m, MonadResourceBase m)
    => String
    -> C.Conduit w m BS.ByteString
pbufSerialize session = C.awaitForever f
    where
      fn = session ++ ".out"
      f pb =
          do let bslRaw = runPut (messageWithLengthPutM pb)
                 rawPlus = if _TAG_MESSAGES_
                              then BSL.concat [ _OPEN_TAG_
                                              , bslRaw
                                              , _CLOSE_TAG_
                                              ]
                              else bslRaw
                 shortLogMsg = preview pb
             logInfoWithTag "pbuf-send" (session ++ ": serializing " ++ shortLogMsg)
             appendTraceWithTag "pbuf-send" fn (Left shortLogMsg)
             appendTraceWithTag "pbuf-send" fn (Left (bsToBase16String (BSL.toStrict rawPlus)))
             --M.mapM_ C.yield (BSL.toChunks rawPlus)
             C.yield (BSL.toStrict rawPlus)

pbufParse ::
    (ReflectDescriptor w, Wire w, Show w, LogMonad m, MonadResourceBase m)
    => String
    -> C.Conduit BS.ByteString m w
pbufParse session = new
    where
      fn = session ++ ".in"
      new = read (runGet messageWithLengthGetM . BSL.fromChunks . (:[]))
      read parse =
          do mbs <- C.await
             case mbs of
               Just bs -> checkResult (parse bs)
               Nothing -> return ()
      checkResult result =
          case result of
            Failed _ errmsg -> fail errmsg
            Partial cont -> read (cont . Just . BSL.fromChunks . (:[]))
            Finished rest _ msg -> finished rest msg
      finished rest pb =
          do logInfoWithTag "pbuf-recv" (session ++ ": parsed " ++ show pb)
             appendTraceWithTag "pbuf-recv" fn (Left (show pb))
             C.yield pb
             checkResult (runGet messageWithLengthGetM rest)
--
-- Mock connection for tests
--

newtype SimConnId = SimConnId Int deriving (Show, Eq, Ord, Num)
newtype SimConnState = SimConnState (Map.Map SimConnId [BSL.ByteString])

instance Show SimConnState where
    show _ = "<SimmConnState>"

newtype SimConnM a = SimConnM { unSimConnM :: ErrorT String (State SimConnState) a } deriving Monad

instance LogMonad SimConnM where
    doLog _ _ _ _ _ = return ()

_runSimConnM :: forall a. SimConnState -> SimConnM a -> Either String a
_runSimConnM s (SimConnM ma) =
    evalState x s
    where x :: State SimConnState (Either String a)
          x = runErrorT ma

instance MonadError String SimConnM where
    -- throwError :: e -> m a
    throwError = SimConnM . throwError
    -- catchError :: m a -> (e -> m a) -> m a
    catchError (SimConnM ma) f = SimConnM $ catchError ma (unSimConnM . f)

instance HasConns SimConnM String where
    type Conn SimConnM = SimConnId
    type Socket SimConnM = SimConnState
    type SocketName SimConnM = SimConnState
    type SslServerConfig SimConnM = ()
    openSocket _ = safeError "openSocket not implemented for HasConns SimConnM String"
    acceptConn _ = safeError "acceptConn not implemented for HasConns SimConnM String"
    readConn conn =
        SimConnM $
        do (SimConnState m) <- get
           case Map.lookup conn m of
             Nothing -> safeError "lookup in map failed"
             Just (x:xs) ->
                 do put $ SimConnState $ Map.insert conn xs m
                    return x
             Just [] -> return BSL.empty
    readConnWithTimeout _ conn =
        do x <- readConn conn
           return $ Just x
    writeConn _ _ = safeError "writeConn not implemented for HasConns SimConnM String"
    writeConn' _ _ = safeError "writeConn' not implemented for HasConns SimConnM String"
    closeConn _ = safeError "closeConn not implemented for HasConns SimConnM String"
    showConn _ = return "SimConnM"
    closeSocket _ = safeError "closeSocket not implemented for HasConns SimConnM String"

--
-- Tests
--

-- deleted
