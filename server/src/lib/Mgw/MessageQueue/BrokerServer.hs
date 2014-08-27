{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Mgw.MessageQueue.BrokerServer
    ( createClientHandler, startBrokerServer, brokerServerMain
    , htf_thisModulesTests )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.MessageQueue.LocalBroker
import Mgw.MessageQueue.Types
import Mgw.MessageQueue.Protocol
import Mgw.Util.Concurrent
import Mgw.Util.STM
import Mgw.Util.Tx
import Mgw.Util.Logging hiding (withLogId)
import Mgw.Util.Preview
import Mgw.Util.TestHelper
import qualified Mgw.Util.Conduit as C
import qualified Mgw.Util.StrictList as SL

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Conduit
import Data.Conduit.Network
import Safe
import Test.Framework
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as V

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad.IO.Class
import System.Environment

data QueueSubscriber q
    = QueueSubscriber
      { _qs_queue :: q
      , _qs_subscriber :: SubscriberId
      }

createClientHandler ::
    (MonadResource m, MonadIO m)
    => LogId
    -> MessageBroker q
    -> IO (C.Source m ServerMessage, C.Sink ClientMessage m ())
createClientHandler logId mb =
    do replyChan <- runTx $ newTBMChan ("replyChan_" ++ show logId) 10
       subscriberIds <- runTx $ newTVar SL.Nil
       let cleanup =
             do runTx $ closeTBMChan replyChan
                subs <- runTx $ readTVar subscriberIds
                mapM_ (\(QueueSubscriber q subId) -> runTx $ mb_unsubscribeFromQueue mb q subId)
                      (SL.toLazyList subs)
                return ()
       return (source replyChan `C.finallyP` cleanup
              ,sink subscriberIds replyChan `C.finallyP` cleanup)
    where
      logMsg = withLogId logId
      source replyChan =
          do liftIO $ logInfo (logMsg "Sending queue list")
             C.yield (ServerQueues (V.fromList (HashMap.keys (mb_queues mb))))
             let loop =
                     do mSrvMsg <- liftIO $ runTx $ readTBMChan replyChan
                        case mSrvMsg of
                          Just msg -> C.yield msg >> loop
                          Nothing -> return ()
             loop
      sink subscriberIds replyChan =
          let loop =
                  do mMsg <- C.await
                     case mMsg of
                       Nothing ->
                           liftIO $ logNote (logMsg ("client closed connection"))
                       Just clMsg ->
                           do liftIO $ handleClientMessage clMsg subscriberIds replyChan
                              loop
          in loop
      handleClientMessage msg subscriberIds replyChan =
          case msg of
            ClientSubscribe queueName ->
                case mb_lookupQueue mb queueName of
                  Nothing ->
                      logWarn (logMsg ("ignoring subscription to unkown queue: " ++
                                       preview queueName))
                  Just q ->
                      do logInfo (logMsg ("received subscription for " ++ preview queueName))
                         runTx $
                               do subId <- mb_subscribeToQueue mb q
                                               (messageHandler queueName replyChan)
                                  modifyTVar' subscriberIds (QueueSubscriber q subId SL.:!)
            ClientPublishMessage queueName msg ->
                case mb_lookupQueue mb queueName of
                  Nothing ->
                      logWarn (logMsg ("ignoring publication of message " ++
                                       preview (msg_id msg) ++ " for unknown queue " ++
                                       preview queueName))
                  Just q ->
                      do logInfo (logMsg ("received new message " ++ preview (msg_id msg) ++
                                          " for queue " ++ preview queueName))
                         mb_publishMessage mb q msg
      messageHandler queueName replyChan =
          mkSubscriber (T.pack $ "handler_" ++ show logId ++ "_" ++ preview queueName) $ \msg ->
                       do logInfo (logMsg ("sending message " ++ preview (msg_id msg) ++
                                           " for queue " ++ preview queueName ++
                                           " to client"))
                          runTx $ writeTBMChanFailIfClosed replyChan
                                    (ServerPublishMessage queueName msg)

startBrokerServer :: Int -> MessageBroker q -> IO ()
startBrokerServer port mb =
    runTCPServer (serverSettings port "*") $ \appData ->
        let logId = logIdForNetworkApp appData
        in labelCurrentThread ("ReceiveThread_" ++ show logId) $
           do (source, sink) <- createClientHandler logId mb
              _ <- fork WorkerThread ("SendThread_" ++ show logId) $
                       runResourceT $
                         source $= serializeServerMsg logId $$
                         transPipe liftIO (appSink appData)
              runResourceT $
                transPipe liftIO (appSource appData) $=
                parseClientMsg logId $$ sink

brokerServerMain :: IO ()
brokerServerMain =
    do args <- getArgs
       _ <- withLoggingAndLevel args INFO doWork
       return ()
    where
      doWork args =
          case args of
            (portStr : storeDir : rest@(_ : _))
                 | Just p <- readMay portStr ->
                             do mb <- createLocalBroker (LogId "server") (Just storeDir)
                                          (parseQueueSpec rest)
                                startBrokerServer p mb
            _ ->
                 do progName <- getProgName
                    logError
                        ("USAGE: " ++ progName ++ " PORT STORE_DIR QUEUE_SPEC...\n\n" ++
                         "QUEUE_SPEC is the name of a queue, possibly prefixed with " ++
                         "'@' to denote a persistent queue")
      parseQueueSpec l =
          flip map l $ \s ->
              case s of
                '@':queueName -> (QueueName (T.pack queueName), persistentQueueOpts)
                queueName -> (QueueName (T.pack queueName), transientQueueOpts)
