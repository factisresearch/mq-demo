{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Mgw.MessageQueue.BrokerServer
    ( runClientHandler, startBrokerServer, brokerServerMain
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
import Mgw.Util.Network
import Mgw.Util.STM
import Mgw.Util.Tx
import Mgw.Util.Logging hiding (withLogId)
import Mgw.Util.Preview
import Mgw.Util.TestHelper
import qualified Mgw.Util.Streams as Streams
import qualified Mgw.Util.StrictList as SL

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Safe
import Test.Framework
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as V

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Exception
import System.Environment

data QueueSubscriber q
    = QueueSubscriber
      { _qs_queue :: q
      , _qs_subscriber :: SubscriberId
      }

runClientHandler ::
       LogId
    -> MessageBroker q
    -> Streams.InputStream ClientMessage
    -> Streams.OutputStream ServerMessage
    -> IO ()
runClientHandler logId mb fromClient toClient =
    do logInfo (logMsg "Sending queue list")
       Streams.write (Just $ ServerQueues (V.fromList (HashMap.keys (mb_queues mb)))) toClient
       subscriberIds <- runTx $ newTVar SL.Nil
       Streams.forM_ fromClient (handleClientMessage subscriberIds) `finally`
             do subs <- runTx $ readTVar subscriberIds
                mapM_ (\(QueueSubscriber q subId) -> runTx $ mb_unsubscribeFromQueue mb q subId)
                      (SL.toLazyList subs)
    where
      logMsg = withLogId logId
      handleClientMessage subscriberIds msg =
          case msg of
            ClientSubscribe queueName ->
                case mb_lookupQueue mb queueName of
                  Nothing ->
                      logWarn (logMsg ("ignoring subscription to unkown queue: " ++
                                       preview queueName))
                  Just q ->
                      do logInfo (logMsg ("received subscription for " ++ preview queueName))
                         runTx $
                               do subId <- mb_subscribeToQueue mb q (messageHandler queueName)
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
      messageHandler queueName =
          mkSubscriber (T.pack $ "handler_" ++ show logId ++ "_" ++ preview queueName) $ \msg ->
                       do logInfo (logMsg ("sending message " ++ preview (msg_id msg) ++
                                           " for queue " ++ preview queueName ++
                                           " to client"))
                          Streams.write (Just (ServerPublishMessage queueName msg)) toClient

startBrokerServer :: Int -> MessageBroker q -> IO ()
startBrokerServer port mb =
    runTCPServer (serverSettingsTCP port "*") $ \appData ->
        let logId = logIdForNetworkApp appData
        in labelCurrentThread ("ClientHandler_" ++ show logId) $
             do fromClient <- parseClientMsg logId (na_inputStream appData)
                toClient <- serializeServerMsg logId (na_outputStream appData)
                runClientHandler logId mb fromClient toClient

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
