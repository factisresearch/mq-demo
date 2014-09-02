{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Mgw.MessageQueue.BrokerStub
    (
      sendClientMain, recvClientMain
    , htf_thisModulesTests
    )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.MessageQueue.BrokerServer
import Mgw.MessageQueue.LocalBroker
import Mgw.MessageQueue.Protocol
import Mgw.MessageQueue.Types
import Mgw.Util.Concurrent
import Mgw.Util.Exception
import Mgw.Util.STM
import Mgw.Util.Tx
import Mgw.Util.Logging hiding (withLogId)
import Mgw.Util.Network
import Mgw.Util.Preview
import Mgw.Util.Sleep
import qualified Mgw.Util.Streams as Streams
import Mgw.Util.TestHelper
import Mgw.Util.TimeSpan

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.HashSet (HashSet)
import Safe
import Test.Framework
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad
import Control.Monad.IO.Class
import System.Environment

createBrokerStub ::
    LogId
    -> (TBMChan ServerMessage, TBMChan ClientMessage)
    -> IO (MessageBroker Queue)
createBrokerStub logId (recvChan, sendChan) =
    do logInfo (logMsg "waiting for queue list..")
       mMsg <- runTx $ readTBMChan recvChan
       case mMsg of
         Just (ServerQueues queues) ->
             do logInfo (logMsg ("received queue list"))
                run sendChan recvChan (V.toList queues)
         _ ->
             do logAndFail (logMsg ("Expected queue list from server but got something else: " ++
                                    show mMsg))
    where
      logMsg = withLogId logId
      localLogId = LogId (unLogId logId `T.append` "-local")
      run sendChan recvChan queues =
          do localBroker <- createLocalBroker localLogId Nothing
                                (map (\q -> (q, transientQueueOpts)) queues)
             serverSubscriptionsVar <- newTVarIO HashSet.empty
             closeVar <- newTVarIO False
             _ <- fork WorkerThread ("BrokerStubReceiveThread_" ++ show logId) $
                      handleMessage localBroker closeVar serverSubscriptionsVar
                                    recvChan sendChan
             return $ MessageBroker
                  { mb_subscribeToQueue = subscribe serverSubscriptionsVar closeVar
                                              sendChan localBroker
                  , mb_unsubscribeFromQueue = unsubscribe localBroker
                  , mb_publishMessage = publish closeVar sendChan
                  , mb_lookupQueue = mb_lookupQueue localBroker
                  , mb_knownQueues = mb_knownQueues localBroker
                  }
      subscribe :: TVar (HashSet QueueName) -> TVar Bool -> TBMChan ClientMessage
                -> MessageBroker Queue -> Queue -> Subscriber -> STM SubscriberId
      subscribe serverSubscriptionsVar closeVar sendChan localBroker localQueue sub =
          do failIfClosed closeVar
             let queueName = q_name localQueue
             subId <- mb_subscribeToQueue localBroker localQueue sub
             set <- readTVar serverSubscriptionsVar
             unless (queueName `HashSet.member` set) $
                    do writeTVar serverSubscriptionsVar $! (HashSet.insert queueName set)
                       logInfo (logMsg ("Sending subscription for " ++ preview queueName
                                        ++ " to server"))
                       writeTBMChanFailIfClosed sendChan (ClientSubscribe queueName)
             return subId
      unsubscribe localBroker localQueue subId =
          -- FIXME: if we are the last subscriber for localQueue, we should unsubscribe
          -- from the server
          mb_unsubscribeFromQueue localBroker localQueue subId
      publish closeVar sendChan localQueue msg =
          do runTx $ failIfClosed closeVar
             logInfo (logMsg ("Sending message " ++ preview (msg_id msg) ++ " for queue " ++
                              preview (q_name localQueue) ++ " to server"))
             runTx $ writeTBMChanFailIfClosed sendChan
                       (ClientPublishMessage (q_name localQueue) msg)
      failIfClosed closeVar =
          do close <- readTVar closeVar
             when close (logAndFail (logMsg ("Connection to server definitely closed")))
      handleMessage localBroker closeVar subscriptionsVar recvChan sendChan =
          let loop =
                  do mMsg <- runTx $ readTBMChan recvChan
                     case mMsg of
                       Nothing ->
                           do logInfo (logMsg "Received channel closed")
                              runTx $ writeTVar closeVar True
                       Just (ServerQueues _) ->
                           do logDebug (logMsg "Got ServerExistingQueues message, ignoring")
                              -- FIXME: need to check that new queues match old queues
                              logInfo ("Got a reconnect, subscribing again ...")
                              runTx $
                                do subscribedQueues <- readTVar subscriptionsVar
                                   mapM_ (\q -> writeTBMChanFailIfClosed
                                                  sendChan (ClientSubscribe q))
                                         (HashSet.toList subscribedQueues)
                              loop
                       Just (ServerPublishMessage queueName msg) ->
                           do mq <- runTx $ mb_lookupQueue localBroker queueName
                              case mq of
                                Nothing ->
                                    logWarn (logMsg ("Received message " ++ preview (msg_id msg) ++
                                                     " for unknown queue " ++ preview queueName))
                                Just queue ->
                                     do logInfo (logMsg ("Received message " ++
                                                         preview (msg_id msg) ++
                                                         " for queue " ++ preview queueName))
                                        mb_publishMessage localBroker queue msg
                              loop
          in loop

connectChans ::
    (Show a, Show b)
    => LogId
    -> Streams.InputStream a
    -> Streams.OutputStream b
    -> TBMChan a
    -> TBMChan b
    -> IO ()
    -> IO ()
connectChans logId input output recvChan sendChan cleanup =
    do finishVar <- newTVarIO False
       _ <- fork WorkerThread ("BrokerStubSender_" ++ show logId) (doSend finishVar)
       _ <- fork WorkerThread ("BrokerStubReceiver_" ++ show logId) (doReceive finishVar)
       return ()
    where
      doSend finishVar =
          let loop = do mClMsg <- liftIO $ runTx $
                                  do finished <- readTVar finishVar
                                     if finished
                                     then return Nothing
                                     else readTBMChan sendChan
                        Streams.write mClMsg output
                        case mClMsg of
                          Just _ -> loop
                          Nothing -> return ()
          in loop `finally` cleanup
      doReceive finishVar =
          let loop = do mSrvMsg <- Streams.read input
                        logDebug ("Received " ++ show mSrvMsg)
                        case mSrvMsg of
                          Just srvMsg ->
                              do liftIO $ runTx $ writeTBMChanFailIfClosed recvChan srvMsg
                                 loop
                          Nothing ->
                              return ()
          in loop `finally`
                 do runTx $ writeTVar finishVar True
                    cleanup

infiniteRetryClient :: ClientSettings -> (NetworkApp -> IO ()) -> IO ()
infiniteRetryClient settings action =
    let waitTime = seconds 1
        loop =
            do runTCPClient settings action `catchSafe`
                 (\e -> logDebug ("Error in communication with gateway: " ++ show e))
               logNote ("Connection terminated, relaunching in " ++ show waitTime)
               sleepTimeSpan waitTime
               loop
    in loop

withBrokerClient :: String -> Int -> (MessageBroker Queue -> IO ()) -> IO ()
withBrokerClient host port action =
    do sendChan <- runTx $ newTBMChan ("BrokerStubSendChan") 20
       recvChan <- runTx $ newTBMChan ("BorkerStubRecvChan") 20
       _ <- fork WorkerThread "InfiniteRetry" $
              infiniteRetryClient (clientSettingsTCP port hostBytes)
                                  (clientApp sendChan recvChan)
       mb <- createBrokerStub (LogId "BrokerStub") (recvChan, sendChan)
       action mb
       return ()
    where
      clientApp sendChan recvChan app =
          let logId = logIdForNetworkApp app
              logMsg = withLogId logId
          in do logInfo (logMsg ("Connected to server " ++ show (na_sockAddr app) ++
                                 ", local addr: " ++ show (na_localAddr app)))
                source <- parseServerMsg logId (na_inputStream app)
                sink <- serializeClientMsg logId (na_outputStream app)
                finishedVar <- newTVarIO False
                connectChans logId source sink recvChan sendChan
                             (runTx $ writeTVar finishedVar True)
                logInfo (logMsg "Waiting for connection to finish")
                runTx $
                      do b <- readTVar finishedVar
                         unless b retryTx
                logInfo (logMsg "Connection to server finished")
      hostBytes = T.encodeUtf8 (T.pack host)

sendClientMain :: IO ()
sendClientMain =
    do args <- getArgs
       _ <- withLoggingAndLevel args INFO doWork
       return ()
    where
      doWork args =
          case args of
            [host, portStr, queueName, msgId, msg]
                | Just port <- readMay portStr ->
                    withBrokerClient host port $ \mb ->
                        do mq <- runTx $ mb_lookupQueue mb (QueueName (T.pack queueName))
                           case mq of
                             Nothing -> logError ("Unkown queue: " ++ queueName)
                             Just q ->
                                 do let bytes = T.encodeUtf8 (T.pack msg)
                                    mb_publishMessage mb q
                                        (Message (MessageId (T.pack msgId)) bytes)
                                    sleepTimeSpan (milliseconds 50)
            _ ->
                do progName <- getProgName
                   logError ("USAGE: " ++ progName ++ " HOST PORT QUEUE MSG_ID MSG")

recvClientMain :: IO ()
recvClientMain =
    do args <- getArgs
       _ <- withLoggingAndLevel args INFO doWork
       return ()
    where
      doWork args =
          case args of
            [host, portStr, queueName]
                | Just port <- readMay portStr ->
                    withBrokerClient host port $ \mb ->
                        do mq <- runTx $ mb_lookupQueue mb (QueueName (T.pack queueName))
                           case mq of
                             Nothing -> logError ("Unkown queue: " ++ queueName)
                             Just q ->
                                 do _ <- runTx $ mb_subscribeToQueue mb q
                                                  (mkSubscriber "recvClient" handleMsg)
                                    wait
            _ ->
                do progName <- getProgName
                   logError ("USAGE: " ++ progName ++ " HOST PORT QUEUE")
      handleMsg msg =
          let bytes = msg_payload msg
          in case T.decodeUtf8' bytes of
               Right t -> logNote ("Received message: " ++ T.unpack t)
               Left _ -> logNote ("Received binary message")
      wait =
          do sleepTimeSpan (minutes 5)
             wait

setupLocalBrokerForTest ::
    String -> MessageBroker Queue -> IO (MessageBroker Queue)
setupLocalBrokerForTest what serverBroker =
    do sendChan <- runTx $ newTBMChan (mkLogId' "BrokerStubSendChan") 20
       recvChan <- runTx $ newTBMChan (mkLogId' "BrokerStubRecvChan") 20
       let cleanup =
               runTx (closeTBMChan sendChan >> closeTBMChan recvChan)
       -- serverFromClient :: InputStream ClientMsg
       serverFromClient <- Streams.makeInputStream $
                             do mx <- runTx $ readTBMChan sendChan
                                return mx
       -- serverToClient :: OutputStream ServerMsg
       serverToClient <- Streams.makeOutputStream $ \mx ->
                           case mx of
                             Nothing -> cleanup
                             Just x -> runTx (writeTBMChanFailIfClosed recvChan x)
       _ <- fork WorkerThread (mkLogId' "server-stub-connector") $
              runClientHandler (mkLogId "server-test-handler")
                               serverBroker serverFromClient serverToClient
       createBrokerStub (mkLogId "stub") (recvChan, sendChan)
    where
      mkLogId' s =
          if null what then s else what ++ "_" ++ s
      mkLogId s =
          LogId (T.pack (mkLogId' s))

brokerStubWithBrokerServerTest ::
    (MessageBroker Queue -> MessageBroker Queue -> IO ()) -> IO ()
brokerStubWithBrokerServerTest fun =
    do serverBroker <- createLocalBroker (LogId "server-local") Nothing queues
       localBroker <- setupLocalBrokerForTest "local-broker" serverBroker
       fun serverBroker localBroker
    where
      queues = [(testQueue1, transientQueueOpts)
               ,(testQueue2, transientQueueOpts)]

test_brokerStubWithBrokerServer1 :: IO ()
test_brokerStubWithBrokerServer1 =
    brokerStubWithBrokerServerTest $ \_serverBroker localBroker ->
       messageBrokerTest localBroker localBroker

test_brokerStubWithBrokerServer2 :: IO ()
test_brokerStubWithBrokerServer2 =
    brokerStubWithBrokerServerTest $ \serverBroker localBroker ->
       messageBrokerTest serverBroker localBroker

test_brokerStubWithBrokerServer3 :: IO ()
test_brokerStubWithBrokerServer3 =
    brokerStubWithBrokerServerTest $ \serverBroker localBroker ->
       messageBrokerTest localBroker serverBroker

test_brokerStubWithBrokerServer4 :: IO ()
test_brokerStubWithBrokerServer4 =
    do serverBroker <- createLocalBroker (LogId "server-local") Nothing queues
       localBroker1 <- setupLocalBrokerForTest "local-broker-1" serverBroker
       localBroker2 <- setupLocalBrokerForTest "local-broker-2" serverBroker
       messageBrokerTest localBroker1 localBroker2
    where
      queues = [(testQueue1, transientQueueOpts)
               ,(testQueue2, transientQueueOpts)]
