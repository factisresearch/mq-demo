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
import Mgw.Util.Preview
import Mgw.Util.Sleep
import Mgw.Util.TestHelper
import Mgw.Util.TimeSpan
import qualified Mgw.Util.Conduit as C

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Conduit.Network
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
    -> IO (MessageBroker LocalQueue)
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
                  { mb_queues = mb_queues localBroker
                  , mb_subscribeToQueue = subscribe serverSubscriptionsVar closeVar
                                              sendChan localBroker
                  , mb_unsubscribeFromQueue = unsubscribe localBroker
                  , mb_publishMessage = publish closeVar sendChan
                  }
      subscribe :: TVar (HashSet QueueName) -> TVar Bool -> TBMChan ClientMessage
                -> MessageBroker LocalQueue -> LocalQueue -> Subscriber -> STM SubscriberId
      subscribe serverSubscriptionsVar closeVar sendChan localBroker localQueue sub =
          do failIfClosed closeVar
             let queueName = lq_name localQueue
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
                              preview (lq_name localQueue) ++ " to server"))
             runTx $ writeTBMChanFailIfClosed sendChan
                       (ClientPublishMessage (lq_name localQueue) msg)
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
                           do case mb_lookupQueue localBroker queueName of
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
    -> C.Source (C.ResourceT IO) a
    -> C.Sink b (C.ResourceT IO) ()
    -> TBMChan a
    -> TBMChan b
    -> IO ()
    -> IO ()
connectChans logId source sink recvChan sendChan cleanup =
    do finishVar <- newTVarIO False
       _ <- fork WorkerThread ("BrokerStubSender_" ++ show logId) (doSend finishVar)
       _ <- fork WorkerThread ("BrokerStubReceiver_" ++ show logId) (doReceive finishVar)
       return ()
    where
      doSend finishVar =
          C.runResourceT
               ((sendSource finishVar `C.finallyP` cleanup) C.$$ sink)
      doReceive finishVar =
          ignoreIOException $ C.runResourceT
              (source C.$$ (recvSink `C.finallyP`
                            do runTx $ writeTVar finishVar True
                               cleanup))
      sendSource finishVar =
          let loop = do mClMsg <- liftIO $ runTx $
                                  do finished <- readTVar finishVar
                                     if finished
                                     then return Nothing
                                     else readTBMChan sendChan
                        case mClMsg of
                          Just clMsg -> C.yield clMsg >> loop
                          Nothing -> return ()
          in loop
      recvSink =
          let loop = do mSrvMsg <- C.await
                        logDebug ("Received " ++ show mSrvMsg)
                        case mSrvMsg of
                          Just srvMsg ->
                              do liftIO $ runTx $ writeTBMChanFailIfClosed recvChan srvMsg
                                 loop
                          Nothing ->
                              return ()
          in loop

infiniteRetryClient :: ClientSettings IO -> Application IO -> IO ()
infiniteRetryClient settings app =
    let waitTime = seconds 1
        loop =
            do runTCPClient settings app `catchSafe`
                 (\e -> logDebug ("Error in communication with gateway: " ++ show e))
               logNote ("Connection terminated, relaunching in " ++ show waitTime)
               sleepTimeSpan waitTime
               loop
    in loop

setupBrokerStubChans ::
    LogId
    -> (C.Source (C.ResourceT IO) ServerMessage
       ,C.Sink ClientMessage (C.ResourceT IO) ())
    -> IO (TBMChan ServerMessage, TBMChan ClientMessage)
setupBrokerStubChans logId (source, sink) =
    do sendChan <- runTx $ newTBMChan ("BrokerStubSendChan_" ++ show logId) 20
       recvChan <- runTx $ newTBMChan ("BrokerStubRecvChan_" ++ show logId) 20
       connectChans logId source sink recvChan sendChan (runTx $
                                                         do closeTBMChan sendChan
                                                            closeTBMChan recvChan)
       return (recvChan, sendChan)

withBrokerClient :: String -> Int -> (MessageBroker LocalQueue -> IO ()) -> IO ()
withBrokerClient host port action =
    do sendChan <- runTx $ newTBMChan ("BrokerStubSendChan") 20
       recvChan <- runTx $ newTBMChan ("BorkerStubRecvChan") 20
       _ <- fork WorkerThread "InfiniteRetry" $
              infiniteRetryClient (clientSettings port hostBytes)
                                  (clientApp sendChan recvChan)
       mb <- createBrokerStub (LogId "BrokerStub") (recvChan, sendChan)
       action mb
       return ()
    where
      clientApp sendChan recvChan app =
          let logId = logIdForNetworkApp app
              logMsg = withLogId logId
          in do logInfo (logMsg ("Connected to server " ++ show (appSockAddr app) ++
                                 ", local addr: " ++ show (appLocalAddr app)))
                let source = C.transPipe liftIO (appSource app C.$= parseServerMsg logId)
                    sink = C.transPipe liftIO (serializeClientMsg logId C.=$ appSink app)
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
                       case mb_lookupQueue mb (QueueName (T.pack queueName)) of
                         Nothing -> logError ("Unkown queue: " ++ queueName)
                         Just q ->
                             do let bytes = T.encodeUtf8 (T.pack msg)
                                mb_publishMessage mb q (Message (MessageId (T.pack msgId)) bytes)
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
                       case mb_lookupQueue mb (QueueName (T.pack queueName)) of
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

brokerStubWithBrokerServerTest ::
    (MessageBroker LocalQueue -> MessageBroker LocalQueue -> IO ()) -> IO ()
brokerStubWithBrokerServerTest fun =
    do serverBroker <- createLocalBroker (LogId "server-local") Nothing queues
       sourceSink <- createClientHandler (LogId "server") serverBroker
       chans <- setupBrokerStubChans (LogId "stub-chans") sourceSink
       localBroker <- createBrokerStub (LogId "stub") chans
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
       sourceSink1 <- createClientHandler (LogId "server1") serverBroker
       chans1 <- setupBrokerStubChans (LogId ("stub1-chans")) sourceSink1
       localBroker1 <- createBrokerStub (LogId "stub1") chans1
       sourceSink2 <- createClientHandler (LogId "server2") serverBroker
       chans2 <- setupBrokerStubChans (LogId ("stub2-chans")) sourceSink2
       localBroker2 <- createBrokerStub (LogId "stub2") chans2
       messageBrokerTest localBroker1 localBroker2
    where
      queues = [(testQueue1, transientQueueOpts)
               ,(testQueue2, transientQueueOpts)]
