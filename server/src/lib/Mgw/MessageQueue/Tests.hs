{-# OPTIONS_GHC -fno-warn-unused-binds  -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mgw.MessageQueue.Tests
    ( htf_thisModulesTests )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.MessageQueue.LocalBroker
import Mgw.MessageQueue.BrokerServer
import Mgw.MessageQueue.Types
import Mgw.Util.STM

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Test.Framework
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

----------------------------------------
-- STDLIB
----------------------------------------
{-
test_messageQueueLocal =
    do (_, broker) <-
           createMessageBroker Nothing [ (QueueName "persistent", QueueOpts True)
                                       , (QueueName "memory", QueueOpts False)
                                       ]
       basicBrokerTest (lb_getQueues broker)
-}
{-
test_messageQueueRemote =
    do (_, _localBroker) <-
           createMessageBroker (Just 9876)
                                   [ (QueueName "persistent", QueueOpts True)
                                   , (QueueName "memory", QueueOpts False)
                                   ]
       remoteBroker <- connectToMessageGateway "127.0.0.1" 9876
       print (HM.keys $ rb_getQueues remoteBroker)
       basicBrokerTest $ rb_getQueues remoteBroker
-}
{-
basicBrokerTest :: MessageQueue q => HM.HashMap QueueName q -> IO ()
basicBrokerTest channels =
    do broadcastVar <- newTVarIO Nothing
       let broadcastHandler chan msg =
               atomically $ writeTVar broadcastVar (Just (chan, msg))
           readResetBVar :: IO (Maybe (T.Text, Message))
           readResetBVar =
               do broadcastVal <-
                      atomically $ readTVar broadcastVar
                  atomically $ writeTVar broadcastVar $! Nothing
                  return broadcastVal
           withQueueOrError queueName fun =
               case HM.lookup (QueueName queueName) channels of
                 Just q ->
                     fun q
                 Nothing ->
                     safeError ("Queue " ++ show queueName ++ " doesn't exist!")
           memQ = "memory"
           perQ = "persistent"
           msg = Message "Hello!"
       withQueueOrError memQ $ \q ->
           do _ <- subscribeToQueue q (broadcastHandler memQ)
              return ()
       withQueueOrError memQ $ \q ->
           do publishMessageToQueue q msg
              broadcastVal <- readResetBVar
              assertEqual (Just (memQ, msg)) broadcastVal
       withQueueOrError perQ $ \q ->
           do publishMessageToQueue q msg
              broadcastVal <- readResetBVar
              assertEqual Nothing broadcastVal
       withQueueOrError perQ $ \q ->
           do sid <- subscribeToQueue q (broadcastHandler perQ)
              publishMessageToQueue q msg
              broadcastVal <- readResetBVar
              assertEqual (Just (perQ, msg)) broadcastVal
              unsubscribeFromQueue q sid
              publishMessageToQueue q msg
              broadcastVal' <- readResetBVar
              assertEqual Nothing broadcastVal'
       return ()
-}
