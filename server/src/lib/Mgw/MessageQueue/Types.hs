{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Mgw.MessageQueue.Types
    (
      QueueName(..), QueueOpts(..), QueuePersistence(..), transientQueueOpts, persistentQueueOpts
    , SubscriberName(..), Subscriber(..), mkSubscriber, SubscriberId(..)
    , MessageId(..), Message(..)
    , MessageBroker(..), mb_lookupQueue
    , LogId(..), withLogId, logIdForNetworkApp
    , ServerMessage(..), ClientMessage(..)
    , messageBrokerTest, testQueue1, testQueue2, testQueue3
    , htf_thisModulesTests
    )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.MD5
import Mgw.Util.STM
import Mgw.Util.Tx
import Mgw.Util.Logging hiding (withLogId)
import Mgw.Util.Preview
import Mgw.Util.Sleep
import Mgw.Util.TimeSpan

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Conduit.Network (AppData(..))
import Data.Hashable
import Test.Framework
import Data.HashMap.Strict (HashMap)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as V

----------------------------------------
-- STDLIB
----------------------------------------

newtype QueueName = QueueName { unQueueName :: T.Text }
    deriving (Eq, Show, Hashable)

instance Preview QueueName where
    previewsPrec _ (QueueName name) =
        showString "QUEUE:" .
        showString (T.unpack name)

newtype SubscriberName = SubscriberName { unSubscriberName :: T.Text }
    deriving (Eq, Show, Hashable)

instance Preview SubscriberName where
    previewsPrec _ (SubscriberName name) =
        showString "SUBSCRIBER:" .
        showString (T.unpack name)

data Subscriber
    = Subscriber
      { sub_name :: !SubscriberName
      , sub_action :: !(Message -> IO ())
      }

mkSubscriber :: T.Text -> (Message -> IO ()) -> Subscriber
mkSubscriber name action =
    Subscriber (SubscriberName name) action

newtype MessageId = MessageId { unMessageId :: T.Text }
    deriving (Eq, Show, Hashable)

instance Preview MessageId where
    previewsPrec _ (MessageId x) =
        showString "MESSAGE:" .
        showString (T.unpack x)

data Message
    = Message
      { msg_id :: !MessageId
      , msg_payload :: !BS.ByteString
      }
    deriving (Eq, Show)

mkMessage :: T.Text -> BS.ByteString -> Message
mkMessage id payload =
    Message (MessageId id) payload

newtype SubscriberId = SubscriberId { _unSubscriberId :: Int }
    deriving (Eq, Show, Hashable)

instance Preview SubscriberId where
    previewsPrec _ (SubscriberId x) =
        showString "SUBSCRIBER_ID:" .
        showString (show x)

data MessageBroker q
    = MessageBroker
      { mb_queues :: !(HashMap QueueName q)
      , mb_subscribeToQueue :: q -> Subscriber -> STM SubscriberId
      , mb_unsubscribeFromQueue :: q -> SubscriberId -> STM ()
      , mb_publishMessage :: q -> Message -> IO ()
      }

mb_lookupQueue :: MessageBroker q -> QueueName -> Maybe q
mb_lookupQueue mb name =
    HashMap.lookup name (mb_queues mb)

data QueuePersistence
    = PersistentQueue
    | TransientQueue
      deriving (Eq, Show)

data QueueOpts
  = QueueOpts
    { qo_persistence :: QueuePersistence
    }
    deriving (Eq, Show)

transientQueueOpts :: QueueOpts
transientQueueOpts =
    QueueOpts
    { qo_persistence = TransientQueue
    }

persistentQueueOpts :: QueueOpts
persistentQueueOpts =
    QueueOpts
    { qo_persistence = PersistentQueue
    }

newtype LogId = LogId { unLogId :: T.Text }
    deriving (Eq)

instance Show LogId where
    showsPrec _ (LogId t) = showString (T.unpack t)

logIdForNetworkApp :: AppData m -> LogId
logIdForNetworkApp app =
    LogId $ T.pack $ take 6 $ show $ md5s
            (show (appSockAddr app) ++ show (appLocalAddr app))

withLogId :: LogId -> String -> String
withLogId clId msg =
    T.unpack (unLogId clId) ++ ": " ++ msg

data ServerMessage
   = ServerQueues !(V.Vector QueueName)
   | ServerPublishMessage !QueueName !Message
   deriving (Show, Eq)

data ClientMessage
   = ClientSubscribe !QueueName
   | ClientPublishMessage !QueueName !Message
   deriving (Show, Eq)

--
-- TESTS
--
testQueue1 :: QueueName
testQueue1 = QueueName "testQueue1"

testQueue2 :: QueueName
testQueue2 = QueueName "testQueue2"

testQueue3 :: QueueName
testQueue3 = QueueName "testQueue3"

-- Assumes that the given message brokers has testQueue1 and testQueue2.
messageBrokerTest :: MessageBroker q1 -> MessageBroker q2 -> IO ()
messageBrokerTest mb1 mb2 =
    do (q11, q12) <- subAssert $ lookupQueues mb1
       (q21, q22) <- subAssert $ lookupQueues mb2
       (subs1, v1) <- mkSub "sub1"
       (subs2, v2) <- mkSub "sub2"
       (subs3, v3) <- mkSub "sub3"
       subId <- runTx $ mb_subscribeToQueue mb1 q11 subs1
       _ <- runTx $ mb_subscribeToQueue mb1 q12 subs3
       sleepTimeSpan (milliseconds 100)
       mb_publishMessage mb2 q21 (mkMessage "msg1" "1st message to queue 1")
       mb_publishMessage mb2 q22 (mkMessage "msg2" "1st message to queue 2")
       sleepTimeSpan (milliseconds 100)
       runTx $ mb_unsubscribeFromQueue mb1 q11 subId
       _ <- runTx $ mb_subscribeToQueue mb1 q11 subs2
       sleepTimeSpan (milliseconds 100)
       mb_publishMessage mb2 q21 (mkMessage "msg3" "2nd message to queue 1")
       mb_publishMessage mb2 q22 (mkMessage "msg4" "2nd message to queue 2")
       sleepTimeSpan (milliseconds 100)
       runTx (readTVar v1) >>= assertEqual [MessageId "msg1"]
       runTx (readTVar v2) >>= assertEqual [MessageId "msg3"]
       runTx (readTVar v3) >>= assertEqual [MessageId "msg4", MessageId "msg2"]
    where
      mkSub :: T.Text -> IO (Subscriber, TVar [MessageId])
      mkSub name =
          do var <- newTVarIO []
             let action msg =
                     do runTx $ modifyTVar' var (msg_id msg:)
                        logInfo (preview name ++ " received " ++ preview (msg_id msg))
                 sub = mkSubscriber name action
             return (sub, var)
      lookupQueues mb =
          do q1 <- assertJust (mb_lookupQueue mb testQueue1)
             q2 <- assertJust (mb_lookupQueue mb testQueue2)
             return (q1, q2)
