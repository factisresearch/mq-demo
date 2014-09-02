{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mgw.MessageQueue.LocalBroker
    ( createLocalBroker, Queue, q_name
    , htf_thisModulesTests )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.MessageQueue.Types
import Mgw.Util.Exception
import Mgw.Util.Logging hiding (withLogId)
import Mgw.Util.Misc
import Mgw.Util.Preview
import Mgw.Util.SafeCopy
import Mgw.Util.STM
import Mgw.Util.Tx

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Test.Framework
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

----------------------------------------
-- STDLIB
----------------------------------------
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString as BS

data Queue
   = Queue
   { q_subscribers :: TVar (HashMap SubscriberId Subscriber)
   , q_nextSubscriberId :: TVar SubscriberId
   , q_persist :: Message -> IO ()
   , q_name :: !QueueName
   }

createQueue :: Maybe FilePath -> QueueName -> QueueOpts -> STM Queue
createQueue mStoreDir name opts =
    do subs <- newTVar HashMap.empty
       nextId <- newTVar (SubscriberId 0)
       return $ Queue
                  { q_subscribers = subs
                  , q_nextSubscriberId = nextId
                  , q_persist = persistFun
                  , q_name = name
                  }
    where
      persistFun msg =
          case qo_persistence opts of
            TransientQueue -> return ()
            PersistentQueue ->
                case mStoreDir of
                  Nothing -> logError("Cannot persist message for queue " ++
                                      preview name ++ ": no store directory given")
                  Just storeDir ->
                    do let fname = map (\c -> if c == '/' then '_' else c)
                                       (T.unpack (unMessageId (msg_id msg)))
                           path = storeDir </> fname
                       mf <- findNonexistingFile False path
                       case mf of
                         Nothing -> logError ("Cannot find unique file name based on " ++ path)
                         Just f -> BS.writeFile f (safeEncode msg)

subscribeToQueue :: LogId -> Queue -> Subscriber -> STM SubscriberId
subscribeToQueue logId lq sub =
    do subId@(SubscriberId i) <- readTVar (q_nextSubscriberId lq)
       writeTVar (q_nextSubscriberId lq) $! (SubscriberId (i + 1))
       modifyTVar' (q_subscribers lq) (HashMap.insert subId sub)
       logDebug (logMsg ("Subscription to local queue " ++ preview (q_name lq) ++
                         ": " ++ preview subId))
       return subId
    where
      logMsg = withLogId logId

unsubscribeFromQueue :: LogId -> Queue -> SubscriberId -> STM ()
unsubscribeFromQueue logId lq subId =
    do modifyTVar' (q_subscribers lq) (HashMap.delete subId)
       logDebug (logMsg ("Unsubscription from local queue " ++ preview (q_name lq) ++
                         ": " ++ preview subId))
    where
      logMsg = withLogId logId

publishMessageToQueue :: LogId -> Queue -> Message -> IO ()
publishMessageToQueue logId lq msg =
    do subs <- runTx $ readTVar (q_subscribers lq)
       logIOException ("Error persisting message " ++ preview (msg_id msg))
                      (q_persist lq msg)
       mapM_ publish (HashMap.elems subs)
       logDebug (logMsg ("Published message " ++ preview (msg_id msg) ++
                         " to the following subscribers of local queue " ++
                         preview (q_name lq) ++ ": " ++
                         show (map sub_name (HashMap.elems subs))))
    where
      logMsg = withLogId logId
      publish sub =
          sub_action sub msg `catchSafe` \exc ->
              logWarn ("Caught exception while publishing message " ++
                       show (msg_id msg) ++ " to subscriber " ++
                       show (sub_name sub) ++ ": " ++ show exc)

createLocalBroker ::
    LogId -> Maybe FilePath -> [(QueueName, QueueOpts)] -> IO (MessageBroker Queue)
createLocalBroker logId storeDir queues =
    do queueList <- flip mapM queues $ \(name, opts) ->
                    do q <- runTx (createQueue storeDir name opts)
                       return (name, q)
       let !queueMap = HashMap.fromList queueList
       return $ MessageBroker
                  { mb_lookupQueue = \name ->
                        let mx = HashMap.lookup name queueMap
                        in return mx
                  , mb_knownQueues = return (HashMap.keys queueMap)
                  , mb_subscribeToQueue = subscribeToQueue logId
                  , mb_unsubscribeFromQueue = unsubscribeFromQueue logId
                  , mb_publishMessage = publishMessageToQueue logId
                  }

test_localBroker =
    do mb <- createLocalBroker (LogId "local") Nothing queues
       messageBrokerTest mb mb
    where
      queues = [(testQueue1, transientQueueOpts)
               ,(testQueue2, transientQueueOpts)]

deriveSafeCopy 1 'base ''Message
deriveSafeCopy 1 'base ''MessageId
