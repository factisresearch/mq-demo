{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mgw.MessageQueue.LocalBroker
    ( createLocalBroker, LocalQueue, lq_name
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

data LocalQueue
   = LocalQueue
   { lq_subscribers :: TVar (HashMap SubscriberId Subscriber)
   , lq_nextSubscriberId :: TVar SubscriberId
   , lq_persist :: Message -> IO ()
   , lq_name :: !QueueName
   }

createLocalQueue :: Maybe FilePath -> QueueName -> QueueOpts -> STM LocalQueue
createLocalQueue mStoreDir name opts =
    do subs <- newTVar HashMap.empty
       nextId <- newTVar (SubscriberId 0)
       return $ LocalQueue
                  { lq_subscribers = subs
                  , lq_nextSubscriberId = nextId
                  , lq_persist = persistFun
                  , lq_name = name
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

subscribeToLocalQueue :: LogId -> LocalQueue -> Subscriber -> STM SubscriberId
subscribeToLocalQueue logId lq sub =
    do subId@(SubscriberId i) <- readTVar (lq_nextSubscriberId lq)
       writeTVar (lq_nextSubscriberId lq) $! (SubscriberId (i + 1))
       modifyTVar' (lq_subscribers lq) (HashMap.insert subId sub)
       logDebug (logMsg ("Subscription to local queue " ++ preview (lq_name lq) ++
                         ": " ++ preview subId))
       return subId
    where
      logMsg = withLogId logId

unsubscribeFromLocalQueue :: LogId -> LocalQueue -> SubscriberId -> STM ()
unsubscribeFromLocalQueue logId lq subId =
    do modifyTVar' (lq_subscribers lq) (HashMap.delete subId)
       logDebug (logMsg ("Unsubscription from local queue " ++ preview (lq_name lq) ++
                         ": " ++ preview subId))
    where
      logMsg = withLogId logId

publishMessageToLocalQueue :: LogId -> LocalQueue -> Message -> IO ()
publishMessageToLocalQueue logId lq msg =
    do subs <- runTx $ readTVar (lq_subscribers lq)
       logIOException ("Error persisting message " ++ preview (msg_id msg))
                      (lq_persist lq msg)
       mapM_ publish (HashMap.elems subs)
       logDebug (logMsg ("Published message " ++ preview (msg_id msg) ++
                         " to the following subscribers of local queue " ++
                         preview (lq_name lq) ++ ": " ++
                         show (map sub_name (HashMap.elems subs))))
    where
      logMsg = withLogId logId
      publish sub =
          sub_action sub msg `catchSafe` \exc ->
              logWarn ("Caught exception while publishing message " ++
                       show (msg_id msg) ++ " to subscriber " ++
                       show (sub_name sub) ++ ": " ++ show exc)

createLocalBroker ::
    LogId -> Maybe FilePath -> [(QueueName, QueueOpts)] -> IO (MessageBroker LocalQueue)
createLocalBroker logId storeDir queues =
    do queueList <- flip mapM queues $ \(name, opts) ->
                    do q <- runTx (createLocalQueue storeDir name opts)
                       return (name, q)
       let !queueMap = HashMap.fromList queueList
       return $ MessageBroker
                  { mb_queues = queueMap
                  , mb_subscribeToQueue = subscribeToLocalQueue logId
                  , mb_unsubscribeFromQueue = unsubscribeFromLocalQueue logId
                  , mb_publishMessage = publishMessageToLocalQueue logId
                  }

test_localBroker =
    do mb <- createLocalBroker (LogId "local") Nothing queues
       messageBrokerTest mb mb
    where
      queues = [(testQueue1, transientQueueOpts)
               ,(testQueue2, transientQueueOpts)]

deriveSafeCopy 1 'base ''Message
deriveSafeCopy 1 'base ''MessageId
