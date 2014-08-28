{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Mgw.MessageQueue.Protocol
    ( parseClientMsg, parseServerMsg
    , serializeClientMsg, serializeServerMsg )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.MessageQueue.Types
import Mgw.Util.Logging
import Mgw.Util.Pb
import Mgw.Util.Preview
import qualified Com.Factisresearch.Checkpad.Protos.MessageQueue.ClientMessage as ClientMessage
import qualified Com.Factisresearch.Checkpad.Protos.MessageQueue.ClientSubscribe as ClientSubscribe
import qualified Com.Factisresearch.Checkpad.Protos.MessageQueue.PublishMessage as PublishMessage
import qualified Com.Factisresearch.Checkpad.Protos.MessageQueue.ServerMessage as ServerMessage
import qualified Com.Factisresearch.Checkpad.Protos.MessageQueue.ServerQueues as ServerQueues
import qualified Mgw.Util.Streams as Streams

----------------------------------------
-- SITE-PACKAGES
----------------------------------------

----------------------------------------
-- STDLIB
----------------------------------------
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V

parseClientMsg :: LogId
               -> Streams.InputStream BS.ByteString
               -> IO (Streams.InputStream ClientMessage)
parseClientMsg logId input =
    do pbufStream <- pbufParse (T.unpack (unLogId logId)) input
       Streams.concatMapM parseClientMsg pbufStream
    where
      parseClientMsg msg =
          case ClientMessage.subscribe msg of
            Just sub -> parseClientSubscribe sub
            Nothing ->
                case ClientMessage.publishMessage msg of
                  Just publ -> parsePublishMessage publ ClientPublishMessage
                  Nothing -> handleError msg
      parseClientSubscribe msg =
          case ClientSubscribe.queueName msg of
            Just utf8 ->
                return [(ClientSubscribe (QueueName (textFromUtf8 utf8)))]
            Nothing -> handleError msg

handleError msg =
    do logError ("Invalid protocol buffer message " ++
                 show msg)
       return []

parsePublishMessage msg f =
    case (PublishMessage.queueName msg
         ,PublishMessage.messageId msg
         ,PublishMessage.payload msg)
    of
      (Just name, Just id, Just payload) ->
          return [(f (QueueName (textFromUtf8 name))
                     (Message (MessageId (textFromUtf8 id))
                              (BSL.toStrict payload)))]
      _ ->
          handleError msg

parseServerMsg :: LogId
              -> Streams.InputStream BS.ByteString
              -> IO (Streams.InputStream ServerMessage)
parseServerMsg logId input =
    do pbufStream <- pbufParse (T.unpack (unLogId logId)) input
       Streams.concatMapM parseServerMsg pbufStream
    where
      parseServerMsg msg =
          case ServerMessage.queues msg of
            Just queues -> parseServerQueues queues
            Nothing ->
                case ServerMessage.publishMessage msg of
                  Just publ -> parsePublishMessage publ ServerPublishMessage
                  Nothing -> handleError msg
      parseServerQueues msg =
          let l = map textFromUtf8 (fromSeq (ServerQueues.queues msg))
          in return [ServerQueues (V.fromList (map QueueName l))]

instance Preview ServerMessage.ServerMessage where
    previewsPrec = showsPrec

instance Preview ClientMessage.ClientMessage where
    previewsPrec = showsPrec

serializeServerMsg :: LogId
                   -> Streams.OutputStream BS.ByteString
                   -> IO (Streams.OutputStream ServerMessage)
serializeServerMsg logId out =
    do pbufStream <- pbufSerialize (T.unpack (unLogId logId)) out
       Streams.contramap serialize pbufStream
    where
      serialize msg =
          case msg of
            ServerQueues queues ->
                defaultValue
                {
                  ServerMessage.queues =
                      Just $ defaultValue
                      {
                        ServerQueues.queues = mkTextSeq (map unQueueName (V.toList queues))
                      }
                }
            ServerPublishMessage queueName msg ->
                  defaultValue
                  {
                    ServerMessage.publishMessage =
                        Just $ serializePublishMessage queueName msg
                  }

serializePublishMessage queueName (Message msgId payload) =
    defaultValue
    {
      PublishMessage.queueName = Just (textToUtf8 (unQueueName queueName))
    , PublishMessage.messageId = Just (textToUtf8 (unMessageId msgId))
    , PublishMessage.payload = Just (BSL.fromStrict payload)
    }

serializeClientMsg :: LogId
                   -> Streams.OutputStream BS.ByteString
                   -> IO (Streams.OutputStream ClientMessage)
serializeClientMsg logId out =
    do pbufStream <- pbufSerialize (T.unpack (unLogId logId)) out
       Streams.contramap serialize pbufStream
    where
      serialize msg =
          case msg of
            ClientSubscribe queueName ->
                defaultValue
                {
                  ClientMessage.subscribe =
                      Just $ defaultValue
                      {
                        ClientSubscribe.queueName = Just (textToUtf8 (unQueueName queueName))
                      }
                }
            ClientPublishMessage queueName msg ->
                  defaultValue
                  {
                    ClientMessage.publishMessage =
                        Just $ serializePublishMessage queueName msg
                  }
