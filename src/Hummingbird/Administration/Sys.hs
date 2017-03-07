{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.Administration.Sys
(
  sysInfoPublisher
) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad            (forM_, forever)
import           Data.String
import qualified Network.MQTT.Broker      as Broker
import           Network.MQTT.Message     (Message (..), QoS (..), Retain (..))
import qualified System.Clock             as Clock

sysInfoPublisher :: Broker.Broker auth -> (Async () -> IO a) -> IO a
sysInfoPublisher auth = withAsync $ forM_ [0..] $ \uptime-> do
  threadDelay 2000000
  time <- Clock.sec <$> Clock.getTime Clock.Realtime
  Broker.publishUpstream auth (uptimeMsg (uptime :: Int))
  Broker.publishUpstream auth (unixtimeMsg time)
    where
      uptimeMsg uptime = Message { msgTopic  = "$SYS/uptime" , msgQoS = QoS0 , msgRetain = Retain False, msgPayload = fromString $ show uptime }
      unixtimeMsg time = Message { msgTopic  = "$SYS/unixtime" , msgQoS = QoS0 , msgRetain = Retain False, msgPayload = fromString $ show time }
      --unixtimeMsg time = Message (TF.Topic "$SYS/unixtime") QoS0 (Retain False) (fromString $ show time)
