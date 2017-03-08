{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.Administration.Sys
(
  sysInfoPublisher
) where

import           Control.Concurrent
import           Control.Monad        (forM_)
import           Data.String
import qualified System.Clock         as Clock

import qualified Network.MQTT.Broker  as Broker
import           Network.MQTT.Message (Message (..), QoS (..), Retain (..))

sysInfoPublisher :: Broker.Broker auth -> IO ()
sysInfoPublisher auth = forM_ [0..] $ \uptime-> do
  threadDelay 1000000
  time <- Clock.sec <$> Clock.getTime Clock.Realtime
  Broker.publishUpstream auth (uptimeMsg (uptime :: Int))
  Broker.publishUpstream auth (unixtimeMsg time)
    where
      uptimeMsg uptime = Message "$SYS/uptime" QoS0 (Retain False) (fromString $ show uptime)
      unixtimeMsg time = Message "$SYS/unixtime" QoS0 (Retain False) (fromString $ show time)
