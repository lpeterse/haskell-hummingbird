{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.SysInfo
  ( run
  ) where
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import           Control.Concurrent
import           Control.Monad        (forM_)
import           Data.String
import qualified System.Clock         as Clock

import qualified Network.MQTT.Broker  as Broker
import           Network.MQTT.Message (Message (..), QoS (..), Retain (..))

run :: Broker.Broker auth -> IO ()
run broker = forM_ [0..] $ \uptime-> do
  threadDelay 1000000
  time <- Clock.sec <$> Clock.getTime Clock.Realtime
  Broker.publishUpstream broker (uptimeMsg (uptime :: Int))
  Broker.publishUpstream broker (unixtimeMsg time)
    where
      uptimeMsg uptime = Message "$SYS/uptime"   QoS0 (Retain False) (fromString $ show uptime)
      unixtimeMsg time = Message "$SYS/unixtime" QoS0 (Retain False) (fromString $ show time)
