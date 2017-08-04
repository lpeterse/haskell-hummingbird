module Hummingbird.Terminator ( run ) where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird.Terminator
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forever)
import           Network.MQTT.Broker

run :: Broker auth -> IO ()
run broker = forever $ do
  terminateExpiredSessions broker
  threadDelay 10000000
