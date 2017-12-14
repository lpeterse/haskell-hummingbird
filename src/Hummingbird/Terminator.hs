module Hummingbird.Terminator ( run ) where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forever)
import           Network.MQTT.Broker

run :: Broker auth -> IO ()
run broker = forever $ do
  terminateExpiredSessions broker
  threadDelay 10000000
