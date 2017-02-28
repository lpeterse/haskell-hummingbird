{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Response where

import           Control.Monad
import qualified Data.Binary                       as B
import           Data.Int
import qualified Data.Text                         as T
import           GHC.Generics                      (Generic)
import           Network.MQTT.Session              (Connection,
                                                    connectionCleanSession,
                                                    connectionCreatedAt,
                                                    connectionRemoteAddress,
                                                    connectionSecure,
                                                    connectionWebSocket)

import           Hummingbird.Administration.Escape

data Response
   = Success
   | Failure String
   | Help
   | BrokerInfo
   { brokerVersion           :: String
   , brokerUptime            :: Int64
   , brokerSessionCount      :: Int
   , brokerSubscriptionCount :: Int
   }
   | Session SessionInfo
   | SessionList [SessionInfo]
   | SessionSubscriptions String
   deriving (Eq, Ord, Show, Generic)

data SessionInfo
   = SessionInfo
   { sessionIdentifier        :: Int
   , sessionClientIdentifier  :: T.Text
   , sessionConnection        :: Maybe Connection
   , sessionCreatedAt         :: Int64
   , sessionSubscriptionCount :: Int
   , sessionQueueQos0         :: (Int, Int)
   , sessionQueueQos1         :: (Int, Int)
   , sessionQueueQos2         :: (Int, Int)
   }
   deriving (Eq, Ord, Show, Generic)

instance B.Binary Response
instance B.Binary SessionInfo

render :: Monad m => (String -> m ()) -> Response -> m ()
render p Success =
  p (cyan "Success")

render p (Failure e) =
  p (lightRed e)

render p Help = do
    p "help                      : show this help"
    p "broker"
    p "  info                    : show broker information"
    p "sessions                  : list all sessions"
    p "  [0-9]+                  : show session summary"
    p "    disconnect            : disconnect associated client (if any)"
    p "    subscriptions         : show session subscriptions"
    p "    terminate             : terminate session (and disconnect client)"

render p info@BrokerInfo {} = do
  format "Version            " $ brokerVersion info
  format "Uptime             " $ ago (brokerUptime info)
  format "Sessions           " $ show (brokerSessionCount info)
  format "Subscriptions      " $ show (brokerSubscriptionCount info)
  format "Throughput         " "20,454/s 9,779/s 15,831/s"
  where
    format key value =
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m\STX"

render p (Session s) = do
  format "Created            " $ show (sessionCreatedAt s)
  format "Identifier         " $ show (sessionClientIdentifier s)
  case sessionConnection s of
    Nothing -> format "Connection         " $ lightRed "not connected"
    Just conn -> do
      p $ cyan "Connection"
      format "  Clean Session    " $ show (connectionCleanSession conn)
      format "  Created          " $ show (connectionCreatedAt conn)
      format "  Secure           " $ show (connectionSecure conn)
      format "  WebSocket        " $ show (connectionWebSocket conn)
      case connectionRemoteAddress conn of
        Nothing   -> pure ()
        Just addr -> format "  Remote Address   " $ show addr
  format "Subscriptions      " $ show (sessionSubscriptionCount s)
  p $ cyan   "Queues"
  format "  QoS 0            " $ show (fst $ sessionQueueQos0 s) ++ " / " ++ show (snd $ sessionQueueQos0 s)
  format "  QoS 1            " $ show (fst $ sessionQueueQos1 s) ++ " / " ++ show (snd $ sessionQueueQos1 s)
  format "  QoS 2            " $ show (fst $ sessionQueueQos2 s) ++ " / " ++ show (snd $ sessionQueueQos2 s)
  where
    format key value =
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m\STX"

render p (SessionList ss) =
  forM_ ss $ \session->
    p $ statusDot (sessionConnection session) ++
    leftPad 8 ' ' (show $ sessionIdentifier session) ++
    leftPad 30 ' ' (show $ sessionClientIdentifier session)
  where
    statusDot Nothing     = lightRed dot
    statusDot (Just conn) | connectionCleanSession conn = lightBlue dot
                          | otherwise                   = lightGreen dot

render p (SessionSubscriptions s) =
  p s
