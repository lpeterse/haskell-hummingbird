{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Response where

import           Control.Monad
import qualified Data.Binary                       as B
import           Data.Int
import           Data.UUID (UUID)
import qualified Data.Text                         as T
import           GHC.Generics                      (Generic)
import           Network.MQTT.Authentication       (Quota(..), Principal (..))
import           Network.MQTT.Message              (ClientIdentifier)
import           Network.MQTT.Session              (Connection,
                                                    connectionCleanSession,
                                                    connectionCreatedAt,
                                                    connectionRemoteAddress,
                                                    connectionSecure,
                                                    connectionWebSocket)
import qualified Network.MQTT.SessionStatistics as SS

import           Hummingbird.Administration.Escape

data Response
   = Success String
   | Failure String
   | Help
   | BrokerInfo
   { brokerVersion           :: String
   , brokerUptime            :: Int64
   , brokerSessionCount      :: Int
   , brokerSubscriptionCount :: Int
   }
   | Session SessionInfo
   | SessionList [SessionListElement]
   | SessionSubscriptions String
   deriving (Eq, Ord, Show, Generic)

data SessionListElement
   = SessionListElement
   { lsessionIdentifier          :: Int
   , lsessionClientIdentifier    :: ClientIdentifier
   , lsessionPrincipalIdentifier :: UUID
   , lsessionConnection          :: Maybe Connection
   , lsessionCreatedAt           :: Int64
   } deriving (Eq, Ord, Show, Generic)

data SessionInfo
   = SessionInfo
   { sessionIdentifier          :: Int
   , sessionClientIdentifier    :: ClientIdentifier
   , sessionPrincipalIdentifier :: UUID
   , sessionCreatedAt           :: Int64
   , sessionConnection          :: Maybe Connection
   , sessionStatistics          :: SS.StatisticsSnapshot
   , sessionQuota               :: Quota
   }
   deriving (Eq, Ord, Show, Generic)

instance B.Binary Response
instance B.Binary SessionListElement
instance B.Binary SessionInfo

render :: Monad m => (String -> m ()) -> Response -> m ()
render p (Success msg) =
  p (cyan msg)

render p (Failure msg) =
  p (lightRed msg)

render p Help = do
    p "help                      : show this help"
    p "broker"
    p "  info                    : show broker information"
    p "sessions                  : list all sessions"
    p "  [0-9]+                  : show session summary"
    p "    disconnect            : disconnect associated client (if any)"
    p "    subscriptions         : show session subscriptions"
    p "    terminate             : terminate session (and disconnect client)"
    p "transports                : show status of transports"
    p "  start                   : start transports"
    p "  stop                    : stop transports"

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
  p $ cyan "Quota"
  format "  Max idle session TTL         " $ show (quotaSessionTTL $ sessionQuota s)
  format "  Max inflight messages        " $ show (quotaMaxInflightMessages $ sessionQuota s)
  format "  Max queue size QoS 0         " $ show (quotaMaxQueueSizeQoS0 $ sessionQuota s)
  format "  Max queue size QoS 1         " $ show (quotaMaxQueueSizeQoS1 $ sessionQuota s)
  format "  Max queue size QoS 2         " $ show (quotaMaxQueueSizeQoS2 $ sessionQuota s)
  p $ cyan "Statistics"
  format "  Messages published           " $ show (SS.messagesPublished $ sessionStatistics s)
  format "  Messages dropped             " $ show (SS.messagesDropped $ sessionStatistics s)
  where
    format key value =
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m\STX"

render p (SessionList ss) =
  forM_ ss $ \session->
    p $ statusDot (lsessionConnection session) ++
    leftPad 8 ' ' (show $ lsessionIdentifier session) ++
    leftPad 30 ' ' (show $ lsessionClientIdentifier session)
  where
    statusDot Nothing     = lightRed dot
    statusDot (Just conn) | connectionCleanSession conn = lightBlue dot
                          | otherwise                   = lightGreen dot

render p (SessionSubscriptions s) =
  p s
