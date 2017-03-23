{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Response where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Binary                           as B
import           Data.Int
import           Data.Maybe
import           Data.UUID                             (UUID)
import           GHC.Generics                          (Generic)
import           Network.MQTT.Broker.Authentication    (Principal (..),
                                                        Quota (..))
import           Network.MQTT.Broker.Session           (Connection,
                                                        SessionIdentifier,
                                                        SessionStatistic (..),
                                                        connectionCleanSession,
                                                        connectionCreatedAt,
                                                        connectionRemoteAddress,
                                                        connectionSecure,
                                                        connectionWebSocket)
import           Network.MQTT.Message                  (ClientIdentifier (..),
                                                        Username (..))
import           System.Clock

import           Hummingbird.Administration.Escape

data Response
   = Success String
   | Failure String
   | Help
   | BrokerInfo
   { brokerUptime            :: Int64
   , brokerSessionCount      :: Int
   , brokerSubscriptionCount :: Int
   }
   | Session SessionInfo
   | SessionList [SessionInfo]
   | SessionSubscriptions String
   deriving (Eq, Show, Generic)

data SessionInfo
   = SessionInfo
   { sessionIdentifier          :: SessionIdentifier
   , sessionCreatedAt           :: Int64
   , sessionClientIdentifier    :: ClientIdentifier
   , sessionPrincipalIdentifier :: UUID
   , sessionPrincipal           :: Principal
   , sessionConnection          :: Maybe Connection
   , sessionStatistic           :: SessionStatistic
   }
   deriving (Eq, Show, Generic)

instance B.Binary Response
instance B.Binary SessionInfo

render :: MonadIO m => (String -> m ()) -> Response -> m ()
render p (Success msg) =
  p (cyan msg)

render p (Failure msg) =
  p (lightRed msg)

render p Help = do
    p "help                      : show this help"
    p "broker                    : show broker information"
    p "config"
    p "  reload                  : reload configuration file (does not apply it!)"
--    p "auth"
--    p "  restart                 : restart the authentication module (after config file reload)"
    p "sessions                  : list all sessions"
    p "  [0-9]+                  : show session summary"
    p "    disconnect            : disconnect associated client (if any)"
    p "    subscriptions         : show session subscriptions"
    p "    terminate             : terminate session (and disconnect client)"
    p "transports                : show status of transports"
    p "  start                   : start transports"
    p "  stop                    : stop transports"

render p info@BrokerInfo {} = do
--  format "Version            " $ brokerVersion info
  format "Uptime             " $ ago (brokerUptime info)
  format "Sessions           " $ show (brokerSessionCount info)
  format "Subscriptions      " $ show (brokerSubscriptionCount info)
--  format "Throughput         " "20,454/s 9,779/s 15,831/s"
  where
    format key value =
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m"

render p (Session s) = do
  now <- liftIO $ sec <$> getTime Realtime
  format "Alive since                    " $ ago $ now - sessionCreatedAt s
  format "Client                         " $ escapeText clientIdentifier
  format "Principal                      " $ show (sessionPrincipalIdentifier s)
  case principalUsername (sessionPrincipal s) of
    Nothing -> pure ()
    Just (Username username) -> format "  Username                     " $ escapeText username
  p $ cyan "  Quota"
  format "    Max idle session TTL       " $ show (quotaMaxIdleSessionTTL $ principalQuota $ sessionPrincipal s)
  format "    Max packet size            " $ show (quotaMaxPacketSize $ principalQuota $ sessionPrincipal s)
  format "    Max packet identifiers     " $ show (quotaMaxPacketIdentifiers $ principalQuota $ sessionPrincipal s)
  format "    Max queue size QoS 0       " $ show (quotaMaxQueueSizeQoS0 $ principalQuota $ sessionPrincipal s)
  format "    Max queue size QoS 1       " $ show (quotaMaxQueueSizeQoS1 $ principalQuota $ sessionPrincipal s)
  format "    Max queue size QoS 2       " $ show (quotaMaxQueueSizeQoS2 $ principalQuota $ sessionPrincipal s)
  case sessionConnection s of
    Nothing -> format "Connection                     " $ lightRed "not connected"
    Just conn -> do
      p $ cyan "Connection"
      format "  Alive since                  " $ ago $ now - connectionCreatedAt conn
      format "  Clean Session                " $ show (connectionCleanSession conn)
      format "  Secure                       " $ show (connectionSecure conn)
      format "  WebSocket                    " $ show (connectionWebSocket conn)
      case connectionRemoteAddress conn of
        Nothing   -> pure ()
        Just addr -> format "  Remote Address               " $ escapeByteString addr
  p $ cyan "Statistic"
  format "  Publications  accepted       " $ show (ssPublicationsAccepted  $ sessionStatistic s)
  format "  Publications  dropped        " $ show (ssPublicationsDropped   $ sessionStatistic s)
  format "  Retentions    accepted       " $ show (ssRetentionsAccepted    $ sessionStatistic s)
  format "  Retentions    dropped        " $ show (ssRetentionsDropped     $ sessionStatistic s)
  format "  Subscriptions accepted       " $ show (ssSubscriptionsAccepted $ sessionStatistic s)
  format "  Subscriptions rejected       " $ show (ssSubscriptionsRejected $ sessionStatistic s)
  where
    ClientIdentifier clientIdentifier = sessionClientIdentifier s
    format key value =
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m"

render p (SessionList ss) = do
  now <- liftIO $ sec <$> getTime Realtime
  forM_ ss $ \session-> do
    let x1 = leftPad 8 ' '  $ show $ sessionIdentifier session
    let x2 = status (sessionConnection session)
    let x3 = leftPad 18 ' ' $ ago $ now - sessionCreatedAt session
    let x4 = leftPad 35 ' ' $ lightCyan $ show $ sessionPrincipalIdentifier session
    let x5 = leftPad 24 ' ' $ showClientIdentifier (sessionClientIdentifier session)
    let x6 = leftPad 12 ' ' $ fromMaybe "" $ escapeByteString <$> (sessionConnection session >>= connectionRemoteAddress)
    p $ unwords [x1,x2,x3,x4,x5,x6]
  where
    status Nothing                                   = lightRed   "IDLE"
    status (Just conn) | connectionCleanSession conn = lightBlue  "TEMP"
                       | otherwise                   = lightGreen "CONN"
    showClientIdentifier (ClientIdentifier s) = take 24 $ escapeText s

render p (SessionSubscriptions s) =
  p s
