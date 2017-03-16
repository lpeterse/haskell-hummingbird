{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Response where

import           Control.Monad
import           Data.Maybe
import           Data.List (intercalate)
import qualified Data.Binary                       as B
import           Control.Monad.IO.Class
import           Data.Int
import           Data.UUID                         (UUID)
import           GHC.Generics                      (Generic)
import           System.Clock
import           Network.MQTT.Broker.Authentication       (Principal (..), Quota (..))
import           Network.MQTT.Message              (ClientIdentifier (..), Username (..))
import           Network.MQTT.Broker.Session              (Connection,
                                                    connectionCleanSession,
                                                    connectionCreatedAt,
                                                    connectionRemoteAddress,
                                                    connectionSecure,
                                                    connectionWebSocket)
import qualified Network.MQTT.Broker.SessionStatistics    as SS

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
   { sessionIdentifier          :: Int
   , sessionCreatedAt           :: Int64
   , sessionClientIdentifier    :: ClientIdentifier
   , sessionPrincipalIdentifier :: UUID
   , sessionPrincipal           :: Principal
   , sessionConnection          :: Maybe Connection
   , sessionStatistics          :: SS.StatisticsSnapshot
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
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m\STX"

render p (Session s) = do
  now <- liftIO $ sec <$> getTime Realtime
  format "Alive since                    " $ ago $ now - sessionCreatedAt s
  format "Client                         " $ showEscaped clientIdentifier
  format "Principal                      " $ showEscaped (sessionPrincipalIdentifier s)
  case principalUsername (sessionPrincipal s) of
    Nothing -> pure ()
    Just (Username username) -> format "  Username                     " $ showEscaped username
  p $ cyan "  Quota"
  format "    Max idle session TTL       " $ show (quotaSessionTTL $ principalQuota $ sessionPrincipal s)
  format "    Max inflight messages      " $ show (quotaMaxInflightMessages $ principalQuota $ sessionPrincipal s)
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
        Just addr -> format "  Remote Address               " $ showEscaped addr
  p $ cyan "Statistics"
  format "  Publications  accepted       " $ show (SS.publicationsAccepted  $ sessionStatistics s)
  format "  Publications  dropped        " $ show (SS.publicationsDropped   $ sessionStatistics s)
  format "  Subscriptions accepted       " $ show (SS.subscriptionsAccepted $ sessionStatistics s)
  format "  Subscriptions denied         " $ show (SS.subscriptionsDenied   $ sessionStatistics s)
  format "  Retentions    accepted       " $ show (SS.retentionsAccepted    $ sessionStatistics s)
  format "  Retentions    dropped        " $ show (SS.retentionsDropped     $ sessionStatistics s)
  where
    ClientIdentifier clientIdentifier = sessionClientIdentifier s
    format key value =
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m\STX"


render p (SessionList ss) = do
  now <- liftIO $ sec <$> getTime Realtime
  forM_ ss $ \session-> do
    let x1 = leftPad 8 ' '  $ show $ sessionIdentifier session
    let x2 = status (sessionConnection session)
    let x3 = leftPad 12 ' ' $ ago $ now - sessionCreatedAt session
    let x4 = leftPad 35 ' ' $ lightCyan $ showEscaped $ sessionPrincipalIdentifier session
    let x5 = leftPad 24 ' ' $ showClientIdentifier (sessionClientIdentifier session)
    let x6 = leftPad 12 ' ' $ fromMaybe "" $ showEscaped <$> (sessionConnection session >>= connectionRemoteAddress)
    p $ unwords [x1,x2,x3,x4,x5,x6]
  where
    status Nothing                                   = lightRed   "DISCONNECTED"
    status (Just conn) | connectionCleanSession conn = lightBlue  "CONNECTED   "
                       | otherwise                   = lightGreen "CONNECTED+  "
    showClientIdentifier (ClientIdentifier s) = take 24 $ showEscaped s

render p (SessionSubscriptions s) =
  p s

showEscaped :: Show a => a -> String
showEscaped = init . tail . show
