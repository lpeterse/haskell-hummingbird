module Hummingbird.Transport ( runTransports ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.Default
import           Data.Int
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.X509.CertificateStore         as X509
import qualified Network.Stack.Server               as SS
import qualified Network.TLS                        as TLS
import qualified Network.TLS.Extra.Cipher           as TLS
import           System.Exit
import           System.IO
import qualified System.Socket                      as S
import qualified System.Socket.Family.Inet          as S
import qualified System.Socket.Protocol.Default     as S
import qualified System.Socket.Type.Stream          as S

import qualified Network.MQTT.Broker                as Broker
import           Network.MQTT.Broker.Authentication
import qualified Network.MQTT.Server                as Server

import           Hummingbird.Configuration

runTransports :: Authenticator auth => Broker.Broker auth -> [ TransportConfig ] -> IO ()
runTransports broker transportConfigs =
  void $ forConcurrently transportConfigs (runTransport broker)

runTransport :: Authenticator auth => Broker.Broker auth -> TransportConfig -> IO ()
runTransport broker transportConfig = case transportConfig of
  SocketTransport {} -> do
    cfg <- createSocketConfig transportConfig
    let mqttConfig = Server.MqttServerConfig {
        Server.mqttMaxMessageSize  = maxMessageSize,
        Server.mqttTransportConfig = cfg
      } :: SS.ServerConfig (Server.MQTT (S.Socket S.Inet S.Stream S.Default))
    runServerStack mqttConfig broker
  TlsTransport {} -> do
    cfg <- createSecureSocketConfig transportConfig
    let mqttConfig = Server.MqttServerConfig {
        Server.mqttMaxMessageSize  = maxMessageSize,
        Server.mqttTransportConfig = cfg
      }
    runServerStack mqttConfig broker
  WebSocketTransport socketConfig@SocketTransport {} -> do
    cfg <- createSocketConfig socketConfig
    let mqttConfig = Server.MqttServerConfig {
      Server.mqttMaxMessageSize  = maxMessageSize,
      Server.mqttTransportConfig = SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      }
    }
    runServerStack mqttConfig broker
  WebSocketTransport tlsConfig@TlsTransport {} -> do
    cfg <- createSecureSocketConfig tlsConfig
    let mqttConfig = Server.MqttServerConfig {
      Server.mqttMaxMessageSize  = maxMessageSize,
      Server.mqttTransportConfig = SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      }
    }
    runServerStack mqttConfig broker
  _ -> error "Server stack not implemented."
  where
    -- TODO: Shall be set per user after authentication
    maxMessageSize :: Int64
    maxMessageSize = 1000000

    createSocketConfig :: TransportConfig -> IO (SS.ServerConfig (S.Socket S.Inet S.Stream S.Default))
    createSocketConfig (SocketTransport a p b) = do
      (addrinfo:_) <- S.getAddressInfo (Just $ T.encodeUtf8 a) (Just $ T.encodeUtf8 $ T.pack $ show p) (mconcat [S.aiNumericHost, S.aiNumericService]) :: IO [S.AddressInfo S.Inet S.Stream S.Default]
      pure SS.SocketServerConfig {
            SS.socketServerConfigBindAddress = S.socketAddress addrinfo
          , SS.socketServerConfigListenQueueSize = b
        }
    createSocketConfig _ = error "not a socket config"
    createSecureSocketConfig :: TransportConfig -> IO (SS.ServerConfig (SS.TLS (S.Socket S.Inet S.Stream S.Default)))
    createSecureSocketConfig (TlsTransport tc cc ca crt key) = do
      mcs <- X509.readCertificateStore ca
      case mcs of
        Nothing -> do
          hPutStrLn stderr $ ca ++ ": cannot read/interpret."
          exitFailure
        Just cs -> do
          ecred <- TLS.credentialLoadX509 crt key
          case ecred of
            Left e -> error e
            Right credential -> do
              cfg <- createSocketConfig tc
              pure SS.TlsServerConfig {
                    SS.tlsTransportConfig = cfg
                  , SS.tlsServerParams    = def {
                      TLS.serverWantClientCert = cc
                    , TLS.serverCACertificates = X509.listCertificates cs
                    , TLS.serverShared = def {
                        TLS.sharedCredentials = TLS.Credentials [credential]
                      }
                    , TLS.serverSupported = def {
                        TLS.supportedVersions = [TLS.TLS12]
                      , TLS.supportedCiphers  = TLS.ciphersuite_all
                      }
                    }
                  }
    createSecureSocketConfig _ = error "not a tls config"

runServerStack :: (Authenticator auth, SS.StreamServerStack transport, Server.MqttServerTransportStack transport) => SS.ServerConfig (Server.MQTT transport) -> Broker.Broker auth -> IO ()
runServerStack serverConfig broker =
  SS.withServer serverConfig $ \server-> forever $ SS.withConnection server $ \connection info->
    Server.handleConnection broker serverConfig connection info
