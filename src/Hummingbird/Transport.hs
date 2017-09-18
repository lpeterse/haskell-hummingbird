{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.Transport ( run, Config (..) ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default
import           Data.Int
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Word
import qualified Data.X509.CertificateStore         as X509
import qualified Network.Stack.Server               as SS
import qualified Network.TLS                        as TLS
import qualified Network.TLS.Extra.Cipher           as TLS
import qualified Network.WebSockets                 as WS
import qualified System.Log.Logger                  as LOG
import qualified System.Socket                      as S
import qualified System.Socket.Family.Inet          as S
import qualified System.Socket.Protocol.Default     as S
import qualified System.Socket.Type.Stream          as S

import qualified Network.MQTT.Broker                as Broker
import           Network.MQTT.Broker.Authentication
import qualified Network.MQTT.Broker.Server         as Server

run :: Authenticator auth => Broker.Broker auth -> [ Config ] -> IO ()
run broker transportConfigs =
  void $ forConcurrently transportConfigs (runTransport broker)
    `catch` \e-> do
      LOG.errorM "transports" $ "Transports thread died: " ++ show (e :: SomeException)
      throwIO e

runTransport :: Authenticator auth => Broker.Broker auth -> Config -> IO ()
runTransport broker transportConfig = case transportConfig of
  SocketTransport {} -> do
    cfg <- createSocketConfig transportConfig
    let mqttConfig = Server.MqttServerConfig {
        Server.mqttTransportConfig = cfg
      } :: SS.ServerConfig (Server.Mqtt (S.Socket S.Inet S.Stream S.Default))
    runServerStack mqttConfig broker
  TlsTransport {} -> do
    cfg <- createSecureSocketConfig transportConfig
    let mqttConfig = Server.MqttServerConfig {
        Server.mqttTransportConfig = cfg
      }
    runServerStack mqttConfig broker
  WebSocketTransport { wsTransport = tc@SocketTransport {}, wsFramePayloadSizeLimit = fpsl, wsMessageDataSizeLimit = mdsl } -> do
    cfg <- createSocketConfig tc
    let mqttConfig = Server.MqttServerConfig {
      Server.mqttTransportConfig = SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      , SS.wsConnectionOptions = WS.defaultConnectionOptions {
          WS.connectionFramePayloadSizeLimit = WS.SizeLimit fpsl
        , WS.connectionMessageDataSizeLimit = WS.SizeLimit mdsl
        }
      }
    }
    runServerStack mqttConfig broker
  WebSocketTransport { wsTransport = tc@TlsTransport {}, wsFramePayloadSizeLimit = fpsl, wsMessageDataSizeLimit = mdsl } -> do
    cfg <- createSecureSocketConfig tc
    let mqttConfig = Server.MqttServerConfig {
      Server.mqttTransportConfig = SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      , SS.wsConnectionOptions = WS.defaultConnectionOptions {
          WS.connectionFramePayloadSizeLimit = WS.SizeLimit fpsl
        , WS.connectionMessageDataSizeLimit = WS.SizeLimit mdsl
        }
      }
    }
    runServerStack mqttConfig broker
  _ -> error "Server stack not implemented."
  where
    createSocketConfig :: Config -> IO (SS.ServerConfig (S.Socket S.Inet S.Stream S.Default))
    createSocketConfig (SocketTransport a p b l) = do
      (addrinfo:_) <- S.getAddressInfo (Just $ T.encodeUtf8 a) (Just $ T.encodeUtf8 $ T.pack $ show p) (mconcat [S.aiNumericHost, S.aiNumericService]) :: IO [S.AddressInfo S.Inet S.Stream S.Default]
      pure SS.SocketServerConfig {
            SS.socketServerConfigBindAddress = S.socketAddress addrinfo
          , SS.socketServerConfigListenQueueSize = b
          , SS.socketServerConfigConnectionLimit = l
        }
    createSocketConfig _ = error "not a socket config"
    createSecureSocketConfig :: Config -> IO (SS.ServerConfig (SS.Tls (S.Socket S.Inet S.Stream S.Default)))
    createSecureSocketConfig (TlsTransport tc cc ca crt key) = do
      mcs <- X509.readCertificateStore ca
      case mcs of
        Nothing ->
          error $ show ca ++ ": cannot read/interpret."
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
                      TLS.supportedCiphers =
                        TLS.ciphersuite_default
                      , TLS.supportedVersions =
                          [ TLS.TLS12 ]
                      , TLS.supportedHashSignatures =
                          [ (TLS.HashSHA384, TLS.SignatureRSA)
                          , (TLS.HashSHA384, TLS.SignatureECDSA)
                          , (TLS.HashSHA256, TLS.SignatureRSA)
                          , (TLS.HashSHA256, TLS.SignatureECDSA)
                          ]
                      }
                    }
                  }
    createSecureSocketConfig _ = error "not a tls config"

runServerStack :: (Authenticator auth, SS.StreamServerStack transport, Server.MqttServerTransportStack transport) => SS.ServerConfig (Server.Mqtt transport) -> Broker.Broker auth -> IO ()
runServerStack serverConfig broker =
  SS.withServer serverConfig $ \server->
    SS.serveForever server $ \connection info->
       Server.serveConnection broker connection info

-------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------

data Config
   = SocketTransport
     { bindAddress     :: T.Text
     , bindPort        :: Word16
     , listenBacklog   :: Int
     , connectionLimit :: Int
     }
   | WebSocketTransport
     { wsTransport             :: Config
     , wsFramePayloadSizeLimit :: Int64
     , wsMessageDataSizeLimit  :: Int64
     }
   | TlsTransport
     { tlsTransport      :: Config
     , tlsWantClientCert :: Bool
     , tlsCaFilePath     :: FilePath
     , tlsCrtFilePath    :: FilePath
     , tlsKeyFilePath    :: FilePath
     }
  deriving (Eq, Ord, Show)

instance FromJSON Config where
  parseJSON (Object v) = do
    t <- v .: "type" :: Parser String
    case t of
      "websocket" -> WebSocketTransport
        <$> v .: "transport"
        <*> v .:? "framePayloadSizeLimit" .!= 65535
        <*> v .:? "messageDataSizeLimit"  .!= 65535
      "socket" -> SocketTransport
        <$> v .: "bindAddress"
        <*> v .: "bindPort"
        <*> v .: "listenBacklog"
        <*> v .: "connectionLimit"
      "tls" -> TlsTransport
        <$> v .: "transport"
        <*> v .: "wantClientCert"
        <*> v .: "caFilePath"
        <*> v .: "crtFilePath"
        <*> v .: "keyFilePath"
      _ -> fail "Expected 'socket', 'websocket' or 'tls'."
  parseJSON invalid = typeMismatch "Config" invalid
