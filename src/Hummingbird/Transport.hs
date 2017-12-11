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
import           Data.Typeable
import           Data.Word
import qualified Data.X509.CertificateStore         as X509
import qualified Network.TLS                        as TLS
import qualified Network.TLS.Extra.Cipher           as TLS
import qualified Network.WebSockets                 as WS
import qualified Networking                         as SS
import qualified Networking.Socket                  as SS
import qualified Networking.TLS                     as SS
import qualified Networking.WebSocket               as SS
import qualified System.Log.Logger                  as LOG
import qualified System.Socket                      as S
import qualified System.Socket.Family.Inet          as S
import qualified System.Socket.Protocol.Default     as S
import qualified System.Socket.Type.Stream          as S

import qualified Network.MQTT.Broker                as Broker
import           Network.MQTT.Broker.Authentication
import qualified Network.MQTT.Broker.Server         as Server
import           Network.MQTT.Message               (ClientIdentifier (..))

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
    runServerStack broker (cfg :: SS.ServerConfig (S.Socket S.Inet S.Stream S.Default))
  TlsTransport {} -> do
    cfg <- createSecureSocketConfig transportConfig
    runServerStack broker cfg
  WebSocketTransport { wsTransport = tc@SocketTransport {}, wsFramePayloadSizeLimit = fpsl, wsMessageDataSizeLimit = mdsl } -> do
    cfg <- createSocketConfig tc
    runServerStack broker $ SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      , SS.wsConnectionOptions = WS.defaultConnectionOptions {
          WS.connectionFramePayloadSizeLimit = WS.SizeLimit fpsl
        , WS.connectionMessageDataSizeLimit = WS.SizeLimit mdsl
        }
      }
  WebSocketTransport { wsTransport = tc@TlsTransport {}, wsFramePayloadSizeLimit = fpsl, wsMessageDataSizeLimit = mdsl } -> do
    cfg <- createSecureSocketConfig tc
    runServerStack broker $ SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      , SS.wsConnectionOptions = WS.defaultConnectionOptions {
          WS.connectionFramePayloadSizeLimit = WS.SizeLimit fpsl
        , WS.connectionMessageDataSizeLimit = WS.SizeLimit mdsl
        }
      }
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

runServerStack :: (Authenticator auth, SS.ServerStack transport, SS.StreamOriented transport, Server.MqttServerTransportStack transport) => Broker.Broker auth -> SS.ServerConfig transport -> IO ()
runServerStack broker config =
  SS.withServer config $ \server->
    forever $ SS.serveForever server $ \connection info->
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

instance (Typeable f, Typeable t, Typeable p, S.Family f, S.Protocol p, S.Type t, S.HasNameInfo f) => Server.MqttServerTransportStack (S.Socket f t p) where
  getConnectionRequest (SS.SocketConnectionInfo addr) = do
    remoteAddr <- S.hostName <$> S.getNameInfo addr (S.niNumericHost `mappend` S.niNumericService)
    pure ConnectionRequest {
        requestClientIdentifier = ClientIdentifier mempty
      , requestSecure = False
      , requestCleanSession = True
      , requestCredentials = Nothing
      , requestHttp = Nothing
      , requestCertificateChain = Nothing
      , requestRemoteAddress = Just remoteAddr
      , requestWill = Nothing
      }

instance (SS.ServerStack a, Server.MqttServerTransportStack a) => Server.MqttServerTransportStack (SS.WebSocket a) where
  getConnectionRequest (SS.WebSocketConnectionInfo tci rh) = do
    req <- Server.getConnectionRequest tci
    pure req {
        requestHttp = Just (WS.requestPath rh, WS.requestHeaders rh)
      }

instance (SS.ServerStack a, Server.MqttServerTransportStack a) => Server.MqttServerTransportStack (SS.Tls a) where
  getConnectionRequest (SS.TlsConnectionInfo tci mcc) = do
    req <- Server.getConnectionRequest tci
    pure req {
        requestSecure = True
      , requestCertificateChain = mcc
      }
