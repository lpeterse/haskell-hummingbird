{-# LANGUAGE FlexibleContexts  #-}
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
import qualified System.Log.Logger                  as Log
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
  SocketTransport {} ->
    createSocketConfig transportConfig >>= runServerStack broker
  TlsTransport {} ->
    createSecureSocketConfig transportConfig >>= runServerStack broker
  WebSocketTransport { wsTransport = t@SocketTransport {} } ->
    createSocketConfig t >>= createWebSocketConfig transportConfig >>= runServerStack broker
  WebSocketTransport { wsTransport = t@TlsTransport {} } ->
    createSecureSocketConfig t >>= createWebSocketConfig transportConfig >>= runServerStack broker
  _ -> error "Server stack not implemented."
  where
    createSocketConfig :: Config -> IO (SS.ServerConfig (S.Socket S.Inet S.Stream S.Default), SS.ServerHooks (S.Socket S.Inet S.Stream S.Default))
    createSocketConfig (SocketTransport a p b l) = do
      (addrinfo:_) <- S.getAddressInfo (Just $ T.encodeUtf8 a) (Just $ T.encodeUtf8 $ T.pack $ show p) (mconcat [S.aiNumericHost, S.aiNumericService]) :: IO [S.AddressInfo S.Inet S.Stream S.Default]
      let cfg = SS.SocketServerConfig {
            SS.socketServerConfigBindAddress = S.socketAddress addrinfo
          , SS.socketServerConfigListenQueueSize = b
          , SS.socketServerConfigConnectionLimit = l
        }
      let hooks = SS.SocketServerHooks
      pure (cfg, hooks)
    createSocketConfig _ = error "not a socket config"

    createSecureSocketConfig :: Config -> IO (SS.ServerConfig (SS.Tls (S.Socket S.Inet S.Stream S.Default)), SS.ServerHooks (SS.Tls (S.Socket S.Inet S.Stream S.Default)))
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
              (cfg, hooks) <- createSocketConfig tc
              let cfg' = SS.TlsServerConfig {
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
              let hooks' = SS.TlsServerHooks {
                  SS.tlsTransportHooks = hooks
                , SS.tlsServerHooks = def
                }
              pure (cfg', hooks')
    createSecureSocketConfig _ = error "not a tls config"

    createWebSocketConfig :: Config -> (SS.ServerConfig a, SS.ServerHooks a) -> IO (SS.ServerConfig (SS.WebSocket a), SS.ServerHooks (SS.WebSocket a))
    createWebSocketConfig (WebSocketTransport { wsFramePayloadSizeLimit = fpsl, wsMessageDataSizeLimit = mdsl }) (cfg, hooks) = do
      let cfg' = SS.WebSocketServerConfig {
          SS.wsTransportConfig   = cfg
        , SS.wsConnectionOptions = WS.defaultConnectionOptions {
            WS.connectionFramePayloadSizeLimit = WS.SizeLimit fpsl
          , WS.connectionMessageDataSizeLimit = WS.SizeLimit mdsl
          }
        }
      let hooks' = SS.WebSocketServerHooks {
          SS.wsTransportHooks      = hooks
        , SS.wsOnConnectionRequest = const $ pure $ Right $ WS.AcceptRequest (Just "mqtt") []
        }
      pure (cfg', hooks')
    createWebSocketConfig _ _ = error "not a web socket config"

runServerStack :: (Authenticator auth, SS.ServerStack transport, SS.StreamOriented transport, Server.MqttServerTransportStack transport, Show (SS.ConnectionInfo transport)) => Broker.Broker auth -> (SS.ServerConfig transport, SS.ServerHooks transport) -> IO ()
runServerStack broker (config, hooks) =
  SS.withServer config $ \server->
    forever $ SS.serveForever server hooks $ \connection info-> do
      Log.debugM "Transport" $ "New incoming connection: " ++ show info
      Server.serveConnection broker connection info `catch` \e->
        Log.warningM "Transport" $ "Connection lost with exception: " ++ show (e :: SomeException)

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
        <*> v .:? "listenBacklog"         .!= 16
        <*> v .:? "connectionLimit"       .!= 1024
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
