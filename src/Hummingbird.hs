{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hummingbird
  ( runCommandLine, runWithConfig ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Crypto.BCrypt                  as BCrypt
import           Data.Aeson
import qualified Data.ByteString                as BS
import           Data.Default
import           Data.Default.Class
import           Data.Proxy
import           Data.String
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.X509.CertificateStore     as X509
import           Network.MQTT.Authentication
import qualified Network.MQTT.Broker            as Broker
import           Network.MQTT.Message
import qualified Network.MQTT.Server            as Server
import qualified Network.Stack.Server           as SS
import qualified Network.TLS                    as TLS
import qualified Network.TLS.Extra.Cipher       as TLS
import           Options
import qualified System.Clock                   as Clock
import           System.Exit
import           System.IO
import qualified System.Log.Formatter           as LOG
import qualified System.Log.Handler             as LOG hiding (setLevel)
import qualified System.Log.Handler.Simple      as LOG
import qualified System.Log.Handler.Syslog      as LOG
import qualified System.Log.Logger              as LOG
import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet      as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S

import qualified Hummingbird.AdminInterface     as Admin
import           Hummingbird.Configuration

data MainOptions = MainOptions

data CliOptions = CliOptions
  { cliSocketPath :: FilePath }

data ServerOptions = ServerOptions
  { serverConfigFilePath :: FilePath }

data PwhashOptions = PwhashOptions

instance Options MainOptions where
  defineOptions = pure MainOptions

instance Options CliOptions where
  defineOptions = CliOptions
    <$> simpleOption "socket" "~/.hummingbird.socket" "Path to the servers administration socket (unix domain socket)"

instance Options ServerOptions where
  defineOptions = ServerOptions
    <$> simpleOption "config" "settings.yaml" "Path to .yaml configuration file"

instance Options PwhashOptions where
  defineOptions = pure PwhashOptions

runCommandLine :: (Authenticator auth, FromJSON (AuthenticatorConfig auth)) => Proxy (Config auth) -> IO ()
runCommandLine authConfigProxy = runSubcommand
  [ subcommand "cli"    cliCommand
  , subcommand "pwhash" pwhashCommand
  , subcommand "server" serverCommand
  ]
  where
    cliCommand    :: MainOptions -> CliOptions -> [String] -> IO ()
    cliCommand _ opts _ = Admin.runCommandLineInterface (cliSocketPath opts)

    serverCommand :: MainOptions -> ServerOptions -> [String] -> IO ()
    serverCommand _ opts _ = do
      ec <- loadConfigFromFile (serverConfigFilePath opts)
      case ec of
        Left e    -> hPutStrLn stderr e >> exitFailure
        Right cfg -> runWithConfig (cfg `asProxyTypeOf` authConfigProxy)

    pwhashCommand :: MainOptions -> PwhashOptions -> [String] -> IO ()
    pwhashCommand _ _ _ = do
      hSetEcho stdin False
      password <- BS.getLine
      mhash <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy password
      case mhash of
        Nothing   -> exitFailure
        Just hash -> BS.putStrLn hash

runWithConfig :: (Authenticator auth) => Config auth -> IO ()
runWithConfig conf = do
  LOG.removeAllHandlers
  LOG.updateGlobalLogger LOG.rootLoggerName (LOG.setLevel $ logLevel $ logging conf)
  forM_ (logAppenders $ logging conf) $ \appender->
   case appender of
     SyslogAppender  -> do
       s <- LOG.openlog "hummingbird" [LOG.PID] LOG.USER LOG.DEBUG
       LOG.updateGlobalLogger LOG.rootLoggerName (LOG.addHandler s)
     ConsoleAppender -> do
       lh <- LOG.streamHandler stderr LOG.DEBUG
       let h = LOG.setFormatter lh (LOG.simpleLogFormatter "[$time : $loggername : $prio] $msg")
       LOG.updateGlobalLogger LOG.rootLoggerName (LOG.addHandler h)
  LOG.infoM "hummingbird" "Started hummingbird MQTT message broker."
  authenticator <- newAuthenticator (auth conf)
  broker <- Broker.new authenticator
  -- The following background tasks are forked off as Asyncs.
  -- They will be cancelled automatically and won't outlive the main thread.
  withSysTopicThread broker $ \_->
    Admin.withAdminInterfaceThread broker $ \_->
      forConcurrently_ (servers conf) (runServerWithConfig broker)

runServerWithConfig :: Authenticator auth => Broker.Broker auth -> ServerConfig -> IO ()
runServerWithConfig broker serverConfig = case srvTransport serverConfig of
  SocketTransport {} -> do
    cfg <- createSocketConfig (srvTransport serverConfig)
    let mqttConfig = Server.MqttServerConfig {
        Server.mqttMaxMessageSize  = srvMaxMessageSize serverConfig,
        Server.mqttTransportConfig = cfg
      } :: SS.ServerConfig (Server.MQTT (S.Socket S.Inet S.Stream S.Default))
    runServerStack mqttConfig broker
  TlsTransport {} -> do
    cfg <- createSecureSocketConfig (srvTransport serverConfig)
    let mqttConfig = Server.MqttServerConfig {
        Server.mqttMaxMessageSize  = srvMaxMessageSize serverConfig,
        Server.mqttTransportConfig = cfg
      }
    runServerStack mqttConfig broker
  WebSocketTransport socketConfig@SocketTransport {} -> do
    cfg <- createSocketConfig socketConfig
    let mqttConfig = Server.MqttServerConfig {
      Server.mqttMaxMessageSize  = srvMaxMessageSize serverConfig,
      Server.mqttTransportConfig = SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      }
    }
    runServerStack mqttConfig broker
  WebSocketTransport tlsConfig@TlsTransport {} -> do
    cfg <- createSecureSocketConfig tlsConfig
    let mqttConfig = Server.MqttServerConfig {
      Server.mqttMaxMessageSize  = srvMaxMessageSize serverConfig,
      Server.mqttTransportConfig = SS.WebSocketServerConfig {
        SS.wsTransportConfig = cfg
      }
    }
    runServerStack mqttConfig broker
  _ -> error "Server stack not implemented."
  where
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

withSysTopicThread :: Broker.Broker auth -> (Async () -> IO a) -> IO a
withSysTopicThread broker = withAsync $ forM_ [0..] $ \uptime-> do
  threadDelay 2000000
  time <- Clock.sec <$> Clock.getTime Clock.Realtime
  Broker.publishUpstream' broker (uptimeMsg (uptime :: Int))
  Broker.publishUpstream' broker (unixtimeMsg time)
  where
    uptimeMsg uptime = Message "$SYS/uptime" (fromString $ show uptime) Qos0 False
    unixtimeMsg time = Message "$SYS/unixtime" (fromString $ show time) Qos0 False
