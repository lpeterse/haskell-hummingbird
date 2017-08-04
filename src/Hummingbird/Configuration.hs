{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
module Hummingbird.Configuration where
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor.Identity
import qualified Data.HashMap.Strict                as HM
import           Data.Int
import qualified Data.Map                           as M
import           Data.String
import qualified Data.Text                          as T
import           Data.Word
import qualified Data.Yaml                          as Yaml
import qualified System.Log.Logger                  as Log

import           Network.MQTT.Broker.Authentication
import qualified Network.MQTT.Trie                  as R

import qualified Hummingbird.Prometheus             as Prometheus

loadConfigFromFile :: (FromJSON (AuthenticatorConfig auth)) => FilePath -> IO (Either String (Config auth))
loadConfigFromFile path = do
  ec <- Yaml.decodeFileEither path
  pure $ case ec of
    Left e  -> Left (Yaml.prettyPrintParseException e)
    Right c -> Right c

data Config auth
   = Config
   { auth       :: AuthenticatorConfig auth
   , admin      :: AdminConfig
   , transports :: [ TransportConfig  ]
   , logging    :: LogConfig
   , prometheus :: Maybe Prometheus.Config
   }

instance Show (Config auth) where
  show _ = "TODO: Implement Show instance."

newtype AdminConfig
   = AdminConfig
   { adminSocketPath ::   FilePath
   } deriving (Eq, Ord, Show)

data ServerConfig
   = ServerConfig
   { srvMaxMessageSize :: Int64
   , srvTransport      :: TransportConfig
   }
  deriving (Eq, Ord, Show)

data TransportConfig
   = SocketTransport
     { bindAddress   :: T.Text
     , bindPort      :: Word16
     , listenBacklog :: Int
     }
   | WebSocketTransport
     { wsTransport             :: TransportConfig
     , wsFramePayloadSizeLimit :: Int64
     , wsMessageDataSizeLimit  :: Int64
     }
   | TlsTransport
     { tlsTransport      :: TransportConfig
     , tlsWantClientCert :: Bool
     , tlsCaFilePath     :: FilePath
     , tlsCrtFilePath    :: FilePath
     , tlsKeyFilePath    :: FilePath
     }
  deriving (Eq, Ord, Show)

data Privilege
   = Publish
   | Subscribe
   | Retain
   deriving (Eq, Ord, Show)

instance FromJSON Privilege where
  parseJSON (String "PUB") = pure Publish
  parseJSON (String "SUB") = pure Subscribe
  parseJSON (String "RET") = pure Retain
  parseJSON _              = fail "Expected 'PUB', 'SUB', or 'RET'."

instance FromJSON (R.Trie (Identity [Privilege])) where
  parseJSON (Object a) = R.Trie <$> HM.foldlWithKey' f (pure M.empty) a
    where
      f pm k v = do
        m    <- pm
        node <- parseJSON v
        pure $ M.insert (fromString $ T.unpack k) node m
  parseJSON invalid = typeMismatch "Trie" invalid

instance FromJSON (R.TrieNode (Identity [Privilege])) where
  parseJSON (Object v) = do
    subtree  <- v .:? "/" .!= R.empty
    mpubsub  <- v .:? "?"
    pure $ R.node subtree (Identity <$> mpubsub)
  parseJSON invalid = typeMismatch "TrieNode" invalid

data LogConfig
   = LogConfig
     { logLevel     :: Log.Priority
     , logAppenders :: [LogAppender]
     } deriving (Eq, Ord, Show)

data LogAppender
   = SyslogAppender
   | ConsoleAppender
   deriving (Eq, Ord, Show)

instance (FromJSON (AuthenticatorConfig auth)) => FromJSON (Config auth) where
  parseJSON (Object v) = Config
    <$> v .: "auth"
    <*> v .: "admin"
    <*> v .: "transports"
    <*> v .: "logging"
    <*> v .:? "prometheus"
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON LogConfig where
  parseJSON (Object v) = LogConfig
    <$> pLevel
    <*> v .: "appenders"
    where
      pLevel = do
        s <- v .: "level" :: Parser String
        case s of
          "DEBUG"     -> pure Log.DEBUG
          "INFO"      -> pure Log.INFO
          "NOTICE"    -> pure Log.NOTICE
          "WARNING"   -> pure Log.WARNING
          "ERROR"     -> pure Log.ERROR
          "CRITICAL"  -> pure Log.CRITICAL
          "ALERT"     -> pure Log.ALERT
          "EMERGENCY" -> pure Log.EMERGENCY
          _           -> fail "Expected DEBUG, INFO, WARNING, ERROR etc."
  parseJSON invalid = typeMismatch "LogConfig" invalid

instance FromJSON LogAppender where
  parseJSON (Object v) = do
    t <- v .: "type" :: Parser String
    case t of
      "syslog"  -> pure SyslogAppender
      "console" -> pure ConsoleAppender
      _         -> fail "Expected 'syslog' or 'console'."
  parseJSON invalid = typeMismatch "LogAppender" invalid

instance FromJSON AdminConfig where
  parseJSON (Object v) = AdminConfig
    <$> v .: "socketPath" .!= "/var/run/hummingbird/hummingbird.socket"
  parseJSON invalid = typeMismatch "AdminConfig" invalid

instance FromJSON TransportConfig where
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
      "tls" -> TlsTransport
        <$> v .: "transport"
        <*> v .: "wantClientCert"
        <*> v .: "caFilePath"
        <*> v .: "crtFilePath"
        <*> v .: "keyFilePath"
      _ -> fail "Expected 'socket', 'websocket' or 'tls'."
  parseJSON invalid = typeMismatch "TransportConfig" invalid
