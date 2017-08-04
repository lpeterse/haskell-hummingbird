{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
module Hummingbird.Configuration where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird.Configuration
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
import qualified Data.Yaml                          as Yaml

import           Network.MQTT.Broker.Authentication
import qualified Network.MQTT.Trie                  as R

import qualified Hummingbird.Logging                as Logging
import qualified Hummingbird.Prometheus             as Prometheus
import qualified Hummingbird.Transport              as Transport

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
   , transports :: [ Transport.Config  ]
   , logging    :: Logging.Config
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
   , srvTransport      :: Transport.Config
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

instance (FromJSON (AuthenticatorConfig auth)) => FromJSON (Config auth) where
  parseJSON (Object v) = Config
    <$> v .: "auth"
    <*> v .: "admin"
    <*> v .: "transports"
    <*> v .: "logging"
    <*> v .:? "prometheus"
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON AdminConfig where
  parseJSON (Object v) = AdminConfig
    <$> v .: "socketPath" .!= "/var/run/hummingbird/hummingbird.socket"
  parseJSON invalid = typeMismatch "AdminConfig" invalid

