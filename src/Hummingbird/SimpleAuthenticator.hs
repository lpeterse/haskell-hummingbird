{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hummingbird.SimpleAuthenticator where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird.SimpleAuthenticator
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import           Control.Exception
import qualified Crypto.BCrypt                      as BCrypt
import           Data.Aeson                         (FromJSON (..), (.:?))
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString         as AP
import qualified Data.ByteString                    as BS
import           Data.Functor.Identity
import qualified Data.Map                           as M
import           Data.Maybe
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Typeable
import           Data.UUID                          (UUID)

import           Network.MQTT.Broker.Authentication
import           Network.MQTT.Message
import qualified Network.MQTT.Trie                  as R

import qualified Hummingbird.Configuration          as C

data SimpleAuthenticator
   = SimpleAuthenticator
   { authPrincipals   :: M.Map UUID SimplePrincipalConfig
   , authDefaultQuota :: Quota
   } deriving (Eq, Show)

data SimplePrincipalConfig
   = SimplePrincipalConfig
   { cfgUsername     :: Maybe T.Text
   , cfgPasswordHash :: Maybe BS.ByteString
   , cfgQuota        :: Maybe SimpleQuotaConfig
   , cfgPermissions  :: M.Map Filter (Identity [C.Privilege])
   } deriving (Eq, Show)

data SimpleQuotaConfig
   = SimpleQuotaConfig
   { cfgQuotaMaxSessions          :: Maybe Int
   , cfgQuotaMaxIdleSessionTTL    :: Maybe Int
   , cfgQuotaMaxPacketSize        :: Maybe Int
   , cfgQuotaMaxPacketIdentifiers :: Maybe Int
   , cfgQuotaMaxQueueSizeQoS0     :: Maybe Int
   , cfgQuotaMaxQueueSizeQoS1     :: Maybe Int
   , cfgQuotaMaxQueueSizeQoS2     :: Maybe Int
   } deriving (Eq, Ord, Show)

instance Authenticator SimpleAuthenticator where
  data AuthenticatorConfig SimpleAuthenticator
     = SimpleAuthenticatorConfig
       { cfgPrincipals   :: M.Map UUID SimplePrincipalConfig
       , cfgDefaultQuota :: Quota
       }
  data AuthenticationException SimpleAuthenticator
     = SimpleAuthenticationException deriving (Eq, Ord, Show, Typeable)

  newAuthenticator config = SimpleAuthenticator
    <$> pure (cfgPrincipals config)
    <*> pure (cfgDefaultQuota config)

  authenticate auth req =
    pure $ case requestCredentials req of
      Just (reqUser, Just reqPass) ->
        case mapMaybe (byUsernameAndPassword reqUser reqPass) $ M.assocs (authPrincipals auth) of
          [(uuid, _)] -> Just uuid
          _           -> Nothing
      _ -> Nothing
    where
      byUsernameAndPassword (Username reqUser) (Password reqPass) p@(_, principal) = do
        -- Maybe monad - yields Nothing on failure!
        user <- cfgUsername principal
        pwhash <- cfgPasswordHash principal
        -- The user is authenticated if username _and_ supplied password match.
        if user == reqUser && BCrypt.validatePassword pwhash reqPass
          then Just p
          else Nothing

  getPrincipal auth pid =
    case M.lookup pid (authPrincipals auth) of
      Nothing -> pure Nothing
      Just pc -> pure $ Just Principal {
          principalUsername             = Username <$> cfgUsername pc
        , principalQuota                = mergeQuota (cfgQuota pc) (authDefaultQuota auth)
        , principalPublishPermissions   = R.mapMaybe f $ M.foldrWithKey' R.insert R.empty (cfgPermissions pc)
        , principalSubscribePermissions = R.mapMaybe g $ M.foldrWithKey' R.insert R.empty (cfgPermissions pc)
        , principalRetainPermissions    = R.mapMaybe h $ M.foldrWithKey' R.insert R.empty (cfgPermissions pc)
        }
    where
      f (Identity xs)
        | C.Publish `elem` xs   = Just ()
        | otherwise             = Nothing
      g (Identity xs)
        | C.Subscribe `elem` xs = Just ()
        | otherwise             = Nothing
      h (Identity xs)
        | C.Retain    `elem` xs = Just ()
        | otherwise             = Nothing
      -- Prefers a user quota property over the default quota property.
      mergeQuota Nothing defaultQuota = defaultQuota
      mergeQuota (Just quota) defaultQuota = Quota {
          quotaMaxSessions          = fromMaybe (quotaMaxSessions          defaultQuota) (cfgQuotaMaxSessions          quota)
        , quotaMaxIdleSessionTTL    = fromMaybe (quotaMaxIdleSessionTTL    defaultQuota) (cfgQuotaMaxIdleSessionTTL    quota)
        , quotaMaxPacketSize        = fromMaybe (quotaMaxPacketSize        defaultQuota) (cfgQuotaMaxPacketSize        quota)
        , quotaMaxPacketIdentifiers = fromMaybe (quotaMaxPacketIdentifiers defaultQuota) (cfgQuotaMaxPacketIdentifiers quota)
        , quotaMaxQueueSizeQoS0     = fromMaybe (quotaMaxQueueSizeQoS0     defaultQuota) (cfgQuotaMaxQueueSizeQoS0     quota)
        , quotaMaxQueueSizeQoS1     = fromMaybe (quotaMaxQueueSizeQoS1     defaultQuota) (cfgQuotaMaxQueueSizeQoS1     quota)
        , quotaMaxQueueSizeQoS2     = fromMaybe (quotaMaxQueueSizeQoS2     defaultQuota) (cfgQuotaMaxQueueSizeQoS2     quota)
       }

  getLastException _ = pure Nothing

instance Exception (AuthenticationException SimpleAuthenticator)

instance FromJSON SimplePrincipalConfig where
  parseJSON (Object v) = SimplePrincipalConfig
    <$> v .:? "username"
    <*> ((T.encodeUtf8 <$>) <$> v .:? "password")
    <*> v .:? "quota"
    <*> v .:? "permissions" .!= mempty
  parseJSON invalid = typeMismatch "SimplePrincipalConfig" invalid

instance FromJSON SimpleQuotaConfig where
  parseJSON (Object v) = SimpleQuotaConfig
    <$> v .:? "maxSessions"
    <*> v .:? "maxIdleSessionTTL"
    <*> v .:? "maxPacketSize"
    <*> v .:? "maxPacketIdentifiers"
    <*> v .:? "maxQueueSizeQoS0"
    <*> v .:? "maxQueueSizeQoS1"
    <*> v .:? "maxQueueSizeQoS2"
  parseJSON invalid = typeMismatch "SimpleQuotaConfig" invalid

instance FromJSON Quota where
  parseJSON (Object v) = Quota
    <$> v .: "maxSessions"
    <*> v .: "maxIdleSessionTTL"
    <*> v .: "maxPacketSize"
    <*> v .: "maxPacketIdentifiers"
    <*> v .: "maxQueueSizeQoS0"
    <*> v .: "maxQueueSizeQoS1"
    <*> v .: "maxQueueSizeQoS2"
  parseJSON invalid = typeMismatch "Quota" invalid

instance FromJSON (AuthenticatorConfig SimpleAuthenticator) where
  parseJSON (Object v) = SimpleAuthenticatorConfig
    <$> v .: "principals"
    <*> v .: "defaultQuota"
  parseJSON invalid = typeMismatch "SimpleAuthenticatorConfig" invalid

instance FromJSON Filter where
  parseJSON (String t) =
    case AP.parseOnly filterParser (T.encodeUtf8 t) of
      Left e  -> fail e
      Right x -> pure x
  parseJSON invalid = typeMismatch "Filter" invalid

instance FromJSONKey Filter where
  fromJSONKey = FromJSONKeyTextParser $ \t->
    case AP.parseOnly filterParser (T.encodeUtf8 t) of
      Left e  -> fail e
      Right x -> pure x
