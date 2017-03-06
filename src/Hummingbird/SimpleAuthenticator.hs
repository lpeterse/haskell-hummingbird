{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hummingbird.SimpleAuthenticator where

import           Control.Exception
import qualified Crypto.BCrypt               as BCrypt
import           Data.Aeson                  (FromJSON (..), (.:?))
import           Data.Aeson.Types
import qualified Data.ByteString             as BS
import           Data.Functor.Identity
import           Data.List                   as L
import qualified Data.Map                    as M
import           Data.Maybe
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Typeable
import           Data.UUID                   (UUID)
import qualified Data.UUID                   as UUID
import           Data.Word

import           Network.MQTT.Authentication
import           Network.MQTT.Message
import qualified Network.MQTT.RoutingTree    as R

import           Hummingbird.Configuration   hiding (auth)

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
   , cfgPermissions  :: Maybe (R.RoutingTree (Identity [Privilege]))
   } deriving (Eq, Show)

data SimpleQuotaConfig
   = SimpleQuotaConfig
   { cfgQuotaIdleSessionTTL       :: Maybe Word64
   , cfgQuotaMaxInflightMessages  :: Maybe Word64
   , cfgQuotaMaxQueueSizeQoS0     :: Maybe Word64
   , cfgQuotaMaxQueueSizeQoS1     :: Maybe Word64
   , cfgQuotaMaxQueueSizeQoS2     :: Maybe Word64
   } deriving (Eq, Ord, Show)

instance Authenticator SimpleAuthenticator where
  data AuthenticatorConfig SimpleAuthenticator
     = SimpleAuthenticatorConfig
       { cfgPrincipals   :: M.Map UUID SimplePrincipalConfig
       , cfgDefaultQuota :: SimpleQuotaConfig
       }
  data AuthenticationException SimpleAuthenticator
     = SimpleAuthenticationException deriving (Eq, Ord, Show, Typeable)

  newAuthenticator config = SimpleAuthenticator
    <$> pure (cfgPrincipals config)
    <*> pure (f $ cfgDefaultQuota config)
    where
      f qc = Quota {
        quotaSessionTTL = fromMaybe 0 $ cfgQuotaIdleSessionTTL qc
      , quotaMaxInflightMessages = fromMaybe 1 $ cfgQuotaMaxInflightMessages qc
      , quotaMaxQueueSizeQoS0 = fromMaybe 0 $ cfgQuotaMaxQueueSizeQoS0 qc
      , quotaMaxQueueSizeQoS1 = fromMaybe 0 $ cfgQuotaMaxQueueSizeQoS1 qc
      , quotaMaxQueueSizeQoS2 = fromMaybe 0 $ cfgQuotaMaxQueueSizeQoS2 qc
      }

  authenticate auth req =
    pure $ case requestCredentials req of
      Just (reqUser, Just reqPass) ->
        case mapMaybe (byUsernameAndPassword reqUser reqPass) $ M.assocs (authPrincipals auth) of
          [(uuid, _)] -> Just uuid
          _           -> Nothing
      _ -> Nothing
    where
      byUsernameAndPassword (Username reqUser) (Password reqPass) p@(uuid, principal) = do
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
        , principalPublishPermissions   = R.mapMaybe f $ fromMaybe R.empty $ cfgPermissions pc
        , principalSubscribePermissions = R.mapMaybe g $ fromMaybe R.empty $ cfgPermissions pc
        }
    where
      f (Identity xs)
        | Publish `elem` xs   = Just ()
        | otherwise           = Nothing
      g (Identity xs)
        | Subscribe `elem` xs = Just ()
        | otherwise           = Nothing
      -- Prefers a user quota property over the default quota property.
      mergeQuota Nothing defaultQuota = defaultQuota
      mergeQuota (Just principalQuota) defaultQuota = Quota {
          quotaSessionTTL = fromMaybe (quotaSessionTTL defaultQuota) (cfgQuotaIdleSessionTTL principalQuota)
        , quotaMaxInflightMessages = fromMaybe (quotaMaxInflightMessages defaultQuota) (cfgQuotaMaxInflightMessages principalQuota)
        , quotaMaxQueueSizeQoS0 = fromMaybe (quotaMaxQueueSizeQoS0 defaultQuota) (cfgQuotaMaxQueueSizeQoS0 principalQuota)
        , quotaMaxQueueSizeQoS1 = fromMaybe (quotaMaxQueueSizeQoS1 defaultQuota) (cfgQuotaMaxQueueSizeQoS1 principalQuota)
        , quotaMaxQueueSizeQoS2 = fromMaybe (quotaMaxQueueSizeQoS2 defaultQuota) (cfgQuotaMaxQueueSizeQoS2 principalQuota)
       }

instance Exception (AuthenticationException SimpleAuthenticator)

instance FromJSON SimplePrincipalConfig where
  parseJSON (Object v) = SimplePrincipalConfig
    <$> v .:? "username"
    <*> ((T.encodeUtf8 <$>) <$> v .:? "password")
    <*> v .:? "quota"
    <*> v .:? "permissions"
  parseJSON invalid = typeMismatch "SimplePrincipalConfig" invalid

instance FromJSON SimpleQuotaConfig where
  parseJSON (Object v) = SimpleQuotaConfig
    <$> v .:? "idleSessionTTL"
    <*> v .:? "maxInflightMessages"
    <*> v .:? "maxQueueSizeQoS0"
    <*> v .:? "maxQueueSizeQoS1"
    <*> v .:? "maxQueueSizeQoS2"
  parseJSON invalid = typeMismatch "SimpleQuotaConfig" invalid

instance FromJSON (AuthenticatorConfig SimpleAuthenticator) where
  parseJSON (Object v) = SimpleAuthenticatorConfig
    <$> v .: "principals"
    <*> v .: "defaultQuota"
  parseJSON invalid = typeMismatch "SimpleAuthenticatorConfig" invalid
