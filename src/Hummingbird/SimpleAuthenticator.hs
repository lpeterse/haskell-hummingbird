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
   { cfgQuotaSessionTTL           :: Maybe Word64
   , cfgQuotaMaxInflightMessages  :: Maybe Word64
   , cfgQuotaMaxQueueSizeQos0     :: Maybe Word64
   , cfgQuotaMaxQueueSizeQos1     :: Maybe Word64
   , cfgQuotaMaxQueueSizeQos2     :: Maybe Word64
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
        quotaSessionTTL = fromMaybe 0 $ cfgQuotaSessionTTL qc
      , quotaMaxInflightMessages = fromMaybe 1 $ cfgQuotaMaxInflightMessages qc
      , quotaMaxQueueSizeQos0 = fromMaybe 0 $ cfgQuotaMaxQueueSizeQos0 qc
      , quotaMaxQueueSizeQos1 = fromMaybe 0 $ cfgQuotaMaxQueueSizeQos1 qc
      , quotaMaxQueueSizeQos2 = fromMaybe 0 $ cfgQuotaMaxQueueSizeQos2 qc
      }

  authenticate auth req =
    pure $ case requestCredentials req of
      Just (reqUser, Just (Password reqPass)) ->
        case mapMaybe (byUsernameAndPassword reqUser reqPass) $ M.assocs (authPrincipals auth) of
          [(uuid, _)] -> Just uuid
          _           -> Nothing
      _ -> Nothing
    where
      byUsernameAndPassword reqUser reqPass p@(uuid, principal) = do
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
          principalUsername             = cfgUsername pc
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
          quotaSessionTTL = fromMaybe (quotaSessionTTL defaultQuota) (cfgQuotaSessionTTL principalQuota)
        , quotaMaxInflightMessages = fromMaybe (quotaMaxInflightMessages defaultQuota) (cfgQuotaMaxInflightMessages principalQuota)
        , quotaMaxQueueSizeQos0 = fromMaybe (quotaMaxQueueSizeQos0 defaultQuota) (cfgQuotaMaxQueueSizeQos0 principalQuota)
        , quotaMaxQueueSizeQos1 = fromMaybe (quotaMaxQueueSizeQos1 defaultQuota) (cfgQuotaMaxQueueSizeQos1 principalQuota)
        , quotaMaxQueueSizeQos2 = fromMaybe (quotaMaxQueueSizeQos2 defaultQuota) (cfgQuotaMaxQueueSizeQos2 principalQuota)
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
    <$> v .:? "sessionTTL"
    <*> v .:? "maxInflightMessages"
    <*> v .:? "maxQueueSizeQos0"
    <*> v .:? "maxQueueSizeQos1"
    <*> v .:? "maxQueueSizeQos2"
  parseJSON invalid = typeMismatch "SimpleQuotaConfig" invalid

instance FromJSON (AuthenticatorConfig SimpleAuthenticator) where
  parseJSON (Object v) = SimpleAuthenticatorConfig
    <$> v .: "principals"
    <*> v .: "defaultQuota"
  parseJSON invalid = typeMismatch "SimpleAuthenticatorConfig" invalid
