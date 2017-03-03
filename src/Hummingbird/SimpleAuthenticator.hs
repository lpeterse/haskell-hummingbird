{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hummingbird.SimpleAuthenticator where

import           Control.Exception
import qualified Crypto.BCrypt               as BCrypt
import           Data.Aeson                  (FromJSON (..), (.:?))
import           Data.Aeson.Types
import           Data.Functor.Identity
import Data.Word
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Typeable
import           Network.MQTT.Authentication
import           Network.MQTT.Message
import qualified Network.MQTT.RoutingTree    as R

import           Hummingbird.Configuration   hiding (auth)

data SimpleAuthenticator
   = SimpleAuthenticator
     { principals :: [ Principal SimpleAuthenticator ]
     }

data SimplePrincipalConfig
   = SimplePrincipalConfig
   { cfgUsername    :: Maybe T.Text
   , cfgPassword    :: Maybe T.Text
   , cfgQuota       :: Maybe SimpleQuota
   , cfgPermissions :: Maybe (R.RoutingTree (Identity [Privilege]))
   }

data SimpleQuotaConfig
   = SimpleQuotaConfig
   { cfgSessionTTL :: Maybe Word64
   } deriving (Eq, Ord, Show)

data SimpleQuota
   = SimpleQuota
   { sessionTTL    :: Word64
   } deriving (Eq, Ord, Show)

instance Authenticator SimpleAuthenticator where
  data Principal SimpleAuthenticator = SimplePrincipal {
          username             :: T.Text
        , password             :: T.Text
        , quota                :: SimpleQuota
        , publishPermissions   :: R.RoutingTree ()
        , subscribePermissions :: R.RoutingTree ()
        }
  data AuthenticatorConfig SimpleAuthenticator
     = SimpleAuthenticatorConfig
       { cfgPrincipals   :: [ SimplePrincipalConfig ]
       , cfgDefaultQuota :: SimpleQuota
       }
  data AuthenticationException SimpleAuthenticator
     = SimpleAuthenticationException deriving (Eq, Ord, Show, Typeable)
  newAuthenticator config = do
    let ps = fmap f (cfgPrincipals config)

    pure $ SimpleAuthenticator $ fmap f (cfgPrincipals config)
    where
      f = n 


     pure (SimpleAuthenticator $ authPrincipals config)
    {-
    pure (SimpleAuthenticator (authCredentials cfg) pubPermissions subPermissions)
    where
      f (Identity xs)
        | Publish `elem` xs   = Just ()
        | otherwise           = Nothing
      g (Identity xs)
        | Subscribe `elem` xs = Just ()
        | otherwise           = Nothing
      pubPermissions = fmap (R.mapMaybe f) (authPermissions cfg)
      subPermissions = fmap (R.mapMaybe g) (authPermissions cfg) -}
  authenticate auth req = (SimplePrincipal <$>) <$> lookupBasicIdentity
    where
      lookupBasicIdentity =
        pure $ case requestCredentials req of
          Just (username, Just (Password password)) ->
            case M.lookup username (credentials auth) of
              Nothing -> Nothing
              Just hashedPassword
                | BCrypt.validatePassword (T.encodeUtf8 hashedPassword) password -> Just username
                | otherwise                                                      -> Nothing
          _ -> Nothing

  hasPublishPermission auth principal topic = undefined
{-
    pure $ case M.lookup principal (publishPermissions auth) of
      Nothing   -> False
      Just tree -> R.matchTopic topic tree
-}
  hasSubscribePermission auth principal filtr = undefined
{-    pure $ case M.lookup principal (subscribePermissions auth) of
      Nothing   -> False
      Just tree -> R.matchFilter filtr tree
-}

instance Exception (AuthenticationException SimpleAuthenticator)

instance FromJSON SimplePrincipalConfig where
  parseJSON (Object v) = SimplePrincipalConfig
    <$> v .:? "username"
    <*> v .:? "password"
    <*> v .:? "quota"
    <*> v .:? "permissions"
  parseJSON invalid = typeMismatch "SimplePrincipal" invalid

instance FromJSON SimpleQuota where
  parseJSON (Object v) = SimpleQuota
    <$> v .: "sessionTTL"
  parseJSON invalid = typeMismatch "SimpleQuota" invalid

instance FromJSON SimpleQuotaConfig where
  parseJSON (Object v) = SimpleQuotaConfig
    <$> v .:? "sessionTTL"
  parseJSON invalid = typeMismatch "SimpleQuotaConfig" invalid

instance FromJSON (AuthenticatorConfig SimpleAuthenticator) where
  parseJSON (Object v) = SimpleAuthenticatorConfig
    <$> v .:? "principcals" .!= mempty
    <*> v .: "defaultQuota"
  parseJSON invalid = typeMismatch "SimpleAuthenticatorConfig" invalid
