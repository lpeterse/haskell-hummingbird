{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hummingbird.SimpleAuthenticator where

import           Control.Exception
import qualified Crypto.BCrypt               as BCrypt
import           Data.Aeson                  (FromJSON (..), (.:?))
import           Data.Aeson.Types
import           Data.Functor.Identity
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
     { credentials          :: M.Map T.Text T.Text
     , publishPermissions   :: M.Map T.Text (R.RoutingTree ())
     , subscribePermissions :: M.Map T.Text (R.RoutingTree ())
     }

instance Authenticator SimpleAuthenticator where
  data Principal SimpleAuthenticator
     = SimplePrincipal T.Text deriving (Eq, Ord, Show)
  data AuthenticatorConfig SimpleAuthenticator
     = SimpleAuthenticatorConfig
       { authCredentials :: M.Map T.Text T.Text
       , authPermissions :: M.Map T.Text (R.RoutingTree (Identity [Privilege]))
       }
  data AuthenticationException SimpleAuthenticator
     = SimpleAuthenticationException deriving (Eq, Ord, Show, Typeable)
  newAuthenticator cfg =
    pure (SimpleAuthenticator (authCredentials cfg) pubPermissions subPermissions)
    where
      f (Identity xs)
        | Publish `elem` xs   = Just ()
        | otherwise           = Nothing
      g (Identity xs)
        | Subscribe `elem` xs = Just ()
        | otherwise           = Nothing
      pubPermissions = fmap (R.mapMaybe f) (authPermissions cfg)
      subPermissions = fmap (R.mapMaybe g) (authPermissions cfg)
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

  hasPublishPermission auth (SimplePrincipal principal) topic =
    pure $ case M.lookup principal (publishPermissions auth) of
      Nothing   -> False
      Just tree -> R.matchTopic topic tree
  hasSubscribePermission auth (SimplePrincipal principal) filtr =
    pure $ case M.lookup principal (subscribePermissions auth) of
      Nothing   -> False
      Just tree -> R.matchFilter filtr tree

instance Exception (AuthenticationException SimpleAuthenticator)

instance FromJSON (AuthenticatorConfig SimpleAuthenticator) where
  parseJSON (Object v) = SimpleAuthenticatorConfig
    <$> v .:? "credentials" .!= mempty
    <*> v .:? "permissions" .!= mempty
  parseJSON invalid = typeMismatch "SimpleAuthenticatorConfig" invalid
