{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
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
import           Control.Monad                      (foldM)
import qualified Crypto.BCrypt                      as BCrypt
import           Data.Aeson                         (FromJSON (..), (.:?))
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString         as AP
import qualified Data.ByteString                    as BS
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.Map                           as M
import           Data.Maybe
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Typeable
import           Data.UUID                          (UUID)
import qualified Data.Yaml                          as Yaml
import qualified System.Directory                   as Dir
import qualified System.FilePath                    as FilePath
import qualified System.Log.Logger                  as Log

import           Network.MQTT.Broker.Authentication
import           Network.MQTT.Message
import qualified Network.MQTT.Trie                  as R

import qualified Hummingbird.Configuration          as C

data SimpleAuthenticator
   = SimpleAuthenticator
   { authDefaultQuota         :: Quota
   , authPrincipalsByUUID     :: M.Map UUID SimplePrincipalConfig
   , authPrincipalsByUsername :: M.Map T.Text SimplePrincipalConfig
   } deriving (Eq, Show)

data SimplePrincipalConfig
   = SimplePrincipalConfig
   { cfgUUID         :: UUID
   , cfgUsername     :: Maybe T.Text
   , cfgPasswordHash :: Maybe BS.ByteString
   , cfgQuota        :: Maybe SimpleQuotaConfig
   , cfgPermissions  :: M.Map Filter (Identity [C.Privilege])
   } deriving (Eq, Ord, Show)

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
       { cfgPrincipalDirs :: [FilePath]
       , cfgDefaultQuota  :: Quota
       } deriving (Eq, Show)

  data AuthenticationException SimpleAuthenticator
     = SimpleAuthenticationException String deriving (Eq, Ord, Show, Typeable)

  newAuthenticator = newSimpleAuthenticator

  authenticate auth req =
    pure $ case requestCredentials req of
      Just (reqUser, Just reqPass) ->
        case mapMaybe (byUsernameAndPassword reqUser reqPass) $ M.assocs (authPrincipalsByUUID auth) of
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
    case M.lookup pid (authPrincipalsByUUID auth) of
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
    <$> v .:  "uuid"
    <*> v .:? "username"
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
    <$> v .: "principalDirs"
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

newSimpleAuthenticator :: AuthenticatorConfig SimpleAuthenticator -> IO SimpleAuthenticator
newSimpleAuthenticator config = do
  Log.infoM "SimpleAuthenticator" "Creating new SimpleAuthenticator."
  create `catch` \e-> do
    Log.errorM "SimpleAuthenticator" $ "While trying to create new SimpleAuthenticator: " ++ show (e :: SomeException)
    throwIO e
  where
    create = do
      principals <- foldM explore mempty (cfgPrincipalDirs config)
      Log.infoM "SimpleAuthenticator" $ "Found " ++ show (S.size principals) ++ " principal definitions."
      pure $ SimpleAuthenticator (cfgDefaultQuota config) (byUUID principals) (byUsername principals)

    consider :: FilePath -> S.Set SimplePrincipalConfig -> IO (S.Set SimplePrincipalConfig)
    consider path set
      | FilePath.takeExtension path /= ".yml" = do
          Log.debugM "SimpleAuthenticator" $ "Ignoring file " ++ show path ++ " which is not .yml."
          pure set
      | take 1 (FilePath.takeBaseName path) == "." = do
          Log.debugM "SimpleAuthenticator" $ "Ignoring hidden file " ++ show path ++ "."
          pure set
      | otherwise = Yaml.decodeFileEither path >>= \case
          Left e -> throwIO $ SimpleAuthenticationException $ "While parsing '" ++ show path ++ "': " ++ show e
          Right p -> pure $! S.insert p set

    explore :: S.Set SimplePrincipalConfig -> FilePath -> IO (S.Set SimplePrincipalConfig)
    explore accum path = do
      absolutePath <- Dir.makeAbsolute path
      Log.infoM "SimpleAuthenticator" $ "Looking for principal definitions in " ++ show absolutePath ++ "."
      traverseFilesInDirectory consider accum absolutePath

    byUUID :: S.Set SimplePrincipalConfig -> M.Map UUID SimplePrincipalConfig
    byUUID = foldl' add mempty
      where
        add acc p
          | M.member (cfgUUID p) acc = error $ "Duplicate UUID " ++ show (cfgUUID p)
          | otherwise                = M.insert (cfgUUID p) p acc

    byUsername :: S.Set SimplePrincipalConfig -> M.Map T.Text SimplePrincipalConfig
    byUsername = foldl' add mempty
      where
        add acc p = case cfgUsername p of
          Nothing -> acc
          Just u  | M.member u acc -> error $ "Duplicate username " ++ show u
                  | otherwise      -> M.insert u p acc

    traverseFilesInDirectory :: (FilePath -> a -> IO a) -> a -> FilePath -> IO a
    traverseFilesInDirectory digest accum0 path = snd <$> go (mempty, accum0) path
      where
        go (visitedPaths, accum) currentPath
          | S.member currentPath visitedPaths = pure (visitedPaths, accum)
          | otherwise = do
            Log.debugM "SimpleAuthenticator" $ "Inspecting path " ++ show currentPath ++ "."
            Dir.doesDirectoryExist currentPath >>= \case
              True -> do
                paths <- Dir.listDirectory currentPath
                foldM go (S.insert currentPath visitedPaths, accum) (fmap (currentPath FilePath.</>) paths)
              False -> Dir.doesFileExist currentPath  >>= \case
                False -> pure (S.insert currentPath visitedPaths, accum)
                True -> (S.insert currentPath visitedPaths,) <$> digest currentPath accum
