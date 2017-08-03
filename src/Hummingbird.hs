{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hummingbird ( run ) where
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
import           Data.Proxy
import           Data.Version
import           Options

import           Network.MQTT.Broker.Authentication (Authenticator,
                                                     AuthenticatorConfig)

import qualified Hummingbird.Administration.Server  as Admin
import qualified Hummingbird.Broker                 as Broker
import qualified Hummingbird.Configuration          as Config

newtype MainOptions = MainOptions
  { mainSettingsFilePath :: FilePath }

instance Options MainOptions where
  defineOptions = MainOptions
    <$> simpleOption "settings" "/etc/hummingbird/settings.yml" "Path to the .yml settings file"

run :: (Authenticator auth, FromJSON (AuthenticatorConfig auth)) => Version -> Proxy (Config.Config auth) -> IO ()
run version authConfigProxy =
  runCommand $ \mainOpts _args->
    Broker.withBrokerFromSettingsPath
      version
      ( mainSettingsFilePath mainOpts )
      ( Admin.runServerInterface authConfigProxy )
