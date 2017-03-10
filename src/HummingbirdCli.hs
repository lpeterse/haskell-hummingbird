{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module HummingbirdCli ( run ) where

import           Data.Aeson
import           Data.Proxy
import           Options
import           System.Environment
import           System.Exit
import           System.IO                          (hPutStrLn, stderr)

import           Network.MQTT.Broker.Authentication (Authenticator,
                                                     AuthenticatorConfig)

import           Hummingbird.Administration.CLI2    (runCommandLineInterface)
import qualified Hummingbird.Configuration          as Config

data MainOptions = MainOptions
  { mainConfigFilePath :: FilePath }

instance Options MainOptions where
  defineOptions = MainOptions
    <$> simpleOption "settings" "/etc/hummingbird/settings.yml" "Path to the .yml configuration file"

run :: (Authenticator auth, FromJSON (AuthenticatorConfig auth)) => Proxy (Config.Config auth) -> MainOptions -> IO ()
run authConfigProxy opts = Config.loadConfigFromFile (mainConfigFilePath opts) >>= \case
  Left e    -> hPutStrLn stderr e >> exitFailure
  Right cfg -> runCommandLineInterface (cfg `asProxyTypeOf` authConfigProxy)
