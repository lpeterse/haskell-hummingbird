{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module HummingbirdCli ( run ) where

import           Data.Aeson
import           Data.Proxy
import           System.Environment
import           System.Exit
import           System.IO                       (hPutStrLn, stderr)

import           Hummingbird.Administration.CLI2 (runCommandLineInterface)
import qualified Hummingbird.Configuration       as Config

import           Network.MQTT.Authentication     (Authenticator,
                                                  AuthenticatorConfig)

run :: (Authenticator auth, FromJSON (AuthenticatorConfig auth)) => Proxy (Config.Config auth) -> IO ()
run authConfigProxy = Config.loadConfigFromFile "./settings-dev.yaml" >>= \case
  Left e    -> hPutStrLn stderr e >> exitFailure
  Right cfg -> runCommandLineInterface (cfg `asProxyTypeOf` authConfigProxy)
