{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hummingbird ( run ) where

import qualified Crypto.BCrypt                      as BCrypt
import           Data.Aeson
import qualified Data.ByteString.Char8              as BS
import           Data.Proxy
import           Options
import           System.Exit
import           System.IO

import           Network.MQTT.Broker.Authentication (Authenticator,
                                                     AuthenticatorConfig)

import qualified Hummingbird.Administration.CLI     as Admin
import qualified Hummingbird.Administration.Server  as Admin
import qualified Hummingbird.Broker                 as Broker
import qualified Hummingbird.Configuration          as Config

data MainOptions = MainOptions
  { mainConfigFilePath :: FilePath }

data CliOptions = CliOptions

data BrokerOptions = BrokerOptions

data PwhashOptions = PwhashOptions

instance Options MainOptions where
  defineOptions = MainOptions
    <$> simpleOption "settings" "/etc/hummingbird/settings.yml" "Path to the .yaml configuration file"

instance Options CliOptions where
  defineOptions = pure CliOptions

instance Options BrokerOptions where
  defineOptions = pure BrokerOptions

instance Options PwhashOptions where
  defineOptions = pure PwhashOptions

run :: (Authenticator auth, FromJSON (AuthenticatorConfig auth)) => Proxy (Config.Config auth) -> IO ()
run authConfigProxy = runSubcommand
  [ subcommand "cli"    cliCommand
  , subcommand "pwhash" pwhashCommand
  , subcommand "broker" brokerCommand
  ]
  where
    cliCommand    :: MainOptions -> CliOptions -> [String] -> IO ()
    cliCommand mainOpts _ _ = Config.loadConfigFromFile (mainConfigFilePath mainOpts) >>= \case
      Left e    -> hPutStrLn stderr e >> exitFailure
      Right cfg -> Admin.runCommandLineInterface (cfg `asProxyTypeOf` authConfigProxy)

    pwhashCommand :: MainOptions -> PwhashOptions -> [String] -> IO ()
    pwhashCommand _ _ _ = do
      hSetEcho stdin False
      password <- BS.getLine
      mhash <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy password
      case mhash of
        Nothing   -> exitFailure
        Just hash -> BS.putStrLn hash

    brokerCommand :: MainOptions -> BrokerOptions -> [String] -> IO ()
    brokerCommand mainOpts _ _ =
      Broker.withBrokerFromSettingsPath
        (mainConfigFilePath mainOpts)
        (Admin.runServerInterface authConfigProxy)
