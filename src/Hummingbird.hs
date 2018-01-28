{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Hummingbird ( Hummingbird.Hummingbird (), Hummingbird.VendorSettings (..), runWithVendorSettings ) where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import qualified Crypto.BCrypt                      as BCrypt
import           Data.Aeson                         hiding (Options)
import qualified Data.ByteString.Char8              as BS
import           Options
import           System.Exit
import           System.IO

import           Network.MQTT.Broker.Authentication (Authenticator,
                                                     AuthenticatorConfig)

import qualified Hummingbird.Administration.Cli     as Cli
import qualified Hummingbird.Administration.Server  as Administration
import qualified Hummingbird.Internal               as Hummingbird
import qualified Hummingbird.Internal               as HI

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data VendorSettings auth = VendorSettings
  { vendorVersionName :: String
  } deriving (Eq, Ord, Show)

data MainOptions = MainOptions

data BrokerOptions = BrokerOptions
  { configFilePath :: FilePath }

data VersionOptions = VersionOptions

instance Options MainOptions where
  defineOptions = pure MainOptions

instance Options BrokerOptions where
  defineOptions = BrokerOptions
    <$> simpleOption "config" "/etc/hummingbird/config.yml" "Path to the .yml config file"

instance Options VersionOptions where
  defineOptions = pure VersionOptions

runWithVendorSettings :: forall auth. (Authenticator auth, FromJSON (AuthenticatorConfig auth), Show (AuthenticatorConfig auth)) => VendorSettings auth -> IO ()
runWithVendorSettings vendorSettings = runSubcommand
  [ subcommand "cli"     Cli.run
  , subcommand "broker"  runBroker
  , subcommand "pwhash"  runPwhash
  , subcommand "version" runVersion
  ]
  where

    runBroker :: MainOptions -> BrokerOptions -> [String] -> IO ()
    runBroker _ opts _ = do
      hum <- HI.new settings :: IO (Hummingbird.Hummingbird auth)
      HI.start hum
      Administration.run hum
      where
        settings = HI.Settings {
          HI.versionName = vendorVersionName vendorSettings
        , HI.configFilePath = configFilePath opts
        }

    runPwhash :: MainOptions -> VersionOptions -> [String] -> IO ()
    runPwhash _ _ _ = do
      hSetEcho stdin False
      password <- BS.getLine
      mhash <- BCrypt.hashPasswordUsingPolicy
        (BCrypt.fastBcryptHashingPolicy { BCrypt.preferredHashCost = 8 })
        password
      case mhash of
        Nothing   -> exitFailure
        Just hash -> BS.putStrLn hash

    runVersion :: MainOptions -> VersionOptions -> [String] -> IO ()
    runVersion _ _ _ =
      putStrLn (vendorVersionName vendorSettings)
