--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module Main where

import           Data.Version                    (showVersion)

import           Hummingbird
import           Hummingbird.SimpleAuthenticator

import           Paths_hummingbird               (version)

main :: IO ()
main =
  runWithVendorSettings settings
  where
    settings = VendorSettings {
      vendorVersionName = showVersion version
    } :: VendorSettings SimpleAuthenticator
