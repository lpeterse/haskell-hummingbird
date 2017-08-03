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

import           Data.Proxy

import qualified Hummingbird
import           Hummingbird.Configuration
import           Hummingbird.SimpleAuthenticator

import           Paths_hummingbird               (version)

main :: IO ()
main = Hummingbird.run version (Proxy :: Proxy (Config SimpleAuthenticator) )
