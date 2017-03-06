module Main where

import           Data.Proxy

import           Hummingbird.Configuration
import           Hummingbird.SimpleAuthenticator
import qualified HummingbirdCli

main :: IO ()
main = HummingbirdCli.run (Proxy :: Proxy (Config SimpleAuthenticator) )
