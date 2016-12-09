module Main where

import           Data.Proxy

import           Hummingbird.Configuration
import qualified Hummingbird ( runCommandLine )
import           Hummingbird.SimpleAuthenticator

main :: IO ()
main = Hummingbird.runCommandLine (Proxy :: Proxy (Config SimpleAuthenticator) )
