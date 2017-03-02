module Main where

import           Data.Proxy

import qualified Hummingbird
import           Hummingbird.Configuration
import           Hummingbird.SimpleAuthenticator

main :: IO ()
main = Hummingbird.run (Proxy :: Proxy (Config SimpleAuthenticator) )
