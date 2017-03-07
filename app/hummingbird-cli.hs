module Main where

import           Data.Proxy

import           Hummingbird.Configuration
import           Hummingbird.SimpleAuthenticator
import qualified HummingbirdCli
import           Options

main :: IO ()
main = runCommand $ \opts _ -> do
  HummingbirdCli.run (Proxy :: Proxy (Config SimpleAuthenticator) ) opts
