module Main where

import           Hummingbird.Configuration
import qualified Hummingbird ( loadConfig, runWithConfig )
import           Hummingbird.SimpleAuthenticator

main :: IO ()
main = do
  cfg <- Hummingbird.loadConfig :: IO (Config SimpleAuthenticator)
  Hummingbird.runWithConfig cfg
