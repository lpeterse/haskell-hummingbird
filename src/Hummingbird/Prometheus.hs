{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.Prometheus where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird.Prometheus
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import           Control.Monad                     (void)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.String
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Middleware.Prometheus as Prometheus
import qualified Prometheus
import qualified Prometheus.Metric.GHC             as Prometheus

run :: Maybe Config -> IO ()
run (Just cfg) = do
  void $ Prometheus.register Prometheus.ghcMetrics
  Warp.runSettings settings Prometheus.metricsApp
  where
    settings =
      Warp.setHost (bindAddress cfg) $
      Warp.setPort (bindPort cfg)
      Warp.defaultSettings
run _ =
  pure ()

data Config
   = Config
     { bindAddress :: Warp.HostPreference
     , bindPort    :: Warp.Port
     } deriving (Eq, Ord, Show)

instance FromJSON Config where
  parseJSON (Object o) = Config
    <$> (fromString <$> o .: "bindAddress")
    <*> o .: "bindPort"
  parseJSON invalid = typeMismatch "Config" invalid
