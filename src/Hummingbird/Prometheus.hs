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
import qualified Prometheus                        as P
import qualified Prometheus.Metric.GHC             as Prometheus

data Metrics
  = Metrics
  { hummingbird_connections_accepted_total    :: P.Metric P.Counter
  , hummingbird_connections_rejected_total    :: P.Metric P.Counter
  , hummingbird_connections_closed_total      :: P.Metric P.Counter
  , hummingbird_connections_failed_total      :: P.Metric P.Counter
  , hummingbird_publications_upstream_total   :: P.Metric P.Counter
  , hummingbird_publications_downstream_total :: P.Metric P.Counter
  }

newRegisteredMetrics :: IO Metrics
newRegisteredMetrics = Metrics
  <$> (P.register =<< P.counter (P.Info "hummingbird_connections_accepted_total"     "Number of successfully authenticated connection attempts."))
  <*> (P.register =<< P.counter (P.Info "hummingbird_connections_rejected_total"     "Number of connection attempts rejected for various reasons."))
  <*> (P.register =<< P.counter (P.Info "hummingbird_connections_closed_total"       "Number of connections closed gracefully by client."))
  <*> (P.register =<< P.counter (P.Info "hummingbird_connections_failed_total"       "Number of connections terminated exceptionally."))
  <*> (P.register =<< P.counter (P.Info "hummingbird_publications_upstream_total"    "Number of messages published upstream."))
  <*> (P.register =<< P.counter (P.Info "hummingbird_publications_downstream_total"  "Number of messages published downstream."))

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
