{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.Logging
  ( Config (..)
  , Appender (..)
  , setup
  ) where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird.Logging
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import           Control.Monad             (forM_)
import           Data.Aeson
import           Data.Aeson.Types
import           System.IO
import qualified System.Log.Formatter      as Log
import qualified System.Log.Handler        as Log hiding (setLevel)
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Handler.Syslog as Log
import qualified System.Log.Logger         as Log

data Config
   = Config
     { level     :: Log.Priority
     , appenders :: [Appender]
     } deriving (Eq, Ord, Show)

data Appender
   = SyslogAppender
   | ConsoleAppender
   deriving (Eq, Ord, Show)

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> pLevel
    <*> v .: "appenders"
    where
      pLevel = do
        s <- v .: "level" :: Parser String
        case s of
          "DEBUG"     -> pure Log.DEBUG
          "INFO"      -> pure Log.INFO
          "NOTICE"    -> pure Log.NOTICE
          "WARNING"   -> pure Log.WARNING
          "ERROR"     -> pure Log.ERROR
          "CRITICAL"  -> pure Log.CRITICAL
          "ALERT"     -> pure Log.ALERT
          "EMERGENCY" -> pure Log.EMERGENCY
          _           -> fail "Expected DEBUG, INFO, WARNING, ERROR etc."
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON Appender where
  parseJSON (Object v) = do
    t <- v .: "type" :: Parser String
    case t of
      "syslog"  -> pure SyslogAppender
      "console" -> pure ConsoleAppender
      _         -> fail "Expected 'syslog' or 'console'."
  parseJSON invalid = typeMismatch "Appender" invalid

setup :: Config -> IO ()
setup config = do
  Log.removeAllHandlers
  Log.updateGlobalLogger Log.rootLoggerName (Log.setLevel $ level config)
  forM_ (appenders config) $ \case
    SyslogAppender  -> do
      s <- Log.openlog "hummingbird" [Log.PID] Log.USER Log.DEBUG
      Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler s)
    ConsoleAppender -> do
      lh <- Log.streamHandler stderr Log.DEBUG
      let h = Log.setFormatter lh (Log.simpleLogFormatter "[$time : $loggername : $prio] $msg")
      Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler h)
  Log.infoM "hummingbird" "Started hummingbird MQTT message broker."
