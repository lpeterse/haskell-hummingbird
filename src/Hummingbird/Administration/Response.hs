{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Response where

import           Control.Monad
import qualified Data.Binary                       as B
import           Data.Int
import qualified Data.Text                         as T
import           GHC.Generics                      (Generic)

import           Hummingbird.Administration.Escape

data Response
   = Success
   | Failure String
   | Help
   | BrokerInfo
   { brokerVersion           :: String
   , brokerUptime            :: Int64
   , brokerSessionCount      :: Int
   , brokerSubscriptionCount :: Int
   }
   | SessionList [SessionInfo]
   deriving (Eq, Ord, Show, Generic)

data SessionInfo
   = SessionInfo
   { sessionIdentifier       :: Int
   , sessionClientIdentifier :: T.Text
   , sessionStatus           :: SessionInfoStatus
   , sessionCreatedAt        :: Int64
   }
   deriving (Eq, Ord, Show, Generic)

data SessionInfoStatus
  = SessionConnected
  | SessionConnectedClean
  | SessionDisconnected
  deriving (Eq, Ord, Show, Generic)

instance B.Binary Response
instance B.Binary SessionInfo
instance B.Binary SessionInfoStatus

render :: Monad m => (String -> m ()) -> Response -> m ()
render p Success =
  p (cyan "Success")

render p (Failure e) =
  p (lightRed e)

render p Help = do
    p "help                      : show this help"
    p "broker"
    p "  info                    : show broker information"
    p "sessions                  : list all sessions"
    p "  [0-9]+                  : a certain session identified by id"
    p "    info                  : show session information"
    p "    disconnect            : disconnect associated client (if any)"
    p "    terminate             : terminate session (and disconnect client)"

render p info@BrokerInfo {} = do
    format "Version          " $ brokerVersion info
    format "Uptime           " $ formatUptime (brokerUptime info)
    format "Sessions         " $ show (brokerSessionCount info)
    format "Subscriptions    " $ show (brokerSubscriptionCount info)
    format "Throughput       " "20,454/s 9,779/s 15,831/s"
  where
    format key value =
      p $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m\STX"
    formatUptime uptime =
      show days ++ " day" ++ (if days /= 1 then "s, " else ", ") ++
      leftPad 2 '0' (show hours) ++ ":" ++ leftPad 2 '0' (show minutes) ++
      ":" ++ leftPad 2 '0' (show seconds)
      where
        days   = quot uptime (24*3600)
        hours   = rem uptime (24*3600) `quot` 3600
        minutes = rem uptime 3600 `quot` 60
        seconds = rem uptime 60

render p (SessionList ss) =
  forM_ ss $ \session->
    p $ statusDot (sessionStatus session) ++
    leftPad 8 ' ' (show $ sessionIdentifier session) ++
    leftPad 30 ' ' (show $ sessionClientIdentifier session)
  where
    statusDot SessionConnected      = lightGreen dot
    statusDot SessionConnectedClean = lightBlue dot
    statusDot SessionDisconnected   = lightRed dot
