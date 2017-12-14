{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Request where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird.Administration.Request
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import           Control.Applicative
import qualified Data.Binary                 as B
import           Data.Char
import           GHC.Generics                (Generic)
import qualified Text.Parsec                 as P
import qualified Text.Parsec.Prim            as Prim

import           Network.MQTT.Broker.Session (SessionIdentifier (..))

data Request
   = Help
   | BrokerStatus
   | ConfigStatus
   | ConfigReload
   | AuthStatus
   | AuthRestart
   | SessionList
   | SessionStatus        SessionIdentifier
   | SessionDisconnect    SessionIdentifier
   | SessionTerminate     SessionIdentifier
   | SessionSubscriptions SessionIdentifier
   | TransportsStop
   | TransportsStart
   | TransportsStatus
   | Quit
   deriving (Eq, Ord, Show, Generic)

instance B.Binary Request

type Parser = P.Parsec [String] ()

parse :: String -> Either String Request
parse s = case P.parse requestParser "" (words s) of
  Left e  -> Left (show e)
  Right c -> Right c

token :: String -> Parser ()
token s = Prim.tokenPrim showTok posFromTok testTok
  where
    showTok t          = show t
    posFromTok p _ _   = p
    testTok t          = if s == t then Just () else Nothing

int :: Parser Int
int = Prim.tokenPrim showTok posFromTok testTok
  where
    showTok t          = show t
    posFromTok p _ _   = p
    testTok t
      | all isDigit t && length t < length (show (maxBound :: Int)) = Just (read t)
      | otherwise                                                   = Nothing

requestParser :: Parser Request
requestParser = P.choice
  [ token "help"                               >> P.eof >> pure Help
  , token "config"                             >> config
  , token "broker"                             >> broker
  , token "auth"                               >> auth
  , (token "session"   <|> token "sessions")   >> session
  , (token "transport" <|> token "transports") >> transport
  , (token "quit"      <|> token "exit")       >> P.eof >> pure Quit
  ]
  where
    broker :: Parser Request
    broker = P.choice
      [ token "status"        >> P.eof >> pure BrokerStatus
      ,                          P.eof >> pure BrokerStatus
      ]
    config :: Parser Request
    config = P.choice
      [ token "reload"        >> P.eof >> pure ConfigReload
      , token "status"        >> P.eof >> pure ConfigStatus
      ,                          P.eof >> pure ConfigStatus
      ]
    auth :: Parser Request
    auth = P.choice
      [ token "restart"       >> P.eof >> pure AuthRestart
      , token "status"        >> P.eof >> pure AuthStatus
      ,                          P.eof >> pure AuthStatus
      ]
    session :: Parser Request
    session = P.choice
      [ token "list"          >> P.eof >> pure SessionList
      ,                          P.eof >> pure SessionList
      , int >>= \i-> P.choice
        [ token "disconnect"    >> P.eof >> pure (SessionDisconnect     $ SessionIdentifier i)
        , token "terminate"     >> P.eof >> pure (SessionTerminate      $ SessionIdentifier i)
        , token "subscriptions" >> P.eof >> pure (SessionSubscriptions  $ SessionIdentifier i)
        , token "status"        >> P.eof >> pure (SessionStatus         $ SessionIdentifier i)
        ,                          P.eof >> pure (SessionStatus         $ SessionIdentifier i)
        ]
      ]
    transport :: Parser Request
    transport = P.choice
      [ token "start"         >> P.eof >> pure TransportsStart
      , token "stop"          >> P.eof >> pure TransportsStop
      , token "status"        >> P.eof >> pure TransportsStatus
      ,                          P.eof >> pure TransportsStatus
      ]
