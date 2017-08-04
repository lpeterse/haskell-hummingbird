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

import qualified Data.Binary                 as B
import           GHC.Generics                (Generic)
import           Text.Parsec                 as P
import           Text.Parsec.String          (Parser)

import           Network.MQTT.Broker.Session (SessionIdentifier (..))

data Request
   = Help
   | Broker
   | Config
   | ConfigReload
   | Auth
   | AuthReload
   | Sessions
   | SessionsSelect SessionIdentifier
   | SessionsSelectDisconnect SessionIdentifier
   | SessionsSelectTerminate SessionIdentifier
   | SessionsSelectSubscriptions SessionIdentifier
   | TransportsStop
   | TransportsStart
   | TransportsStatus
   | Quit
   deriving (Eq, Ord, Show, Generic)

instance B.Binary Request

parse :: String -> Either String Request
parse s = case P.parse requestParser "" s of
  Left e  -> Left (show e)
  Right c -> Right c

requestParser :: Parser Request
requestParser = spaces >> choice
  [ string "help"          >> spaces >> eof >> pure Help
  , string "config"        >> config
  , string "broker"        >> broker
  , string "auth"          >> auth
  , string "session"       >> optional (string "s") >> sessions
  , string "transport"     >> optional (string "s") >> transports
  , string "quit"          >> spaces >> eof >> pure Quit
  , string "exit"          >> spaces >> eof >> pure Quit
  ]
  where
    broker :: Parser Request
    broker = spaces >> choice
      [ eof >> pure Broker
      ]
    config :: Parser Request
    config = spaces >> choice
      [ eof >> pure Config
      , string "reload" >> spaces >> eof >> pure ConfigReload ]
    auth :: Parser Request
    auth = choice
      [ try $ space >> spaces >> string "restart"       >> spaces >> eof >> pure AuthReload
      , try $                                              spaces >> eof >> pure Auth
      ]
    sessions :: Parser Request
    sessions = choice
      [ try $ space >> spaces >> (read <$> many1 digit :: Parser Int) >>= sessionsSelect
      , try $ spaces >> eof >> pure Sessions
      ]
    sessionsSelect :: Int -> Parser Request
    sessionsSelect i = choice
      [ try $ space >> spaces >> string "disconnect"    >> spaces >> eof >> pure (SessionsSelectDisconnect    $ SessionIdentifier i)
      , try $ space >> spaces >> string "terminate"     >> spaces >> eof >> pure (SessionsSelectTerminate     $ SessionIdentifier i)
      , try $ space >> spaces >> string "subscriptions" >> spaces >> eof >> pure (SessionsSelectSubscriptions $ SessionIdentifier i)
      , try $                                              spaces >> eof >> pure (SessionsSelect              $ SessionIdentifier i)
      ]
    transports :: Parser Request
    transports = choice
      [ try $ space >> spaces >> string "start"         >> spaces >> eof >> pure TransportsStart
      , try $ space >> spaces >> string "stop"          >> spaces >> eof >> pure TransportsStop
      , try $                                              spaces >> eof >> pure TransportsStatus
      ]
