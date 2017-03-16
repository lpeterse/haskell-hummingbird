{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Request where

import qualified Data.Binary        as B
import           GHC.Generics       (Generic)
import           Text.Parsec        as P
import           Text.Parsec.String (Parser)

data Request
   = Help
   | Broker
   | Config
   | ConfigReload
   | Sessions
   | SessionsSelect Int
   | SessionsSelectDisconnect Int
   | SessionsSelectTerminate Int
   | SessionsSelectSubscriptions Int
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
  [ string "help"       >> spaces >> eof >> pure Help
  , string "broker"     >> broker
  , string "config"     >> config
  , string "sessions"   >> sessions
  , string "transports" >> transports
  , string "quit" >> spaces >> eof >> pure Quit
  , string "exit" >> spaces >> eof >> pure Quit
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
    sessions :: Parser Request
    sessions = spaces >> choice
      [ eof >> pure Sessions
      , (read <$> many1 digit :: Parser Int) >>= sessionsSelect
      ]
    sessionsSelect :: Int -> Parser Request
    sessionsSelect i = spaces >> choice
      [ eof >> pure (SessionsSelect i)
      , string "disconnect" >> spaces >> eof >> pure (SessionsSelectDisconnect i)
      , string "terminate"  >> spaces >> eof >> pure (SessionsSelectTerminate i)
      , string "subscriptions" >> spaces >> eof >> pure (SessionsSelectSubscriptions i)
      ]
    transports :: Parser Request
    transports = spaces >> choice
      [ eof >> pure TransportsStatus
      , try $ string "start"  >> spaces >> eof >> pure TransportsStart
      , try $ string "stop"   >> spaces >> eof >> pure TransportsStop
      ]
