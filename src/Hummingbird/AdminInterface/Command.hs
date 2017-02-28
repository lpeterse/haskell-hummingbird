{-# LANGUAGE LambdaCase #-}
module Hummingbird.AdminInterface.Command where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

data Command
   = Help
   | Broker BrokerCommand
   | Sessions SessionsCommand
   deriving (Eq, Ord, Show)

data BrokerCommand
   = BrokerStats
   deriving (Eq, Ord, Show)

data SessionsCommand
   = SessionsList
   | SessionsSelect Int SessionCommand
   deriving (Eq, Ord, Show)

data SessionCommand
   = SessionShow
   | SessionDisconnect
   | SessionTerminate
   deriving (Eq, Ord, Show)

parseCommand :: String -> Either String Command
parseCommand s = case parse commandParser "" s of
  Left e -> Left (show e)
  Right c -> Right c

commandParser :: Parser Command
commandParser = spaces >> choice
  [ string "help"     >> spaces >> eof >> pure Help
  , string "broker"   >> Broker <$> brokerCommand
  , string "sessions" >> Sessions <$> sessionsCommand
  ]
  where
    brokerCommand = spaces >> eof >> pure BrokerStats
    sessionsCommand = spaces >> choice
      [ string "list" >> spaces >> eof >> pure SessionsList
      , SessionsSelect <$> (read <$> many1 digit) <*> sessionCommand
     ]
    sessionCommand = spaces >> choice
      [ string "show"       >> spaces >> eof >> pure SessionShow
      , string "disconnect" >> spaces >> eof >> pure SessionDisconnect
      , string "terminate"  >> spaces >> eof >> pure SessionTerminate
      ]
