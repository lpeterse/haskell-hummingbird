{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.Administration.Request where

import qualified Data.Binary        as B
import           GHC.Generics       (Generic)
import           Text.Parsec        as P
import           Text.Parsec.String (Parser)

data Request
   = Help
   | Broker Broker
   | Sessions
   | SessionsSelectInfo Int
   | SessionsSelectDisconnect Int
   | SessionsSelectTerminate Int
   deriving (Eq, Ord, Show, Generic)

data Broker
   = BrokerInfo
   deriving (Eq, Ord, Show, Generic)

instance B.Binary Request
instance B.Binary Broker

parse :: String -> Either String Request
parse s = case P.parse requestParser "" s of
  Left e  -> Left (show e)
  Right c -> Right c

requestParser :: Parser Request
requestParser = spaces >> choice
  [ string "help"     >> spaces >> eof >> pure Help
  , string "broker"   >> Broker <$> brokerRequest
  , string "sessions" >> sessions
  ]
  where
    brokerRequest = spaces >> choice
      [ string "info" >> eof >> pure BrokerInfo
      ]
    sessions = spaces >> choice
      [ eof >> pure Sessions
      , (read <$> many1 digit :: Parser Int) >>= sessionsSelect
     ]
    sessionsSelect :: Int -> Parser Request
    sessionsSelect i = spaces >> choice
      [ string "info"       >> spaces >> eof >> pure (SessionsSelectInfo i)
      , string "disconnect" >> spaces >> eof >> pure (SessionsSelectDisconnect i)
      , string "terminate"  >> spaces >> eof >> pure (SessionsSelectTerminate i)
      ]
