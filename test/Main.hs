module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Hummingbird.Administration.Request
import           Network.MQTT.Broker.Session        (SessionIdentifier (..))

main :: IO ()
main = defaultMain $ testGroup "Hummingbird"
  [ parserTests
  ]

parserTests :: TestTree
parserTests = testGroup "Request Parser"
  [ testCase "help       #01" $ assertEqual "" (Right Help)             $ parse "help"
  , testCase "help       #02" $ assertEqual "" (Right Help)             $ parse "  help \t "
  , testCase "help       #03" $ assertEqual "" (Right Help)             $ parse "\nhelp\t\n\n\t "
  , testCase "broker     #01" $ assertEqual "" (Right BrokerStatus)     $ parse "broker"
  , testCase "config     #01" $ assertEqual "" (Right ConfigStatus)     $ parse "config"
  , testCase "config     #02" $ assertEqual "" (Right ConfigStatus)     $ parse "config status"
  , testCase "config     #03" $ assertEqual "" (Right ConfigReload)     $ parse "config reload"
  , testCase "auth       #01" $ assertEqual "" (Right AuthStatus)       $ parse "auth"
  , testCase "auth       #01" $ assertEqual "" (Right AuthStatus)       $ parse "auth status"
  , testCase "auth       #03" $ assertEqual "" (Right AuthRestart)      $ parse "auth restart"
  , testCase "transports #01" $ assertEqual "" (Right TransportsStatus) $ parse "transport"
  , testCase "transports #02" $ assertEqual "" (Right TransportsStatus) $ parse "transports"
  , testCase "transports #03" $ assertEqual "" (Right TransportsStatus) $ parse "transports status"
  , testCase "transports #04" $ assertEqual "" (Right TransportsStatus) $ parse "transport status"
  , testCase "session    #01" $ assertEqual "" (Right SessionList)      $ parse "session"
  , testCase "session    #02" $ assertEqual "" (Right SessionList)      $ parse "sessions"
  , testCase "session    #03" $ assertEqual "" (Right $ SessionStatus $ SessionIdentifier 0) $ parse "session 0"
  , testCase "session    #04" $ assertEqual "" (Right $ SessionStatus $ SessionIdentifier 0) $ parse "sessions 0"
  , testCase "session    #05" $ assertEqual "" (Right $ SessionStatus $ SessionIdentifier 234234223) $ parse "sessions 0000234234223"
  , testCase "session    #06" $ assertEqual "" (Right $ SessionStatus $ SessionIdentifier 0) $ parse "sessions 0 status"
  , testCase "session    #07" $ assertEqual "" (Right $ SessionDisconnect $ SessionIdentifier 0) $ parse "sessions 0 disconnect"
  , testCase "session    #08" $ assertEqual "" (Right $ SessionTerminate $ SessionIdentifier 0) $ parse "sessions 0 terminate"
  , testCase "session    #09" $ assertEqual "" (Right $ SessionSubscriptions $ SessionIdentifier 0) $ parse "sessions 0 subscriptions"
  , testCase "quit       #01" $ assertEqual "" (Right Quit)             $ parse "  quit "
  , testCase "quit       #02" $ assertEqual "" (Right Quit)             $ parse "  \r\n\t exit \n "
  ]
