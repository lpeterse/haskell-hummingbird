{-# LANGUAGE DeriveGeneric #-}
module Hummingbird.AdminInterface ( run, runCommandLineInterface, runListeningSocket ) where

import           Control.Exception          (bracket)
import           Control.Monad              (forever)
import           Control.Monad.Trans.Class  (lift)
import           GHC.Generics               (Generic)
import qualified System.Console.Haskeline   as H
import           System.IO                  (FilePath)
import qualified System.Log.Logger          as LOG
import qualified System.Socket              as S
import qualified System.Socket.Family.Inet  as S
import qualified System.Socket.Protocol.TCP as S
import qualified System.Socket.Type.Stream  as S

import qualified Network.MQTT.Broker        as Broker

runListeningSocket :: Broker.Broker a -> IO ()
runListeningSocket broker = bracket
  ( S.socket :: IO (S.Socket S.Inet S.Stream S.TCP) ) S.close
  (\server-> do
    S.setSocketOption server (S.ReuseAddress True)
    S.bind server (S.SocketAddressInet S.inetLoopback 38777)
    S.listen server 1
    forever $ bracket (S.accept server)
      (\(peer,_)-> do
        S.close peer
        LOG.infoM "AdminInterface" "Administrator disconnected."
      )
      (\(peer,_)-> do
        LOG.infoM "AdminInterface" "Administrator connected via local unix domain socket."
        pure ()
      )
  )

runCommandLineInterface :: FilePath -> IO ()
runCommandLineInterface path = bracket
  ( S.socket :: IO (S.Socket S.Inet S.Stream S.TCP) ) S.close
  (\client-> do
    S.connect client (S.SocketAddressInet S.inetLoopback 38777)
    pure ()
  )

banner :: String
banner =
  "\ESC[0;31m" ++
  "|_     _ _  _ _ . _  _ |_ . _ _|\n" ++
  "| ||_|| | || | ||| |(_||_)|| (_|\n" ++
  "\ESC[0;33m---------------------"    ++
  "\ESC[0;31m_|\ESC[0;33m---------\ESC[0m\STX"

run :: FilePath -> IO ()
run path = do
  putStrLn banner
  H.runInputT H.defaultSettings loop
  where
    io :: IO a -> H.InputT IO a
    io  = lift
    prompt :: String
    prompt = "\ESC[0;32m\STXhummingbird\ESC[0;35m>\ESC[0m\STX "
    loop :: H.InputT IO ()
    loop = do
      H.outputStrLn ""
      printBrokerInfo
      H.outputStrLn ""
      forever $ do
        minput <- H.getInputLine prompt
        case minput of
          Nothing    -> printHelp
          Just input -> case input of
            "broker"         -> printBrokerInfo
            "help"         -> printHelp
            "session list" -> printSessionList
            _              -> H.outputStrLn $ red "Unknown command. Try 'help'!"

    printBrokerInfo =  do
      f "Uptime           " "8 days, 03:19:53"
      f "Sessions         " "8,536 (20,123)"
      f "Subscriptions    " "400,155,287"
      f "Throughput       " "20,454/s 9,779/s 15,831/s"
      where
        f key value = H.outputStrLn $ "\ESC[0;36m\STX" ++ key ++ ": \ESC[1;36m\STX" ++ value ++ "\ESC[0m\STX"

    printSessionList = do
      H.outputStrLn $ green dot ++ "  352   TIADemo                                "
      H.outputStrLn $ red   dot ++ "  453   c4658c1b-7c04-44f4-a31b-4fb8c7191b23   "
      H.outputStrLn $ blue  dot ++ " 2348   ubx-2384h9r23fn23f8293fh23f"

    printHelp = do
      H.outputStrLn "help                      : show this help"
      H.outputStrLn "session"
      H.outputStrLn "  list                    : list all sessions"
      H.outputStrLn "  [SID]                   : a certain session identified by id"
      H.outputStrLn "    show                  : show session information"
      H.outputStrLn "    disconnect            : disconnect associated client (if any)"
      H.outputStrLn "    terminate             : terminate session (and disconnect client)"

grey  :: String -> String
grey s = "\ESC[1;30m\STX" ++ s ++ "\ESC[0m\STX"

green :: String -> String
green s = "\ESC[1;32m\STX" ++ s ++ "\ESC[0m\STX"

red   :: String -> String
red s  = "\ESC[1;31m\STX" ++ s ++ "\ESC[0m\STX"

blue  :: String -> String
blue s = "\ESC[1;34m\STX" ++ s ++ "\ESC[0m\STX"

dot   :: String
dot    = "\x2022"

data Request
   = RequestBrokerInfo
   deriving (Eq, Ord, Show, Generic)

data Response
   = ResponseBrokerInfo
   deriving (Eq, Ord, Show, Generic)
