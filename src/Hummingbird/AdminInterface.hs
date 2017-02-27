{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module Hummingbird.AdminInterface ( runCommandLineInterface, run ) where

import           Control.Concurrent.Async
import           Control.Exception              (SomeException, bracket, catch)
import           Control.Monad                  (forM_, forever, void, when)
import           Control.Monad.Trans.Class      (lift)
import qualified Data.Binary                    as B
import qualified Data.Binary.Get                as B
import qualified Data.Binary.Put                as B
import           Data.Bits
import qualified Data.ByteString                as BS
import           Data.Int
import           GHC.Generics                   (Generic)
import qualified System.Console.Haskeline       as H
import           System.Exit
import qualified System.FilePath                as FilePath
import           System.IO                      (FilePath, hPutStrLn, stderr)
import qualified System.Log.Logger              as LOG
import qualified System.Posix.Files             as Files
import qualified System.Socket                  as S
import qualified System.Socket.Family.Unix      as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S

import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T


import qualified Network.MQTT.Broker            as Broker
import qualified Network.MQTT.RoutingTree       as R
import qualified Network.MQTT.Session           as Session

import qualified Hummingbird.Configuration      as C

runCommandLineInterface :: C.Config auth -> IO ()
runCommandLineInterface config =
  case S.socketAddressUnixPath (T.encodeUtf8 $ T.pack $ C.adminSocketPath $ C.admin config) of
    Nothing -> do
      hPutStrLn stderr $ "Invalid path: " ++ C.adminSocketPath (C.admin config)
      exitFailure
    Just addr -> bracket
      ( S.socket :: IO (S.Socket S.Unix S.Stream S.Default) ) S.close
      (\sock-> do
        putStrLn banner
        S.connect sock addr
        H.runInputT H.defaultSettings (loop sock)
      )
  where
    loop :: S.Socket S.Unix S.Stream S.Default -> H.InputT IO ()
    loop peer = do
      H.outputStrLn ""
      printBrokerInfo
      H.outputStrLn ""
      forever $ do
        minput <- H.getInputLine prompt
        case minput of
          Nothing    -> printHelp
          Just input -> case input of
            "broker"       -> printBrokerInfo
            "help"         -> printHelp
            "session"      -> printSessionList
            _              -> H.outputStrLn $ lightRed "Unknown command. Try 'help'!"
      where

        printBrokerInfo = request peer BrokerInfoRequest >>= \case
          BrokerInfoResponse info -> do
            format "Uptime           " $ formatUptime (brokerUptime info)
            format "Sessions         " $ show (brokerSessionCount info)
            format "Subscriptions    " $ show (brokerSubscriptionCount info)
            format "Throughput       " "20,454/s 9,779/s 15,831/s"
          _ -> protocolViolation
          where
            format key value =
              H.outputStrLn $ cyan key ++ ": " ++ lightCyan value ++ "\ESC[0m\STX"
            formatUptime uptime =
              show days ++ " day" ++ (if days /= 1 then "s, " else ", ") ++
              leftPad 2 '0' (show hours) ++ ":" ++ leftPad 2 '0' (show minutes) ++ ":" ++ leftPad 2 '0' (show seconds)
              where
                days   = quot uptime (24*3600)
                hours   = rem uptime (24*3600) `quot` 3600
                minutes = rem uptime 3600 `quot` 60
                seconds = rem uptime 60

        printSessionList = request peer SessionListRequest >>= \case
          SessionListResponse ss -> forM_ ss $ \session->
            H.outputStrLn $ statusDot (sessionStatus session) ++
            leftPad 8 ' ' (show $ sessionIdentifier session) ++
            leftPad 30 ' ' (show $ sessionClientIdentifier session)
          _ -> protocolViolation
          where
            statusDot SessionConnected      = lightGreen dot
            statusDot SessionConnectedClean = lightBlue dot
            statusDot SessionDisconnected   = lightRed dot

    printHelp = do
      H.outputStrLn "broker                    : show broker stats"
      H.outputStrLn "help                      : show this help"
      H.outputStrLn "session"
      H.outputStrLn "  list                    : list all sessions"
      H.outputStrLn "  [SID]                   : a certain session identified by id"
      H.outputStrLn "    show                  : show session information"
      H.outputStrLn "    disconnect            : disconnect associated client (if any)"
      H.outputStrLn "    terminate             : terminate session (and disconnect client)"

    protocolViolation = error "protocolViolation"
    request peer msg =
      sendMessage peer msg >> receiveMessage peer
    sendMessage peer msg =
      lift $ void $ S.sendAllBuilder peer 4096 (B.execPut $ B.put msg) mempty
    receiveMessage peer =
      lift $ execute decoder
      where
        decoder = B.runGetIncremental B.get
        execute = \case
          B.Partial continuation -> do
            bs <- S.receive peer 4096 mempty
            execute $ continuation (if BS.null bs then Nothing else Just bs)
          B.Done _ _ msg -> pure msg
          B.Fail _ _ failure -> error failure

banner :: String
banner =
  "\ESC[0;31m" ++
  "|_     _ _  _ _ . _  _ |_ . _ _|\n" ++
  "| ||_|| | || | ||| |(_||_)|| (_|\n" ++
  "\ESC[0;33m---------------------"    ++
  "\ESC[0;31m_|\ESC[0;33m---------\ESC[0m\STX"

prompt :: String
prompt = "\ESC[0;32m\STXhummingbird\ESC[0;35m>\ESC[0m\STX "

leftPad :: Int -> Char -> String -> String
leftPad i c s = replicate (i - length s) c ++ s

rightPad :: Int -> Char -> String -> String
rightPad i c s = s ++ replicate (i - length s) c

purple  :: String -> String
purple = color "0;35"

cyan  :: String -> String
cyan = color "0;36"

darkGrey  :: String -> String
darkGrey = color "1;30"

lightRed   :: String -> String
lightRed = color "1;31"

lightGreen :: String -> String
lightGreen = color "1;32"

lightBlue  :: String -> String
lightBlue = color "1;34"

lightCyan  :: String -> String
lightCyan = color "1;36"

color :: String -> String -> String
color c s = "\ESC[" ++ c ++ "m\STX" ++ s ++ "\ESC[0m\STX"

dot   :: String
dot    = "\x2022"

data Request
   = BrokerInfoRequest
   | SessionListRequest
   deriving (Eq, Ord, Show, Generic)

data Response
   = BrokerInfoResponse BrokerInfo
   | SessionListResponse [SessionInfo]
   deriving (Eq, Ord, Show, Generic)

data BrokerInfo
   = BrokerInfo
   { brokerUptime            :: Int64
   , brokerSessionCount      :: Int
   , brokerSubscriptionCount :: Int
   }
   deriving (Eq, Ord, Show, Generic)

data SessionInfo
   = SessionInfo
   { sessionIdentifier       :: Int
   , sessionClientIdentifier :: T.Text
   , sessionStatus           :: SessionInfoStatus
   , sessionCreatedAt        :: Int64
   }
   deriving (Eq, Ord, Show, Generic)

getBrokerInfo  :: Broker.Broker auth -> IO BrokerInfo
getBrokerInfo broker = BrokerInfo
  <$> Broker.getUptime broker
  <*> (IM.size <$> Broker.getSessions broker)
  <*> (R.foldl' (\acc set-> acc + IS.size set) 0 <$> Broker.getSubscriptions broker)

sessionInfo :: Session.Session auth -> SessionInfo
sessionInfo session = SessionInfo
  { sessionIdentifier = Session.sessionIdentifier session
  , sessionClientIdentifier = Session.sessionClientIdentifier session
  , sessionStatus = SessionConnectedClean
  , sessionCreatedAt = 0
  }

data SessionInfoStatus
   = SessionConnected
   | SessionConnectedClean
   | SessionDisconnected
   deriving (Eq, Ord, Show, Generic)

instance B.Binary Request
instance B.Binary Response
instance B.Binary BrokerInfo
instance B.Binary SessionInfo
instance B.Binary SessionInfoStatus

run :: C.Config auth -> Broker.Broker auth -> IO a
run config broker = do
    let path = C.adminSocketPath $ C.admin config
    let directory = FilePath.takeDirectory path
    -- Check parent directory permissions. The parent directory must only be
    -- accessible by owner and group.
    directoryMode <- Files.fileMode <$> Files.getFileStatus directory
    when (directoryMode .&. (Files.otherReadMode .|. Files.otherWriteMode .|. Files.otherExecuteMode) /= 0) $ do
      hPutStrLn stderr $ "The directory containing the admin socket must not be world-accessible!"
      exitFailure

    case S.socketAddressUnixPath (T.encodeUtf8 $ T.pack path) of
      Nothing -> do
        hPutStrLn stderr $ "Invalid path: " ++ C.adminSocketPath (C.admin config)
        exitFailure
      Just addr -> bracket
        ( S.socket :: IO (S.Socket S.Unix S.Stream S.Default) )
        (\server-> do
          S.close server
          Files.removeLink path
        )
        (\server-> do
          S.bind server addr
          S.listen server 1
          -- This is a nifty trick inspired by a @snoyman post: We want to catch
          -- all synchronous exceptions and restart the handler, but stop as soon
          -- as the thread receives any async exception. The only way to do it is
          -- by creating another worker thread.
          forever $ withAsync (acceptAndHandle server) $ \handler->
            waitCatch handler >>= \case
              Left e   -> LOG.warningM "AdminInterface" $ "Connection terminated with " ++ show (e :: SomeException)
              Right () -> LOG.infoM "AdminInterface" "Administrator disconnected."
        )
  where
    acceptAndHandle server =
      bracket (S.accept server) (S.close . fst)
        (\(peer,_)-> do
          LOG.infoM "AdminInterface" "Administrator connected via local unix domain socket."
          withMessages peer $ \case
            BrokerInfoRequest ->
              sendMessage peer =<< BrokerInfoResponse <$> getBrokerInfo broker
            SessionListRequest -> do
              sessions <- Broker.getSessions broker
              sendMessage peer $ SessionListResponse $ fmap sessionInfo (IM.elems sessions)
        ) `catch` (\e->
          LOG.infoM "AdminInterface" $ "Administrator disconnected with " ++ show (e :: S.SocketException) ++ "."
        )
    withMessages sock digest = loop mempty
      where
        loop initial =
          execute (decoder `B.pushChunk` initial) >>= \case
            Nothing -> pure ()
            Just (msg, leftover) -> do
              digest msg
              loop leftover
        decoder = B.runGetIncremental B.get
        execute = \case
          B.Partial continuation -> do
            bs <- S.receive sock 4096 mempty
            if BS.null bs
              then pure Nothing -- peer closed connection
              else execute $ continuation (Just bs)
          B.Done leftover _ msg -> pure $ Just (msg, leftover)
          B.Fail _ _ failure -> do
            LOG.warningM "AdminInterface" $ "Parser error: " ++ show failure
            pure Nothing
    sendMessage sock msg =
      void $ S.sendAllBuilder sock 4096 (B.execPut $ B.put msg) mempty
