{-# LANGUAGE LambdaCase    #-}
module Hummingbird.Administration.Server ( runServerInterface ) where

import           Control.Concurrent.Async
import           Control.Exception                  (SomeException, bracket,
                                                     catch, try)
import           Control.Monad                      (forever, void, when)
import qualified Data.Binary                        as B
import qualified Data.Binary.Get                    as B
import qualified Data.Binary.Put                    as B
import           Data.Bits
import qualified Data.ByteString                    as BS
import qualified Data.IntMap                        as IM
import qualified Data.IntSet                        as IS
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           System.Exit
import qualified System.FilePath                    as FilePath
import           System.IO                          (hPutStrLn,                                                     stderr)
import qualified System.Log.Logger                  as LOG
import qualified System.Posix.Files                 as Files
import qualified System.Socket                      as S
import qualified System.Socket.Family.Unix          as S
import qualified System.Socket.Protocol.Default     as S
import qualified System.Socket.Type.Stream          as S

import           Network.MQTT.Authentication (Authenticator)
import qualified Network.MQTT.Broker                as Broker
import qualified Network.MQTT.RoutingTree           as R
import qualified Network.MQTT.Session               as Session

import qualified Hummingbird.Administration.Request as Request
import qualified Hummingbird.Administration.Response as Response
import qualified Hummingbird.Configuration          as C

runServerInterface :: Authenticator auth => C.Config auth -> Broker.Broker auth -> IO a
runServerInterface config broker = do
    let path = C.adminSocketPath $ C.admin config
    let directory = FilePath.takeDirectory path
    -- Check parent directory permissions. The parent directory must only be
    -- accessible by owner and group.
    directoryMode <- Files.fileMode <$> Files.getFileStatus directory
    when (directoryMode .&. (Files.otherReadMode .|. Files.otherWriteMode .|. Files.otherExecuteMode) /= 0) $ do
      hPutStrLn stderr $ show directory ++ " must not be world-accessible!"
      exitFailure
    -- Make sure the socket file does not already exist.
    -- QUESTION: Is this a good idea? It could prevent automatic restart after
    -- unclean shutdown.
    exists <- Files.fileExist path
    when exists $ do
      hPutStrLn stderr $ show path ++ " already exists. Other broker running?"
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
              Left e   -> LOG.warningM "Administration" $ "Connection terminated with " ++ show (e :: SomeException)
              Right () -> LOG.infoM "Administration" "Administrator disconnected."
        )
  where
    acceptAndHandle server =
      bracket (S.accept server) (S.close . fst)
        (\(sock,_)-> do
          LOG.infoM "Administration" "Administrator connected via local unix domain socket."
          receiveMessage sock >>= \case
            Nothing -> pure ()
            Just request -> do
              LOG.infoM "Administration" $ "Administrator executed command: " ++ show request
              response <- process request broker
              sendMessage sock response
        ) `catch` (\e->
          LOG.infoM "Administration" $ "Administrator disconnected with " ++ show (e :: S.SocketException) ++ "."
        )
    receiveMessage :: S.Socket S.Unix S.Stream S.Default -> IO (Maybe Request.Request)
    receiveMessage sock = execute (decoder `B.pushChunk` mempty) >>= \case
      Nothing -> pure Nothing
      Just (msg, _) -> pure (Just msg)
      where
        decoder = B.runGetIncremental B.get
        execute = \case
          B.Partial continuation -> do
            bs <- S.receive sock 4096 mempty
            if BS.null bs
              then pure Nothing -- peer closed connection
              else execute $ continuation (Just bs)
          B.Done leftover _ msg -> pure $ Just (msg, leftover)
          B.Fail _ _ failure -> do
            LOG.warningM "Administration" $ "Parser error: " ++ show failure
            pure Nothing
    sendMessage :: S.Socket S.Unix S.Stream S.Default -> Response.Response -> IO ()
    sendMessage sock msg =
      void $ S.sendAllBuilder sock 4096 (B.execPut $ B.put msg) mempty

process :: Authenticator auth => Request.Request -> Broker.Broker auth -> IO Response.Response
process Request.Help _ =
  pure Response.Help

process (Request.Broker _) broker =
  Response.BrokerInfo
  <$> pure "0.1.0-SNAPSHOT"
  <*> Broker.getUptime broker
  <*> (IM.size <$> Broker.getSessions broker)
  <*> (R.foldl' (\acc set-> acc + IS.size set) 0 <$> Broker.getSubscriptions broker)

process Request.Sessions broker = do
  sessions <- Broker.getSessions broker
  pure $ Response.SessionList $ fmap sessionInfo (IM.elems sessions)
  where
    sessionInfo :: Session.Session auth -> Response.SessionInfo
    sessionInfo session = Response.SessionInfo
      { Response.sessionIdentifier = Session.sessionIdentifier session
      , Response.sessionClientIdentifier = Session.sessionClientIdentifier session
      , Response.sessionStatus = Response.SessionConnectedClean
      , Response.sessionCreatedAt = 0
      }

process (Request.SessionsSelectInfo sid) broker = do
  sessions <- Broker.getSessions broker
  pure $ Response.SessionList $ fmap sessionInfo (IM.elems sessions)
  where
    sessionInfo :: Session.Session auth -> Response.SessionInfo
    sessionInfo session = Response.SessionInfo
      { Response.sessionIdentifier = Session.sessionIdentifier session
      , Response.sessionClientIdentifier = Session.sessionClientIdentifier session
      , Response.sessionStatus = Response.SessionConnectedClean
      , Response.sessionCreatedAt = 0
      }

process (Request.SessionsSelectDisconnect sid) broker =
  pure (Response.Failure "NOT IMPLEMENTED")

process (Request.SessionsSelectTerminate sid) broker =
  try (Broker.terminateSession broker sid) >>= \case
    Right () -> pure Response.Success
    Left e -> pure (Response.Failure $ show (e :: SomeException))
