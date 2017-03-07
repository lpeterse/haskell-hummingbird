{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Hummingbird.Administration.Server ( runServerInterface ) where

import           Control.Concurrent.Async
import           Control.Exception                     (SomeException, bracket,
                                                        catch, try)
import           Control.Monad                         (forever, void, when)
import           Data.Aeson                            (FromJSON)
import qualified Data.Binary                           as B
import qualified Data.Binary.Get                       as B
import qualified Data.Binary.Put                       as B
import           Data.Bits
import qualified Data.ByteString                       as BS
import qualified Data.IntMap                           as IM
import qualified Data.IntSet                           as IS
import           Data.Proxy
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           System.Exit
import qualified System.FilePath                       as FilePath
import           System.IO                             (hPutStrLn, stderr)
import qualified System.Log.Logger                     as LOG
import qualified System.Posix.Files                    as Files
import qualified System.Socket                         as S
import qualified System.Socket.Family.Unix             as S
import qualified System.Socket.Protocol.Default        as S
import qualified System.Socket.Type.Stream             as S

import qualified Network.MQTT.Broker                   as Broker
import           Network.MQTT.Broker.Authentication    (Authenticator,
                                                        AuthenticatorConfig,
                                                        Principal (..))
import qualified Network.MQTT.Broker.Session           as Session
import qualified Network.MQTT.Broker.SessionStatistics as SS
import qualified Network.MQTT.Trie                     as R

import qualified Hummingbird.Administration.Request    as Request
import qualified Hummingbird.Administration.Response   as Response
import           Hummingbird.Broker
import qualified Hummingbird.Configuration             as C

runServerInterface :: (Authenticator auth, FromJSON (AuthenticatorConfig auth)) => Proxy (C.Config auth) -> HummingbirdBroker auth -> IO a
runServerInterface authProxy hum = do
    config <- getConfig hum
    let path = C.adminSocketPath $ C.admin (config `asProxyTypeOf` authProxy)
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
              response <- process request hum
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

process :: (Authenticator auth, FromJSON (AuthenticatorConfig auth)) => Request.Request -> HummingbirdBroker auth -> IO Response.Response
process Request.Help _ =
  pure Response.Help

process Request.Broker broker =
  Response.BrokerInfo
  <$> pure "0.1.0-SNAPSHOT"
  <*> Broker.getUptime (humBroker broker)
  <*> (IM.size <$> Broker.getSessions (humBroker broker))
  <*> (R.foldl' (\acc set-> acc + IS.size set) 0 <$> Broker.getSubscriptions (humBroker broker))

process Request.Sessions broker = do
  sessions <- Broker.getSessions (humBroker broker)
  Response.SessionList <$> mapM sessionListElement (IM.elems sessions)
  where
    sessionListElement :: Session.Session auth -> IO Response.SessionListElement
    sessionListElement session = do
      connection <- Session.getConnection session
      pure Response.SessionListElement {
          Response.lsessionIdentifier = Session.sessionIdentifier session
        , Response.lsessionClientIdentifier = Session.sessionClientIdentifier session
        , Response.lsessionPrincipalIdentifier = Session.sessionPrincipalIdentifier session
        , Response.lsessionCreatedAt = Session.sessionCreatedAt session
        , Response.lsessionConnection = connection
        }

process (Request.SessionsSelect sid) broker = do
  sessions <- Broker.getSessions (humBroker broker)
  case IM.lookup sid sessions of
    Nothing -> pure (Response.Failure "session not found")
    Just s  -> Response.Session <$> sessionInfo s
  where
    sessionInfo :: Session.Session auth -> IO Response.SessionInfo
    sessionInfo session = do
      connection <- Session.getConnection session
      stats <- SS.snapshot $ Session.sessionStatistics session
      subscriptions <- Session.getSubscriptions session
      quota <- principalQuota <$> Session.getPrincipal session
      pure Response.SessionInfo
        { Response.sessionIdentifier = Session.sessionIdentifier session
        , Response.sessionClientIdentifier = Session.sessionClientIdentifier session
        , Response.sessionPrincipalIdentifier = Session.sessionPrincipalIdentifier session
        , Response.sessionConnection = connection
        , Response.sessionCreatedAt = Session.sessionCreatedAt session
        , Response.sessionStatistics = stats
        , Response.sessionQuota = quota
        }

process (Request.SessionsSelectDisconnect sid) broker =
  try (Broker.disconnectSession (humBroker broker) sid) >>= \case
    Right () -> pure (Response.Success "Done.")
    Left e -> pure (Response.Failure $ show (e :: SomeException))

process (Request.SessionsSelectTerminate sid) broker =
  try (Broker.terminateSession (humBroker broker) sid) >>= \case
    Right () -> pure (Response.Success "Done.")
    Left e -> pure (Response.Failure $ show (e :: SomeException))

process (Request.SessionsSelectSubscriptions sid) broker = do
  sessions <- Broker.getSessions (humBroker broker)
  case IM.lookup sid sessions of
    Nothing -> pure (Response.Failure "session not found")
    Just s  -> Response.SessionSubscriptions . show <$> Session.getSubscriptions s

process Request.TransportsStatus broker =
  getTransportsStatus broker >>= \case
    Running -> pure (Response.Success "Running.")
    Stopped -> pure (Response.Success "Stopped.")
    StoppedWithException e -> pure (Response.Failure $ "Stopped with exception: " ++ show e)

process Request.TransportsStart broker =
  try (startTransports broker) >>= \case
    Right () -> pure (Response.Success "Done.")
    Left e -> pure (Response.Failure $ show (e :: SomeException))

process Request.TransportsStop broker =
  try (stopTransports broker) >>= \case
    Right () -> pure (Response.Success "Done.")
    Left e -> pure (Response.Failure $ show (e :: SomeException))

process Request.Config broker =
  getConfig broker >>= \config-> pure (Response.Success $ show config)

process Request.ConfigReload broker =
  reloadConfig broker >>= \case
    Right _ -> pure (Response.Success "Done.")
    Left e -> pure (Response.Failure e)
