{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sebastian Clausen 2017
-- License     :  MIT
--
-- Maintainer  :  github@sebastian-clausen.de
-- Stability   :  experimental
--------------------------------------------------------------------------------
module Hummingbird.Administration.Cli where

import           Control.Exception                   (bracket)
import           Control.Monad                       (forever, void)
import           Control.Monad.Trans.Class           (lift)
import qualified Data.Binary                         as B
import qualified Data.Binary.Get                     as B
import qualified Data.Binary.Put                     as B
import qualified Data.ByteString                     as BS
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Options
import qualified System.Console.Haskeline            as H
import           System.Exit
import           System.IO                           (hPutStrLn, stderr)
import qualified System.Socket                       as S
import qualified System.Socket.Family.Unix           as S
import qualified System.Socket.Protocol.Default      as S
import qualified System.Socket.Type.Stream           as S

import           Hummingbird.Administration.Escape
import qualified Hummingbird.Administration.Request  as Request
import qualified Hummingbird.Administration.Response as Response

data CliOptions = CliOptions
  { interactive :: Bool
  , command     :: String
  , socket      :: FilePath
  }

instance Options CliOptions where
  defineOptions = CliOptions
    <$> simpleOption "interactive" True "Start an interactive command line interpreter."
    <*> simpleOption "command" "help" "The command to execute (only when non-interactive)."
    <*> simpleOption "socket" "/run/hummingbird/S.hummingbird-admin" "The brokers local domain socket for administration."

-- | Execute a command using a local unix domain socket
--   served by a running hummingbird broker.
run :: mainOptions -> CliOptions -> [String] -> IO ()
run _ opts _ =
    if interactive opts
      then runInteractive opts
      else runNonInteractive opts

runInteractive :: CliOptions -> IO ()
runInteractive opts = H.runInputT H.defaultSettings $ do
  H.outputStrLn banner
  lift (execRequest Request.BrokerStatus) >>= Response.render H.outputStrLn
  H.outputStrLn ""
  forever $ H.handleInterrupt (pure ()) $ H.withInterrupt $ H.getInputLine prompt >>= \case
    Nothing   -> lift exitSuccess -- user pressed ctrl+D
    Just line -> case Request.parse line of
      Right Request.Quit -> lift exitSuccess
      Right cmd          -> do
        response <- lift $ execRequest cmd
        Response.render H.outputStrLn response
      Left e             -> H.outputStrLn (lightRed  e)
  where
    execRequest :: Request.Request -> IO Response.Response
    execRequest cmd = case S.socketAddressUnixPath (T.encodeUtf8 $ T.pack $ socket opts) of
      Nothing -> do
        hPutStrLn stderr $ "Invalid socket: " ++ show (socket opts)
        exitFailure
      Just addr -> bracket
        ( S.socket :: IO (S.Socket S.Unix S.Stream S.Default) ) S.close
        (\sock-> S.connect sock addr >> sendMessage sock cmd >> receiveMessage sock )
      where
        sendMessage sock msg =
          void $ S.sendAllBuilder sock 4096 (B.execPut $ B.put msg) mempty
        receiveMessage sock = execute decoder
          where
            decoder = B.runGetIncremental B.get
            execute = \case
              B.Partial continuation -> do
                bs <- S.receive sock 4096 mempty
                execute $ continuation (if BS.null bs then Nothing else Just bs)
              B.Done _ _ msg -> pure msg
              B.Fail _ _ failure -> error failure

runNonInteractive :: CliOptions -> IO ()
runNonInteractive opts =
  case Request.parse (command opts) of
    Right c -> Response.render putStrLn =<< execRequest c
    Left  e -> hPutStrLn stderr e >> exitFailure
  where
    execRequest :: Request.Request -> IO Response.Response
    execRequest cmd =
      case S.socketAddressUnixPath (T.encodeUtf8 $ T.pack $ socket opts) of
        Nothing -> do
          hPutStrLn stderr $ "Invalid socket: " ++ show (socket opts)
          exitFailure
        Just addr -> bracket
          ( S.socket :: IO (S.Socket S.Unix S.Stream S.Default) ) S.close
          (\sock-> S.connect sock addr >> sendMessage sock cmd >> receiveMessage sock )
        where
          sendMessage sock msg =
            void $ S.sendAllBuilder sock 4096 (B.execPut $ B.put msg) mempty
          receiveMessage sock = execute decoder
            where
              decoder = B.runGetIncremental B.get
              execute = \case
                B.Partial continuation -> do
                  bs <- S.receive sock 4096 mempty
                  execute $ continuation (if BS.null bs then Nothing else Just bs)
                B.Done _ _ msg -> pure msg
                B.Fail _ _ failure -> error failure
