{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.Administration.CLI2 ( runCommandLineInterface ) where

import           Control.Exception                   (bracket)
import           Control.Monad                       (void)
import qualified Data.Binary                         as B
import qualified Data.Binary.Get                     as B
import qualified Data.Binary.Put                     as B
import qualified Data.ByteString                     as BS
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           System.Environment
import           System.Exit
import           System.IO                           (hPutStrLn, stderr)
import qualified System.Socket                       as S
import qualified System.Socket.Family.Unix           as S
import qualified System.Socket.Protocol.Default      as S
import qualified System.Socket.Type.Stream           as S

import qualified Hummingbird.Administration.Request  as Request
import qualified Hummingbird.Administration.Response as Response
import qualified Hummingbird.Configuration           as C

runCommandLineInterface :: C.Config auth -> IO ()
runCommandLineInterface config = do
  args <- getArgs
  case Request.parse (unwords args) of
    Right cmd -> do
      response <- execRequest cmd
      Response.render putStrLn response
    Left e  -> print e
  where
    execRequest :: Request.Request -> IO Response.Response
    execRequest cmd = case S.socketAddressUnixPath (T.encodeUtf8 $ T.pack $ C.adminSocketPath $ C.admin config) of
      Nothing -> do
        hPutStrLn stderr $ "Invalid path: " ++ C.adminSocketPath (C.admin config)
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
