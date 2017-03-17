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
module Main where

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
import qualified System.Posix.Files                  as Files
import qualified System.Socket                       as S
import qualified System.Socket.Family.Unix           as S
import qualified System.Socket.Protocol.Default      as S
import qualified System.Socket.Type.Stream           as S

import qualified Hummingbird.Administration.Request  as Request
import qualified Hummingbird.Administration.Response as Response

main :: IO ()
main = do
  args <- getArgs
  case Request.parse (unwords args) of
    Right cmd -> do
      response <- execRequest cmd
      Response.render putStrLn response
    Left e  -> hPutStrLn stderr e >> exitFailure
  where
    findSocket :: IO FilePath
    findSocket = do
      exists <- Files.fileExist "./hummingbird.socket"
      if exists
        then pure "./hummingbird.socket"
        else do
          exists' <- Files.fileExist "/var/run/hummingbird/hummingbird.socket"
          if exists'
            then pure "./hummingbird.socket"
            else hPutStrLn stderr "Cannot find hummingbird.socket" >> exitFailure
    execRequest :: Request.Request -> IO Response.Response
    execRequest cmd = do
      path <- findSocket
      case S.socketAddressUnixPath (T.encodeUtf8 $ T.pack path) of
        Nothing -> hPutStrLn stderr "Illegal state" >> exitFailure
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
