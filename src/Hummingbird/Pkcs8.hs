{-# LANGUAGE OverloadedStrings #-}
module Hummingbird.Pkcs8 (publicKey) where

import           Control.Monad              (void)
import qualified Data.ASN1.BinaryEncoding   as ASN1
import qualified Data.ASN1.Encoding         as ASN1
import qualified Data.ASN1.Types            as ASN1
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as BS
import           Data.Monoid                ((<>))
import qualified Data.PEM                   as PEM
import qualified Data.X509                  as X509

publicKey :: BS.ByteString -> Either String X509.PubKey
publicKey bs = case parsePEM bs of
  Left e -> Left e
  Right [] -> Left "Empty result."
  Right (p:_) -> case PEM.pemName p of
    "PUBLIC KEY" -> case ASN1.decodeASN1' ASN1.BER (PEM.pemContent p) of
      Left e   -> Left (show e)
      Right xs -> case ASN1.fromASN1 xs of
        Left e       -> Left e
        Right (pk,_) -> Right pk
    other        -> Left $ show other ++ " does not introduce a PKCS8 public key file."

parsePEM :: BS.ByteString -> Either String [PEM.PEM]
parsePEM bs = case PEM.pemParseBS <$> sanitizePEM bs of
  Nothing -> Left "Syntax error (expecting PEM file)."
  Just r  -> r

sanitizePEM :: BS.ByteString -> Maybe BS.ByteString
sanitizePEM bs = case A.parseOnly parser bs of
    Left _  -> Nothing
    Right k -> Just k
  where
    begin = "-----BEGIN PUBLIC KEY-----"
    end   = "-----END PUBLIC KEY-----"
    parser = do
      void $ A.string begin
      body <- BS.filter (/=32) <$> A.takeWhile (/=45)
      void $ A.string end
      pure $ begin <> "\n" <> body <> "\n" <> end <> "\n"
