--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module Main where

import qualified Crypto.BCrypt         as BCrypt
import qualified Data.ByteString.Char8 as BS
import           System.Exit
import           System.IO

main :: IO ()
main = do
  hSetEcho stdin False
  password <- BS.getLine
  mhash <- BCrypt.hashPasswordUsingPolicy
    (BCrypt.fastBcryptHashingPolicy { BCrypt.preferredHashCost = 8 })
    password
  case mhash of
    Nothing   -> exitFailure
    Just hash -> BS.putStrLn hash
