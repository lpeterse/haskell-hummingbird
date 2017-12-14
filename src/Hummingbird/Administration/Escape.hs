module Hummingbird.Administration.Escape where
--------------------------------------------------------------------------------
-- |
-- Module      :  Hummingbird.Administration.Escape
-- Copyright   :  (c) Lars Petersen 2017
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.Text       as T

banner :: String
banner =
  "\ESC[0;35m" ++
  " _                               _             _     _         _ \n" ++
  "| |__  _   _ _ __ ___  _ __ ___ (_)_ __   __ _| |__ (_)_ __ __| |\n" ++
  "| '_ \\| | | | '_ ` _ \\| '_ ` _ \\| | '_ \\ / _` | '_ \\| | '__/ _` |\n" ++
  "| | | | |_| | | | | | | | | | | | | | | | (_| | |_) | | | | (_| |\n" ++
  "|_| |_|\\__,_|_| |_| |_|_| |_| |_|_|_| |_|\\__, |_.__/|_|_|  \\__,_|\n" ++
  "                                         |___/\ESC[0m\n"

prompt :: String
prompt = "\ESC[0;32mhummingbird\ESC[0;35m % \ESC[0m"

leftPad :: Int -> Char -> String -> String
leftPad i c s = replicate (i - length s) c ++ s

rightPad :: Int -> Char -> String -> String
rightPad i c s = s ++ replicate (i - length s) c

purple  :: String -> String
purple = color "0;35"

cyan  :: String -> String
cyan = color "0;36"

green :: String -> String
green = color "0;32"

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

lightYellow :: String -> String
lightYellow = color "1;93"

lightMagenta :: String -> String
lightMagenta = color "1;95"

color :: String -> String -> String
color c s = "\ESC[" ++ c ++ "m" ++ s ++ "\ESC[0m"

ago :: (Show a, Integral a) => a -> String
ago duration =
  inverted $ show days ++ " day" ++ (if days /= 1 then "s, " else ", ") ++
  leftPad 2 '0' (show hours) ++ ":" ++ leftPad 2 '0' (show minutes) ++
  ":" ++ leftPad 2 '0' (show seconds)
  where
    days   = quot (abs duration) (24*3600)
    hours   = rem (abs duration) (24*3600) `quot` 3600
    minutes = rem (abs duration) 3600 `quot` 60
    seconds = rem (abs duration) 60
    inverted s
      | signum duration == 1 = s
      | otherwise            = lightRed $ s ++ " ago"

escapeText :: T.Text -> String
escapeText = init . tail . show

escapeByteString :: BS.ByteString -> String
escapeByteString = init . tail . show
