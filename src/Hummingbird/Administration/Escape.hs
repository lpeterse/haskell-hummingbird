module Hummingbird.Administration.Escape where

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
