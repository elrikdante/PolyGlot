{-# LANGUAGE OverloadedStrings #-}
module LogParser.LogParser
       (IP(..),
        parseIP,
        parseTime,
        parseProduct)
       where

import           Control.Applicative
import           Data.Attoparsec.Char8
import qualified Data.Time as T
import           Data.Word

-- Word8: 8-bit unsigned integer values
data IP = IP Word8 Word8 Word8 Word8 deriving Show

-- 
data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

data LogEntry = LogEntry {
  entryTime    :: T.LocalTime,
 entryIP      :: IP,
 entryProduct :: Product} deriving Show

type Log = [LogEntry]



parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

parseProduct :: Parser Product
parseProduct =
      (string "mouse" >> return Mouse)
  <|> (string "keyboard" >> return Keyboard)
  <|> (string "speakers" >> return Speakers)
  <|> (string "monitor" >> return Monitor)

parseTime :: Parser T.LocalTime
parseTime = do
  yyyy <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  dd <- count 2 digit
  char ' '
  hh <- count 2 digit
  char ':'
  jj <- count 2 digit
  char ':'
  ss <- count 2 digit
  return $
    T.LocalTime { T.localDay  = T.fromGregorian (read yyyy) (read mm) (read dd)
                 ,T.localTimeOfDay = T.TimeOfDay (read hh) (read jj) (read ss)
                }
  
