{-# LANGUAGE OverloadedStrings #-}
module LogParser.LogParser
       (IP(..),
        parseLog,
        parseIP,
        parseTime,
        parseProduct,
        parseLogEntry,
        productFromID,
        productToID)
       where

import           Control.Applicative
import           Data.Attoparsec.Char8
import qualified Data.Time as T
import           Data.Word

-- Word8: 8-bit unsigned integer values
data IP = IP Word8 Word8 Word8 Word8 deriving Show

-- Enum
-- deriving Enum enables us to iterate over the constructors for a Product
-- it also lets us map from an Int to a Product.
--
data Product = Mouse | Keyboard | Monitor | Speakers deriving (Enum,Show)

productFromID :: Int -> Product
productFromID = toEnum . (subtract 1)

productToID :: Product -> Int
productToID = succ . fromEnum


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
  return T.LocalTime { T.localDay  = T.fromGregorian (read yyyy) (read mm) (read dd),
                       T.localTimeOfDay = T.TimeOfDay (read hh) (read jj) (read ss)
                     }
  
parseLogEntry :: Parser LogEntry
parseLogEntry = do
  time <- parseTime
  char ' '
  ip <- parseIP
  char ' '
  product <- parseProduct
  return $ LogEntry time ip product

parseLog :: Parser Log
parseLog = many $ parseLogEntry <* endOfLine
