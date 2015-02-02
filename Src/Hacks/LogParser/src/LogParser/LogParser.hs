{- #LANGUAGE OverloadedStrings #-}
module LogParser.LogParser
       (IP(..),
        parseIP,
        parseTime,
        parseProduct)
       where

import Data.Attoparsec.Char8
import qualified Data.Time as T
import Data.Word

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
parseProduct = return $ Mouse

parseTime :: Parser T.LocalTime
parseTime = undefined
