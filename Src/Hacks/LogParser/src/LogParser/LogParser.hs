{-# LANGUAGE OverloadedStrings #-}
module LogParser.LogParser
       (IP(..),
        Log(..),
        LogEntry(..),
        parseLog,
        parseNginxLog,
        parseIP,
        parseTime,
        parseNginxTime,
        parseProduct,
        parseNginxLogEntry,
        parseLogEntry,
        productFromID,

        productToID)
       where

import           Control.Applicative
import           Data.Attoparsec.Char8 as P
import qualified Data.ByteString.Char8 as C8
import qualified Data.Time as T
import           Data.Word as B


-- Word8: 8-bit unsigned integer values
data IP = IP Word8 Word8 Word8 Word8 deriving Show

-- Enum
-- deriving Enum enables us to iterate over the constructors for a Product
-- it also lets us map from an Int to a Product.
data Product = Mouse | Keyboard | Monitor | Speakers deriving (Enum,Show)

data HttpMethod = GET | POST | PUT | PATCH deriving Show

productFromID :: Int -> Product
productFromID = toEnum . (subtract 1)

productToID :: Product -> Int
productToID = succ . fromEnum

data LogEntry = LogEntry {
                      entryTime    :: T.LocalTime,
                      entryIP      :: IP,
                     entryProduct :: Product
                }
                | NginxLogEntry {
                    entryTime :: T.LocalTime,
                    challengeToken :: String,
                    httpMethod :: HttpMethod
                }
              deriving Show

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

parseNginxTime :: Parser T.LocalTime
parseNginxTime = do
  dd <- count 2 digit
  char '/'
  mName <- P.takeWhile $ inClass "a-zA-Z"
  char '/'
  yyyy <- count 4 digit
  char ':'
  hh <- count 2 digit
  char ':'
  mm <- count 2 digit
  char ':'
  ss <- count 2 digit
  return T.LocalTime { T.localDay = T.fromGregorian (read yyyy) (monthAbrToInt mName) (read dd),
                         T.localTimeOfDay = T.TimeOfDay (read hh) (read mm) (read ss)
                       }

  where
    monthAbrToInt :: C8.ByteString -> Int
    monthAbrToInt "Jan" = 1
    monthAbrToInt "Feb" = 2
    monthAbrToInt "Mar" = 3
    monthAbrToInt "Apr" = 4
    monthAbrToInt "May" = 5
    monthAbrToInt "Jun" = 6
    monthAbrToInt "Jul" = 7
    monthAbrToInt "Aug" = 8
    monthAbrToInt "Sep" = 9
    monthAbrToInt "Oct" = 10
    monthAbrToInt "Nov" = 11
    monthAbrToInt "Dec" = 12
    monthAbrToInt a = error "Invalid MonthName"

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


parseHttpMethod :: Parser HttpMethod
parseHttpMethod = do
     (string "GET" >> return GET)
  <|>(string "POST" >> return POST)

parseNginxLogEntry :: Parser LogEntry
parseNginxLogEntry = do
  ip <- parseIP
  string " - - ["
  time <- parseNginxTime
  string "+0000] \""
  httpMethod <- parseHttpMethod
  string " /guest/challenge/"
  challengeToken <- takeTill ( ==' ')

  return NginxLogEntry {entryTime = time,
                        challengeToken = (C8.unpack challengeToken),
                        httpMethod = httpMethod
                       }

parseNginxLog :: Parser Log
parseNginxLog = many $ parseNginxLogEntry <* endOfLine

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
