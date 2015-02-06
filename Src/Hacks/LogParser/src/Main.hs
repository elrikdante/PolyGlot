{-# LANGUAGE OverloadedStrings #-}
module Main
       (main)
       where
import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import LogParser.LogParser

main :: IO ()
main = do
  let
    str = "10.1.1.167 - - [29/Nov/2014:05:12:09 +0000] \"GET /guest/challenge/f43dd5e3-18b3-4363-beb7-469a29c538c8 HTTP/1.1\" 404 65 \"-\" \"AdsBot-Google (+http://www.google.com/adsbot.html)\" \"72.14.199.65\""
  d <- B.readFile "data.txt"
  print $ parseOnly parseNginxLogEntry "10.1.1.167 - - [29/Nov/2014:05:12:09+0000] \"GET /guest/challenge/f43dd5e3-18b3-4363-beb7-469a29c538c8 HTTP/1.1\"\n"
