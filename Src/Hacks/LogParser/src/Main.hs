{-# LANGUAGE OverloadedStrings #-}
module Main
       (main)
       where
import           Data.Attoparsec.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           LogParser.LogParser


main :: IO ()
main = B.getContents >>= \stdIn ->
      case parseOnly parseNginxLog stdIn of
       Right entries   -> print $ length entries
       Left err        -> print $ "Not A Valid NGINX Log Err:" ++ err
