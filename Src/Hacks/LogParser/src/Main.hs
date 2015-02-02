{-# LANGUAGE OverloadedStrings #-}
module Main
       (main)
       where
import Data.Attoparsec.Char8
import LogParser.LogParser

main :: IO ()
main = print $ parseOnly parseIP "127.0.0.1"
