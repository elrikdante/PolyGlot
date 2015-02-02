{-# LANGUAGE OverloadedStrings #-}
module Main
       (main)
       where
import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import LogParser.LogParser

main :: IO ()
main =
  do
    d <- B.readFile "data.txt"
    
    print $ parseOnly parseLog d
    print $ show $ map productFromID [1..4]
    
