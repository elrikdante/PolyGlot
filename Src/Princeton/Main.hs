module Main
(main)

where

import Princeton.A4
import Data.List.Split(splitOn)

main :: IO()
main = do
         d <- readFile "data.txt"

         let
           (size:unions) = lines d

           sSize :: Int
           sSize = read size

           pairs :: [[Int]]
           pairs = map (map read . (splitOn " ")) unions

           qf    = initQF sSize 0
           qf'   = foldl (\ qf (n:m:[]) -> union qf n m) qf pairs
    
         putStrLn $ "Size:" ++ (show sSize)
         putStrLn $ show pairs
         putStrLn $ show $ qf' 
         putStrLn $ "Connected C:" ++ (show $  connectedComponents qf')
         putStrLn (show (isConnected qf' 0 7))
         putStrLn (show (isConnected qf' 8 9   ))
