module Main
       (main)
       where
import Data.List.Split(splitOn)
import QuickFind

main :: IO (QuickFind Int)
main = do
  
  input <- readFile "data.txt"
  
  let
    (size:inputs) = lines input
  
    _N :: Int
    _N = read size
  
    unions :: [[Int]]
    unions = map (map read . (splitOn " ")) inputs
    qf     = foldl (\qf' (n:m:[]) -> union n m qf') (mkQF _N 0) unions

  putStrLn $ show $ isConnected 7 0 qf
  putStrLn $ show $ isConnected 9 6 qf
  return qf
