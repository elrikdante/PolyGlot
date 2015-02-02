module Main
       (main)
       where
import Data.List.Split(splitOn)
import QuickUnion
-- API
{-
-- data.txt should be in this format
-- lines starting with -- are ignored
-- total nodes in the cluster
3
-- issue a union command to these nodes, this would point 2's root at 0's root.
2 0
-- etc
3 5
-}

main :: IO ()
main = do
  d <- readFile "data.txt"

  let 
    (size:unions) = lines d

    _N :: Int
    _N = read size

    unionParams :: [[Int]]
    unionParams = map (map read . (splitOn " ")) unions
    qu = foldl (\qu' (n:m:[]) -> union n m qu') (mkQU _N 0) unionParams
    
  putStrLn "::QuickUnion API::"
  putStrLn $ show qu
  putStrLn $ show unionParams
  putStrLn $ show (isConnected 5 0 qu)
  putStrLn $ show (isConnected 8 3 qu)
  putStrLn $ show (root 2 qu)
