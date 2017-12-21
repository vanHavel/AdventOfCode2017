import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Debug.Trace
import Data.List hiding (find)

import Utility.UnionFind

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day12.txt"
  connections <- map parse <$> lines <$> readFile file
  let (groupsize, groupno) = runST $ solveDay12 connections
  print (groupsize, groupno)
  
-- parse a line of the input
parse :: String -> (Int, [Int])
parse s = let ws = words $ filter (/= ',') s in
  (read $ ws !! 0, map (read . (ws !!)) [2..length ws - 1])
  
-- create array and apply union find to find solution
solveDay12 :: [(Int, [Int])] -> ST s (Int, Int)
solveDay12 connections = do
  let n = length connections
  arr <- newListArray (0, n-1) [0..n-1] :: ST s (STArray s Int Int)
  unionFind (concatMap edges connections) arr
  group0 <- groupOf arr 0
  regionCount <- regions arr
  return $ (length group0, regionCount)

--extract edges from pipe connections
edges :: (Int, [Int]) -> [Edge Int]
edges (a, xs) = map ((,) a) xs

