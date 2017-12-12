import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Debug.Trace
import Data.List hiding (find)

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
  arr <- newListArray (0, n-1) [0..n-1] :: ST s (STUArray s Int Int)
  connected <- unionFind (concatMap edges connections) arr
  group0 <- filterM (\a -> (==) <$> find arr 0 <*> find arr a) [0..n-1]
  roots <- mapM (\a -> find arr a) [0..n-1]
  return $ (length group0, length $ nub roots)

--extract edges from pipe connections
type Edge = (Int, Int)
edges :: (Int, [Int]) -> [Edge]
edges (a, xs) = map ((,) a) xs
  
-- apply union find algorithm for each edge
unionFind :: [Edge] -> STUArray s Int Int -> ST s (STUArray s Int Int)
unionFind [] arr = return arr
unionFind ((a,b):es) arr = do
  ra <- find arr a
  rb <- find arr b
  if ra == rb 
    then unionFind es arr
    else do
      writeArray arr ra rb
      unionFind es arr
    
-- find root of union find set
find :: STUArray s Int Int -> Int -> ST s Int
find arr a = do
  pa <- readArray arr a
  if pa == a 
    then return a
    else find arr pa
    