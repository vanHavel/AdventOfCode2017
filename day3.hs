import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (Left, Right)

-- print solutions
main :: IO ()
main = do
  let pos = 361527
  print $ steps pos
  print $ firstLarger pos
  
-- compute elements on the diagonals and then calculate distance in dist
steps :: Int -> Int
steps pos = 
  let increases = concatMap (replicate 4) [2,4..]
      diagonals = map succ (scanl1 (+) increases) in 
        dist pos diagonals
    
-- calculate distance from given position
dist :: Int -> [Int] -> Int
dist 1 _ = 0
dist pos diags = 
  -- take only diagonals up to pos into account
  let relevantDiagonals = takeWhile (<=pos) diags
      -- special case: the starting points of each border
      startingPoints = map succ (takeEach 4 relevantDiagonals)
      n = length relevantDiagonals in 
          -- on the diagonal: move two steps to next diagonal
        if pos `elem` relevantDiagonals 
          then 2 + dist (pos - 2 * n) diags
          -- on a starting point: move one step left to next border
          else if pos `elem` startingPoints 
            then 1 + dist (pos - 1) diags
            -- otherwise move one step towards the center
            else 1 + dist (pos - (2 * n + 1)) diags

-- take each nth element
takeEach :: Int -> [a] -> [a]
takeEach i xs | length xs < i = []
              | otherwise     = let (y:ys) = drop (i-1) xs in
                                  y:(takeEach i ys)

------------------------------------------
---------------- Part two ----------------
------------------------------------------

-- the four directions and a position type
data Direction = Left | Up | Right | Down deriving (Show)
type Position = (Int, Int)

-- get next direction from previous one and coordinates
nextDirection :: Position -> Direction -> Direction
nextDirection (i,j) previous | i == j && i >= 0 = Right
                             | i == j && i < 0 = Down
                             | i == -j && i >= 0 = Left
                             | i == -j && i < 0 = Right
                             | i == j+1 && i > 0 = Up
                             | otherwise = previous
                             
-- get next Position from previous position and direction
nextPosition :: Position -> Direction -> Position
nextPosition (i,j) dir = case dir of
  Left -> (i-1,j)
  Up -> (i,j-1)
  Right -> (i+1,j)
  Down -> (i,j+1)
  
-- get first larger index by filling the square iteratively
firstLarger :: Int -> Int
firstLarger val = firstLarger' val (Map.singleton (0,0) 1) (0,0) Right

firstLarger' :: Int -> Map Position Int -> Position -> Direction -> Int
firstLarger' val square pos dir = 
  let nextDir = nextDirection pos dir
      nextPos = nextPosition pos nextDir
      nextVal = sumOfNeighbors nextPos square in
        if nextVal > val
          then nextVal
          else firstLarger' val (Map.insert nextPos nextVal square) nextPos nextDir
          
-- get sum of neighboring values
sumOfNeighbors :: Position -> Map Position Int -> Int
sumOfNeighbors pos square = sum $ map (\neighbor -> Map.findWithDefault 0 neighbor square) (neighbors pos) where
  neighbors (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),(i,j-1),(i,j+1),(i+1,j-1),(i+1,j),(i+1,j+1)]