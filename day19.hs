import Data.Array
import Data.Bifunctor

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day19.txt"
  f <- readFile file
  let ls = lines f
      width = length $ head ls
      height = length ls
      field = listArray ((1,1),(height,width)) (filter (/= '\n') f)
      initialPos = getInitialPos field
  print $ walk field initialPos S
    
-- field, position and direction types
type Field = Array (Int, Int) Char
type Position = (Int, Int)
data Direction = N | S | W | E deriving (Show)

-- utilities for directions and position
turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

advance :: Position -> Direction -> Position
advance (i,j) d = case d of
  N -> (pred i, j)
  E -> (i, succ j)
  S -> (succ i, j)
  W -> (i, pred j)
  
-- utilities for field access
isWalkable :: Field -> Position -> Bool
isWalkable field pos = (field ! pos) /= ' '

isLetter :: Field -> Position -> Bool
isLetter field pos = not $ elem (field ! pos) ['+','|','-']

-- find the starting position    
getInitialPos :: Field -> Position
getInitialPos field = 
  let withIndex = zip [1..] (elems field) 
      x = fst . head . filter (\(_,c) -> c /= ' ') $ withIndex
  in (1, x)
  
-- get next position and direction on path
getNext :: Field -> Position -> Direction -> (Position, Direction)
getNext field pos dir = 
  if isWalkable field (advance pos dir) 
    then (advance pos dir, dir)
    else if isWalkable field (advance pos (turnLeft dir))
      then (advance pos (turnLeft dir), turnLeft dir)
      else if isWalkable field (advance pos (turnRight dir)) 
        then (advance pos (turnRight dir), turnRight dir)
        else error "invalid grid"

-- walk along the line, collecting letters
walk :: Field -> Position -> Direction -> (String, Int)
walk field pos dir = case (field ! pos) of
  'F' -> ("F",1)
  _ -> let (nextPos, nextDir) = getNext field pos dir 
           rec = second succ $ walk field nextPos nextDir
       in if isLetter field pos
            then first ((field ! pos) :) rec
            else rec