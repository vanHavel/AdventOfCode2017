module Utility.Orientation where
  
-- data types for direction and position
data Direction = N | E | S | W 
  deriving (Show, Eq)
type Position = (Int, Int)

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

reverseDirection :: Direction -> Direction
reverseDirection = turnLeft . turnLeft

advance :: Position -> Direction -> Position
advance (i,j) d = case d of
  N -> (pred i, j)
  E -> (i, succ j)
  S -> (succ i, j)
  W -> (i, pred j)