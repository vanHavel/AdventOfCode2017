import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Control.Monad.State.Strict

import Utility.Orientation

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day22.txt"
  fields <- concatMap (map parse) <$> lines <$> readFile file
  let indices = [(i,j) | i <- [-12..12], j <- [-12..12]]
      assocs = zip indices fields
      initialGrid = Map.fromList assocs
      initialState = VState {grid=initialGrid, infectingBursts=0, position=(0,0), direction=N}
      sol1 = evalState (infect 10000 Part1) initialState
      sol2 = evalState (infect 10000000 Part2) initialState
  print (sol1, sol2)
   
-- type to handle different problem parts 
data Part = Part1 | Part2

-- grid and status types
data Status = Infected | Healthy | Weakened | Flagged
type Grid = HashMap Position Status

-- state for computation
data VState = VState {
  grid :: Grid,
  infectingBursts :: Int,
  position :: Position,
  direction :: Direction
}

-- parse a node
parse :: Char -> Status
parse '#' = Infected
parse '.' = Healthy

-- run the virus
infect :: Integer -> Part -> State VState Int
infect 0 _ = infectingBursts <$> get
infect n part = do
  status <- getStatus
  case status of
    Infected -> turnRightS
    Healthy -> turnLeftS
    Flagged -> reverseS
    Weakened -> return ()
  updateStatus part
  moveForward
  infect (pred n) part
  
-- query node status
getStatus :: State VState Status
getStatus = do
  pos <- position <$> get
  field <- grid <$> get
  case Map.lookup pos field of
    Nothing -> return Healthy
    Just status -> return status

-- update node status
updateStatus :: Part -> State VState ()
updateStatus part = do
  status <- getStatus
  pos <- position <$> get
  case part of 
    Part1 -> case status of 
      Healthy -> modify' (\s -> s{grid=Map.insert pos Infected (grid s), infectingBursts=succ (infectingBursts s)})
      Infected -> modify' (\s -> s{grid=Map.insert pos Healthy (grid s)})
    Part2 -> case status of
      Healthy -> modify' (\s -> s{grid=Map.insert pos Weakened (grid s)})
      Weakened -> modify' (\s -> s{grid=Map.insert pos Infected (grid s), infectingBursts=succ (infectingBursts s)})
      Infected -> modify' (\s -> s{grid=Map.insert pos Flagged (grid s)})
      Flagged -> modify' (\s -> s{grid=Map.insert pos Healthy (grid s)})
    
-- move one step foward
moveForward :: State VState ()
moveForward = modify' (\s -> s{position=advance (position s) (direction s)})

-- turn left in the state monad
turnLeftS :: State VState ()
turnLeftS = modify' (\s -> s{direction=turnLeft (direction s)})

-- turn right in the state monad
turnRightS :: State VState ()
turnRightS = modify' (\s -> s{direction=turnRight (direction s)})

-- reverse direction
reverseS :: State VState ()
reverseS = modify' (\s -> s{direction=reverseDirection (direction s)})