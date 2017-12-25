import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.ST
import Data.Array.ST
import Data.Array

-- types for the TM
type Tape s = STArray s Int Bool
data Dir = L | R
type TM = Map (Char, Bool) (Char, Bool, Dir)

-- the TM given as input
theTM :: TM
theTM = Map.fromList [(('A',False),('B',True,R)),(('A',True),('C',False,L)),
                      (('B',False),('A',True,L)),(('B',True),('D',True,L)),
                      (('C',False),('D',True,R)),(('C',True),('C',False,R)),
                      (('D',False),('B',False,L)),(('D',True),('E',False,R)),
                      (('E',False),('C',True,R)),(('E',True),('F',True,L)),
                      (('F',False),('E',True,L)),(('F',True),('A',True,R))]
                      
initialTape :: ST s (Tape s)
initialTape = newListArray (-10000, 10000) [False | i <- [-10000..10000]]

-- read input and print solutions
main :: IO ()
main = do
  let bools = runST $ do
       iT <- initialTape
       finalTape <- runTM 12656374 'A' 0 iT
       freeze finalTape 
  print $ checksum (elems bools)
  
runTM :: Int -> Char -> Int -> (Tape s) -> ST s (Tape s)
runTM 0 _ _ currentTape = return currentTape
runTM n currentState currentPos currentTape = do
  currentSymbol <- readArray currentTape currentPos
  let Just (nextState, nextSymbol, dir) = Map.lookup (currentState, currentSymbol) theTM
  writeArray currentTape currentPos nextSymbol
  let nextPos = 
       case dir of
         L -> pred currentPos
         R -> succ currentPos
  runTM (pred n) nextState nextPos currentTape

checksum :: [Bool] -> Int
checksum = length . filter id 

