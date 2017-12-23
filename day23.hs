import Control.Monad.State
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
--package primes
import Data.Numbers.Primes

import Utility.ParseAssembly

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day23.txt"
  is <- map parse <$> lines <$> readFile file
  let iarray = listArray (1, length is) is
  let solution1 = evalState interpretInstructions (IState {pc=1, instructions=iarray, registers=Map.empty, mulCount=0})
  print solution1
  let solution2 = length $ filter (not . isPrime) [109900,109917..126900]
  print solution2
  
-- interpretation monad and state
type Interpret = State IState
data IState = IState {
  pc :: Int,
  instructions :: Array Int Instruction,
  registers :: Map Char Int,
  mulCount :: Int
}

-- interpret instructions, counting number of mul uses
interpretInstructions :: Interpret Int
interpretInstructions = do
  i <- pc <$> get
  (mi, ma) <- bounds <$> instructions <$> get
  if i < mi || i > ma
    then mulCount <$> get
    else do 
      interpretInstruction i
      interpretInstructions
 
-- interpet a single instruction     
interpretInstruction :: Int -> Interpret ()
interpretInstruction i = do
  modify (\s -> s {pc=succ i})
  instruction <- (!i) <$> instructions <$> get
  case instruction of
    Set r v -> do
      j <- getValue v
      updateRegister r (const j)
    Mul r v -> do
      j <- getValue v
      updateRegister r (*j)
      modify (\s -> s {mulCount=succ (mulCount s)})
    Sub r v -> do
      j <- getValue v
      updateRegister r (subtract j)
    JumpNZ v w -> do
      j <- getValue v
      if j == 0 
        then return ()
        else do
          offset <- getValue w
          modify (\s -> s {pc=(i + offset)})
          
-- extract a value
getValue :: Value -> Interpret Int
getValue (Constant i) = return i
getValue (Reference c) = do 
  regs <- registers <$> get
  case Map.lookup c regs of
    Nothing -> return 0
    Just i -> return i
 
-- update a register value   
updateRegister :: Char -> (Int -> Int) -> Interpret ()
updateRegister c f = do
  regs <- registers <$> get
  let updatedValue = case Map.lookup c regs of
                       Nothing -> f 0
                       Just i -> f i
  modify (\s -> s {registers=Map.insert c updatedValue regs})