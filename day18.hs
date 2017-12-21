import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace

import Utility.Day18Parse

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day18.txt"
  is <- map parse <$> lines <$> readFile file
  let iarray = listArray (1, length is) is
  let recovered = evalState interpretInstructions (IState {pc=1, instructions=iarray, registers=Map.empty, lastSound=0})
  print (head recovered)
  
-- interpretation monad and state
type Interpret = State IState
data IState = IState {
  pc :: Int,
  instructions :: Array Int Instruction,
  registers :: Map Char Int,
  lastSound :: Int
}

-- interpret the assembly code and return recovered sounds
interpretInstructions :: Interpret [Int]
interpretInstructions = do
  i <- pc <$> get
  (mi, ma) <- bounds <$> instructions <$> get
  if i < mi || i > ma
    then return []
    else do 
      recovered <- interpretInstruction i
      case recovered of
        Nothing -> interpretInstructions
        Just sound -> (sound :) <$> interpretInstructions

-- interpret a single instruction, maybe recover a sound
interpretInstruction :: Int -> Interpret (Maybe Int)
interpretInstruction i = do
  modify (\s -> s {pc=succ i})
  instruction <- (!i) <$> instructions <$> get
  case instruction of
    Sound v -> do
      j <- getValue v
      modify (\s -> s {lastSound=j})
      return Nothing
    Set r v -> do
      j <- getValue v
      updateRegister r (const j)
      return Nothing
    Add r v -> do
      j <- getValue v
      updateRegister r (+j)
      return Nothing
    Mul r v -> do
      j <- getValue v
      updateRegister r (*j)
      return Nothing
    Mod r v -> do
      j <- getValue v
      updateRegister r (`mod` j)
      return Nothing
    Recover r -> do
      j <- getValue (Reference r)
      if j == 0
        then return Nothing
        else do
          k <- lastSound <$> get
          return $ Just k
    JumpGZ v w -> do
      j <- getValue v
      if j <= 0 
        then return Nothing
        else do
          offset <- getValue w
          modify (\s -> s {pc=(i + offset)})
          return Nothing
      
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