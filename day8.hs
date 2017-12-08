import Data.Map (Map,(!))
import qualified Data.Map as Map
import Control.Monad.State
import Debug.Trace

import Data.List

-- parse input and print solution
main :: IO ()
main = do
  let file = "input/day8.txt"
  instructions <- map parse <$> lines <$> readFile file
  let maxValues = map (\is -> evalState (execute is) Map.empty) (prefixes instructions)
  print (last maxValues, maximum maxValues)
  
-- data types for instructions
data Instruction = Instruction {
  register :: String,
  increase :: Bool,
  amount :: Int,
  condition :: Condition
}
data Condition = Condition {
  operand :: String,
  operator :: Operator,
  value :: Int
}
data Operator = L | LE | E | GE | G | NE
type Registers = Map String Int

-- parse an instruction
parse :: String -> Instruction
parse s = let ws = words s 
              reg = ws !! 0
              inc = (ws !! 1) == "inc" 
              am = read $ ws !! 2
              opd = ws !! 4
              op = case ws !! 5 of
                "<" -> L
                "<=" -> LE
                "==" -> E
                ">=" -> GE
                ">" -> G
                "!=" -> NE
              val = read $ ws !! 6
            in Instruction {register=reg, 
                            increase=inc, 
                            amount=am, 
                            condition=Condition {
                              operand=opd,
                              operator=op,
                              value=val
                              }
                            }

-- exceute the instructions
execute :: [Instruction] -> State Registers Int
execute [] = do
  s <- get
  case Map.elems s of 
    [] -> return 0
    vals -> return $ maximum vals
execute (i:is) = do
  b <- check (condition i)
  if b 
    then do
      let increment = if (increase i) then (amount i) else - (amount i)
      update (register i) (increment)
    else return ()
  execute is
  
-- update a regsiter value. Create it if it does not exist
update :: String -> Int -> State Registers ()
update reg val = modify (Map.alter updater reg) where
  updater Nothing = Just val
  updater (Just a) = Just (a + val)

-- check a condition
check :: Condition -> State Registers Bool
check condition = do
  regVal <- Map.findWithDefault 0 (operand condition) <$> get
  let val = value condition
  return $ case (operator condition) of 
    L -> regVal < val
    LE -> regVal <= val
    E -> regVal == val
    GE -> regVal >= val
    G -> regVal > val
    NE -> regVal /= val

-- get all prefixes of a list
prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes (x:xs) = [] : (map (x:) (prefixes xs))