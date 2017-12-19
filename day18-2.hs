import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor
import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace

import Queue
import Day18Parse

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day18.txt"
  is <- map parse <$> lines <$> readFile file
  let iarray = listArray (1, length is) is
      p1state = PState {pc=1, registers=Map.singleton 'p' 0, queue=emptyQueue}
      p2state = PState {pc=1, registers=Map.singleton 'p' 1, queue=emptyQueue}
      sent = evalState interpretInstructions (IState {instructions=iarray, states=(p1state, p2state), sendCount=0})
  print sent
  
-- interpretation monad and state
type Interpret = State IState
data PState = PState {
  pc :: Int,
  registers :: Map Char Int,
  queue :: Queue Int}
data IState = IState {
  instructions :: Array Int Instruction,
  states :: (PState, PState),
  sendCount :: Int
}

-- interpret the assembly code and return recovered sounds
interpretInstructions :: Interpret Int
interpretInstructions = do
  v1 <- interpretInstruction True
  v2 <- interpretInstruction False
  case (v1, v2) of 
    (Nothing, Nothing) -> do
      sc <- sendCount <$> get
      return sc
    _ -> interpretInstructions

-- interpret a single instruction for a program, return nothing if blocked or terminated
interpretInstruction :: Bool -> Interpret (Maybe ())
interpretInstruction p = do
  let selector = if p then fst else snd
  i <- pc <$> selector <$> states <$> get
  (mi, ma) <- bounds <$> instructions <$> get
  if i < mi || i > ma
    then return Nothing
    else do
      updatePc (succ i) p
      instruction <- (!i) <$> instructions <$> get
      case instruction of
        Sound v -> do
          j <- getValue v p
          writeQueue j (not p)
          if (not p) 
            then modify (\s -> s {sendCount=succ (sendCount s)})
            else return ()
          return $ Just ()
        Set r v -> do
          j <- getValue v p
          updateRegister r (const j) p
          return $ Just ()
        Add r v -> do
          j <- getValue v p
          updateRegister r (+j) p
          return $ Just ()
        Mul r v -> do
          j <- getValue v p
          updateRegister r (*j) p
          return $ Just ()
        Mod r v -> do
          j <- getValue v p
          updateRegister r (`mod` j) p
          return $ Just ()
        Recover r -> do
          mj <- readQueue p
          case mj of
            Nothing -> do
              updatePc i p
              return Nothing
            Just j -> do 
              updateRegister r (const j) p
              return $ Just ()
        JumpGZ v w -> do
          j <- getValue v p
          if j <= 0 
            then return $ Just ()
            else do
              offset <- getValue w p
              updatePc (i + offset) p
              return $ Just ()

-- extract a value
getValue :: Value -> Bool -> Interpret Int
getValue (Constant i) _ = return i
getValue (Reference c) p = do
  let selector = if p then fst else snd
  regs <- registers <$> selector <$> states <$> get
  case Map.lookup c regs of
    Nothing -> return 0
    Just i -> return i
 
-- update a register value   
updateRegister :: Char -> (Int -> Int) -> Bool -> Interpret ()
updateRegister c f p = do
  let selector = if p then fst else snd
  sts <- states <$> get
  let regs = registers $ selector sts
      updatedValue = case Map.lookup c regs of
                       Nothing -> f 0
                       Just i -> f i
      updater = \s -> s {registers=Map.insert c updatedValue regs}
      updatedStates = if p
       then first updater sts
       else second updater sts
  modify (\s -> s {states=updatedStates})

-- read from message queue
readQueue :: Bool -> Interpret (Maybe Int)
readQueue p = do
  let selector = if p then fst else snd
  sts <- states <$> get
  let q = queue $ selector sts
      (mv, newQueue) = dequeue q
      updater = \s -> s {queue=newQueue}
      updatedStates = if p
       then first updater sts
       else second updater sts
  modify (\s -> s {states=updatedStates})
  return mv

-- write to message queue
writeQueue :: Int -> Bool -> Interpret ()
writeQueue i p = do
  let selector = if p then fst else snd
  sts <- states <$> get
  let q = queue $ selector sts
      newQueue = enqueue i q
      updater = \s -> s {queue=newQueue}
      updatedStates = if p
       then first updater sts
       else second updater sts
  modify (\s -> s {states=updatedStates})

-- update program counter
updatePc :: Int -> Bool -> Interpret ()
updatePc i p = do
  sts <- states <$> get
  let updater = \s -> s {pc=i}
      updatedStates = if p
       then first updater sts
       else second updater sts
  modify (\s -> s {states=updatedStates})