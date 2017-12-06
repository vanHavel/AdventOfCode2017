import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day6.txt"
  banks <- map read <$> words <$> readFile file
  let initial = Map.singleton banks 0
  let (steps, cycles) = countSteps initial banks 1
  print (steps, cycles)
  
-- keep map of seen results and their occurrence index, then count steps till repetition
countSteps :: Map [Int] Int -> [Int] -> Int -> (Int, Int)
countSteps seen current count = 
  let next = update current in 
    case Map.lookup next seen of
      Just i -> (count, count - i)
      Nothing -> countSteps (Map.insert next count seen) next (count + 1)
      
-- update a memory bank configuration
update :: [Int] -> [Int]
update xs = let m = maximum xs
                n = length xs
                pos = head [i | i <- [0..(n - 1)], xs !! i == m]
                increase = m `div` n
                bigposs = m `mod` n in
                  [(if i == pos then 0 else a) + increase + (if (i - pos - 1) `mod` n < bigposs then 1 else 0) | (a,i) <- zip xs [0..(n-1)]]