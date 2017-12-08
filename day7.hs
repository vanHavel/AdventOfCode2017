import Data.Map (Map,(!))
import qualified Data.Map as Map
import Debug.Trace

import Data.List

-- parse input and print solution
main :: IO ()
main = do
  let file = "input/day7.txt"
  progs <- Map.fromList <$> map parse <$> lines <$> readFile file
  let root = findRoot progs
  let sol = fixImbalance progs root
  print root
  print sol

  
-- parse a program 
parse :: String -> (String, (Int, [String]))
parse s = let ws = words s
              name = ws !! 0
              weight = read . tail . init $ ws !! 1
              succs = filter (/= "->") $ map (filter (/= ',')) $ drop 2 ws
          in (name, (weight, succs))
          
type ProgramMap = Map String (Int, [String])
       
-- find the root of the program tree         
findRoot :: ProgramMap -> String
findRoot ps = let leftNames = Map.keys ps
                  rightNames = [s | (w, ss) <- Map.elems ps, s <- ss]
              in head (leftNames \\ rightNames)
    
-- fix imbalance in the tree          
fixImbalance :: ProgramMap -> String -> Int
fixImbalance progs root = findMismatch progs sums (Map.keys progs)
  where sums = sumProgs progs 
  
-- build map of summed up tree values using DP
sumProgs :: ProgramMap -> Map String Int
sumProgs progs = m where    
  m = Map.fromList [(name, go name) | name <- Map.keys progs]
  go name = (weight name) + sum (map (m !) (succs name))
  weight name = fst (progs ! name)
  succs name = snd (progs ! name)
    
-- find the mismatch by checking the successor sums for each node
findMismatch :: ProgramMap -> Map String Int -> [String] -> Int
findMismatch progs sums (name:names) = 
  let succs = snd $ progs ! name
      succSums = map (sums !) succs 
  -- if we have less then 3 successors we can not have a unique error
  in if length succs <= 2 
       then findMismatch progs sums names
       else case nub succSums of
         -- if the successors are all identical we have no error
         [_] -> findMismatch progs sums names
         -- identify the odd one out
         vals -> let (oddOne, oddVal) = head [(s,val) | (s,val) <- zip succs vals, length ((filter (== val)) succSums) == 1]
                     normalVal = head $ filter (/= oddVal) vals
                  -- calculate fixed value
                  in fst (progs ! oddOne) - oddVal + normalVal
                
     
                   