import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor
import Data.List
import Debug.Trace

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day21.txt"
  ls <- lines <$> readFile file
  let rules = Map.fromList $ nub $ concatMap expandRule $ map parseRule $ ls
  let finalGrid = run initialGrid 18 rules
  print $ trueCount finalGrid

-- types for grids and utility functions
type Grid = Array (Int, Int) Bool

-- the initial grid
initialGrid :: Grid
initialGrid = listArray ((1,1),(3,3)) [False, True, False, False, False, True, True, True, True]

-- get size of square grid
getWidth :: Grid -> Int
getWidth grid = let ((miy,_),(may,_)) = bounds grid 
                in (may - miy + 1)
                
-- get rows of grid 
getRows :: Grid -> [[Bool]]
getRows grid =
  let n = getWidth grid
      vals = elems grid
  in splitEvery n vals
  
-- split list every n element
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = (take n xs) : (splitEvery n (drop n xs))
                
-- get number of activated cells
trueCount :: Grid -> Int
trueCount = length . filter (==True) . elems

-- types for patterns and rules
type Pattern = [Bool]
type Rules = Map Pattern Pattern

-- parse a single rule
parseRule :: String -> (Pattern, Pattern)
parseRule s = 
  let [left, _, right] = words s
  in (parsePattern left, parsePattern right)
  where parsePattern = map (== '#') . filter (/= '/') 
  
-- expand a rule to all flips/rotations
expandRule :: (Pattern, Pattern) -> [(Pattern, Pattern)]
expandRule (left, right) = 
  let rotations = iterate rotate left
  in [(rot,right) | rot <- take 4 rotations] ++ [(mirror rot, right) | rot <- take 4 rotations]

-- rotate a pattern by 90 degrees clockwise
rotate :: Pattern -> Pattern
rotate [a,b,c,d] = [c,a,d,b]
rotate [a,b,c,d,e,f,g,h,i] = [g,d,a,h,e,b,i,f,c]

-- mirror a pattern along x axis
mirror :: Pattern -> Pattern
mirror [a,b,c,d] = [b,a,d,c]
mirror [a,b,c,d,e,f,g,h,i] = [c,b,a,f,e,d,i,h,g]

-- run update for n steps
run :: Grid -> Int -> Rules -> Grid
run grid 0 _ = grid
run grid n rules = 
  let width = getWidth grid
  in case (width `mod` 2) of
    0 -> run (update 2 grid rules) (pred n) rules
    _ -> run (update 3 grid rules) (pred n) rules
  where
    update i grid rules = buildGrid $ mapPatterns rules $ extract grid i
    

-- extract nxn-patterns from grid, line by line (pattern wise)
extract :: Grid -> Int -> [[Pattern]]
extract grid n = 
  let rows = getRows grid 
  in case n of
    2 -> extract2 rows
    3 -> extract3 rows
extract2 :: [[Bool]] -> [[Pattern]]
extract2 [] = []
extract2 ([]:[]:rest) = [] : (extract2 rest)
extract2 (r1:r2:rest) = 
  let (current : rec) = extract2 ((drop 2 r1):(drop 2 r2):rest)
  in (((take 2 r1) ++ (take 2 r2)) : current) : rec
extract3 :: [[Bool]] -> [[Pattern]]
extract3 [] = []
extract3 ([]:[]:[]:rest) = [] : (extract3 rest)
extract3 (r1:r2:r3:rest) = 
  let (current : rec) = extract3 ((drop 3 r1):(drop 3 r2):(drop 3 r3):rest)
  in (((take 3 r1) ++ (take 3 r2) ++ (take 3 r3)) : current) : rec

-- rebuild grid from nxn-patterns, given line by line (pattern wise)
buildGrid :: [[Pattern]] -> Grid
buildGrid patterns = 
  let n = if length (head $ head patterns) == 9 then 3 else 4
      size = (length (head patterns)) * n
  in listArray ((1,1),(size,size)) (concatMap (buildRows n) patterns)

-- build rows from a patter line
buildRows :: Int -> [Pattern] -> [Bool]
buildRows 3 patterns = (concatMap (take 3) patterns) ++ (concatMap (take 3 . drop 3) patterns) ++ (concatMap (drop 6) patterns)
buildRows 4 patterns = (concatMap (take 4) patterns) ++ (concatMap (take 4 . drop 4) patterns) ++ 
                       (concatMap (take 4 . drop 8) patterns) ++ (concatMap (drop 12) patterns)

-- map all patterns according to rules
mapPatterns :: Rules -> [[Pattern]] -> [[Pattern]]
mapPatterns rules = map (map (applyRule rules))

-- apply a matching rule to a pattern
applyRule :: Rules -> Pattern -> Pattern
applyRule rules pattern = case Map.lookup pattern rules of
  Nothing -> error "invalid pattern"
  Just newPattern -> newPattern