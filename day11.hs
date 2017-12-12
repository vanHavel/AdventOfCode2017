import Data.Bifunctor

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day11.txt"
  dirs <- map parse <$> words <$> map (\c -> if c == ',' then ' ' else c) <$> readFile file
  print $ dist dirs
  let pres = prefixes dirs
  print $ maximum $ map dist pres

-- data type for hex directions
data Direction = N | S | NE | SE | NW | SW

-- parse a direction  
parse :: String -> Direction
parse "n" = N
parse "ne" = NE
parse "se" = SE
parse "s" = S
parse "nw" = NW
parse "sw" = SW

-- calculate distance to origin
dist :: [Direction] -> Int
dist dirs = let (x,y) = coordinates dirs in
  if abs x > abs y 
    then abs x 
    else (abs x) + ((abs y - abs x) `div` 2)
    
-- calculate coordinates given move sequence from origin
coordinates :: [Direction] -> (Int, Int)
coordinates [] = (0,0)
coordinates (d:ds) = let rec = coordinates ds in 
  case d of
    N -> second (+2) rec
    S -> second (subtract 2) rec
    NE -> first succ $ second succ rec
    SE -> first succ $ second pred rec
    NW -> first pred $ second succ rec
    SW -> first pred $ second pred rec

-- get all prefixes of a list
prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes (x:xs) = [] : (map (x:) (prefixes xs))