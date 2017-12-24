import Data.List 
import Data.Bifunctor

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day24.txt"
  bridges <- map parse <$> lines <$> readFile file
  let sol1 = strongestBridge bridges 0
  print sol1
  let sol2 = strongestLongestBridge bridges 0
  print sol2
  
-- parse an input line
parse :: String -> (Int, Int)
parse s = let [a,b] = map read $ words $ map (\c -> if c == '/' then ' ' else c) s in
  (a,b)
  
-- calculate strength of strongest bridge
strongestBridge :: [(Int, Int)] -> Int -> Int
strongestBridge bridges start = 
  maximum $ [a + b + (strongestBridge (delete (a,b) bridges) b) | (a,b) <- bridges, a == start]
            ++ [a + b + (strongestBridge (delete (a,b) bridges) a) | (a,b) <- bridges, b == start]
            ++ [0]
            
-- calculate strength and length of strongest longest bridge
strongestLongestBridge :: [(Int, Int)] -> Int -> (Int, Int)
strongestLongestBridge bridges start =
  maximum $ [first succ $ second (+ (a + b)) $ strongestLongestBridge (delete (a,b) bridges) b | (a,b) <- bridges, a == start]
            ++ [first succ $ second (+ (a + b)) $ strongestLongestBridge (delete (a,b) bridges) a | (a,b) <- bridges, b == start]
            ++ [(0,0)]