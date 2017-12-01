import Data.Char

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day1.txt"
  xs <- map digitToInt <$> init <$> readFile file
  print $ process1 (last xs : xs)
  print $ process2 (shuffle $ splitAt ((length xs) `div` 2) xs)
  
-- calculate sum of direct matches
process1 :: [Int] -> Int
process1 [_] = 0
process1 (x:y:xs) | x == y    = x + process1 (y:xs)
                  | otherwise = process1 (y:xs)
       
-- shuffle two lists together           
shuffle :: ([Int], [Int]) -> [Int]
shuffle ([], []) = []
shuffle ((x:xs), (y:ys)) = x:y:(shuffle (xs, ys))
        
-- calculate twice the sum of pairs          
process2 :: [Int] -> Int
process2 [] = 0
process2 (x:y:xs) | x == y    = 2 * x + process2 xs
                  | otherwise = process2 xs