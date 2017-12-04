import Data.List

-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day4.txt"
  phrases <- lines <$> readFile file
  let solution1 = length . filter isValid1 $ phrases
  let solution2 = length . filter isValid2 $ phrases
  print solution1
  print solution2
 
-- check that passphrase is valid, part 1
isValid1 :: String -> Bool
isValid1 s = nub sorted == sorted where
  sorted = sort . words $ s
  
-- check that passphrase is valid, part 2
isValid2 :: String -> Bool
isValid2 s = nub sorted == sorted where
  sorted = map sort . sort . words $ s