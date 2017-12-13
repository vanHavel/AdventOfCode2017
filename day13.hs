-- read input and print solutions
main :: IO ()
main = do
  let file = "input/day13.txt"
  layers <- map parse <$> lines <$> readFile file
  print $ severity 0 layers
  let firstRange = snd . head $ layers
  print $ head [i | i <- [0..], severity i layers == 0, i `mod` (2 * (firstRange - 1)) /= 0]
  
-- parse an input line
parse :: String -> (Int, Int)
parse s = (read $ init $ ws !! 0, read $ ws !! 1) where
  ws = words s

-- calculate severity of trip with given offset  
severity :: Int -> [(Int, Int)] -> Int
severity _ [] = 0
severity offset ((depth, range):xs) = 
  if (offset + depth) `mod` (2 * (range - 1)) == 0
    then rec + depth * range
    else rec
  where rec = severity offset xs
