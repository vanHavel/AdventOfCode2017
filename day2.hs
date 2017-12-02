-- parse input and read solution
main :: IO ()
main = do
  let file = "input/day2.txt"
  ls <- lines <$> readFile file
  let rows = map ((map read) . words) ls
  print $ solve1 rows
  print $ solve2 rows
  
-- solve puzzle 1
solve1 :: [[Int]] -> Int
solve1 [] = 0
solve1 (l:ls) = maximum l - minimum l + solve1 ls

-- solve puzzle 2
solve2 :: [[Int]] -> Int
solve2 [] = 0
solve2 (l:ls) = a `div` b + solve2 ls where
  [(a,b)] = [(a,b) | a <- l, b <- l, a /= b, a `mod` b == 0]