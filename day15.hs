
-- print solutions
main :: IO ()
main = do
  print $ matchCount gen1 gen2 40000000
  print $ matchCount (filter (divisibleBy 4) gen1) (filter (divisibleBy 8) gen2) 5000000

-- count matches on last 16 bits in 40 million samples
matchCount :: [Integer] -> [Integer] -> Int -> Int
matchCount l1 l2 samples = length $ filter matching $ take samples $ zip l1 l2 where
  matching (a,b) = (a `mod` 65536) == (b `mod` 65536)

-- the two generators
gen1 :: [Integer]
gen1 = 618 : map (\i -> (i * 16807) `mod` 2147483647) gen1
gen2 :: [Integer]
gen2 = 814 : map (\i -> (i * 48271) `mod` 2147483647) gen2

-- divisibility predicate
divisibleBy :: Integer -> Integer -> Bool
divisibleBy k n = n `mod` k == 0