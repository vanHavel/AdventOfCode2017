import Utility.Zipper

-- print solutions
main :: IO ()
main = do
  print $ solvePart1
  print $ solvePart2

-- walk given number of steps, then insert into zipper
walkAndInsert :: Int -> Zipper a -> a -> Zipper a
walkAndInsert 0 zipper y = insert y zipper
walkAndInsert steps zipper y = walkAndInsert (pred steps) (advance zipper) y
 
-- build zipper and extract solution
solvePart1 :: Int 
solvePart1 = 
  let zipper = foldl (walkAndInsert 387) (Zipper [] [0]) [1..2017] in
    current . advance $ zipper
    
-- calculate position of insertion given buffer length and current position
insertPos :: Int -> Int -> Int
insertPos len cur = succ $ (cur + 386) `mod` len

-- calculate list of all insertion positions
insertPositions :: [(Int, Int)]
insertPositions = go [1..50000000] 0 where
  go [] _ = []
  go (i:is) cur = (i, insertPos i cur) : (go is (insertPos i cur))
  
-- get last value inserted after 0
solvePart2 :: Int
solvePart2 = fst . last $ filter (\(i,j) -> j == 1) $ insertPositions