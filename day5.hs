import Data.Array.IO

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day5.txt"
  jumps <- map read <$> lines <$> readFile file
  arr1 <- newListArray (1, length jumps) jumps :: IO (IOUArray Int Int)
  steps1 <- stepCount 1 1 Part1 arr1
  arr2 <- newListArray (1, length jumps) jumps :: IO (IOUArray Int Int)
  steps2 <- stepCount 1 1 Part2 arr2
  print steps1
  print steps2
  
data Part = Part1 | Part2

-- count number of steps until leaving the array
stepCount :: Int -> Int -> Part -> IOUArray Int Int -> IO Int
stepCount count pos part arr = do
  jump <- readArray arr pos
  bounds <- getBounds arr
  if (pos + jump) > snd bounds || (pos + jump) < fst bounds
    then return count
    else do
      case part of
        Part1 -> writeArray arr pos (jump + 1)
        Part2 -> writeArray arr pos (if jump >= 3 then jump - 1 else jump + 1)
      stepCount (count + 1) (pos + jump) part arr
  