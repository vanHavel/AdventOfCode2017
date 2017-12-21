import Data.Array.ST
import Control.Monad.ST
import Numeric
import Data.Word

import Utility.Knothash

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day10.txt"
  contents <- readFile file
  let lengths1 = map read $ words $ map (\x -> if x == ',' then ' ' else x) $ contents
  let hash1 = runST $ do
      arr <- newListArray (0,255) [0..255] :: ST s (STUArray s Int Word8)
      result <- calculateHash arr 0 0 lengths1
      return result
  print $ product (take 2 hash1)
  -- part 2
  print $ showHash $ knotHash contents