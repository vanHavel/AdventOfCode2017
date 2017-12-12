import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Debug.Trace
import Data.Char
import Data.Bits
import Numeric

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day10.txt"
  contents <- readFile file
  let lengths1 = map read $ words $ map (\x -> if x == ',' then ' ' else x) $ contents
  let hash1 = runST $ do
      arr <- newListArray (0,255) [0..255] :: ST s (STUArray s Int Int)
      result <- calculateHash arr 0 0 lengths1
      return result
  print $ product (take 2 hash1)
  -- part 2
  let lengths2 = concat $ replicate 64 $ (map ord contents) ++ [17, 31, 73, 47, 23] 
  let hash2 = runST $ do
      arr <- newListArray (0,255) [0..255] :: ST s (STUArray s Int Int)
      result <- calculateHash arr 0 0 lengths2
      return result
  let solution = reduceHash hash2
  print solution
  
  
-- calculate the hash function
calculateHash :: (STUArray s Int Int) -> Int -> Int -> [Int] -> ST s [Int]
calculateHash arr _ _ [] = do
  list <- getElems arr
  return list
calculateHash arr current skip (l:ls) = do
  let indices = map (`mod` 256) [current..(current + l - 1)]
  toUpdate <- mapM (readArray arr) indices
  writeListFrom current (reverse toUpdate) arr
  list <- getElems arr
  calculateHash arr ((current + l + skip) `mod` 256) (skip + 1) ls

-- write list values from start index into array  
writeListFrom :: Int -> [Int] -> (STUArray s Int Int) -> ST s ()
writeListFrom _ [] _ = return ()
writeListFrom current (i:is) arr = do
  writeArray arr current i
  writeListFrom ((current + 1) `mod` 256) is arr
  
-- reduce hash with xor and transform to hex
reduceHash :: [Int] -> String
reduceHash = toHex . densify where
  densify [] = []
  densify xs = (foldl1 xor $ take 16 xs) : (densify $ drop 16 xs)
  toHex xs = concatMap (\i -> if i < 16 then '0':(showHex i "") else showHex i "") xs
