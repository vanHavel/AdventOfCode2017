module Knothash(calculateHash, knotHash, showHash) where
  
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Debug.Trace
import Data.Char
import Data.Bits
import Numeric
import Data.Word
  
-- calculate one round of the hash function
calculateHash :: (STUArray s Int Word8) -> Int -> Int -> [Int] -> ST s [Word8]
calculateHash arr _ _ [] = do
  list <- getElems arr
  return list
calculateHash arr current skip (l:ls) = do
  let indices = map (`mod` 256) [current..(current + l - 1)]
  toUpdate <- mapM (readArray arr) indices
  writeListFrom current (reverse toUpdate) arr
  list <- getElems arr
  calculateHash arr ((current + l + skip) `mod` 256) (skip + 1) ls
  
-- calculate aknot hash from a string
knotHash :: String -> [Word8]
knotHash s = runST $ do
  let lengths = concat $ replicate 64 $ (map ord s)  ++ [17, 31, 73, 47, 23] 
  arr <- newListArray (0,255) [0..255] :: ST s (STUArray s Int Word8)
  result <- calculateHash arr 0 0 lengths
  return $ reduceHash result

-- write list values from start index into array  
writeListFrom :: Int -> [Word8] -> (STUArray s Int Word8) -> ST s ()
writeListFrom _ [] _ = return ()
writeListFrom current (i:is) arr = do
  writeArray arr current i
  writeListFrom ((current + 1) `mod` 256) is arr
  
-- reduce hash with xor and transform to hex
reduceHash :: [Word8] -> [Word8]
reduceHash [] = []
reduceHash xs = (foldl1 xor $ take 16 xs) : (reduceHash $ drop 16 xs)

-- show hash
showHash :: [Word8] -> String
showHash xs = concatMap (\i -> if i < 16 then '0':(showHex i "") else showHex i "") xs
