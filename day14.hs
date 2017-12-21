import Data.Bits
import Data.Word
import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Debug.Trace

import Utility.Knothash
import Utility.UnionFind

-- print solutions
main :: IO ()
main = do
  let solution = solveDay14
  print solution
  
-- solve day 14 puzzles
solveDay14 :: (Int, Int)
solveDay14 = (bitsSet, regionCount) where
  inputs = map (\i -> "nbysizxe" ++ ('-':show i)) [0..127]
  hashes = map knotHash inputs
  bitsSet = sum $ map countBits $ hashes
  regionCount = runST $ do
    -- create grid of indices
    let gridIx = [(i,j) | i <- [0..127], j <- [0..127]]
    grid <- newListArray ((0,0),(127,127)) gridIx :: ST s (STArray s (Int, Int) (Int, Int))
    -- create array of set bits
    let setBits = listArray ((0,0),(127,127)) (concatMap toBits hashes)
    -- find edges of connected regions
    let edges = [((i,j),(i+1,j)) | i <- [0..126], j <- [0..127], setBits ! (i,j) && setBits ! (i+1,j)] 
             ++ [((i,j),(i,j+1)) | i <- [0..127], j <- [0..126], setBits ! (i,j) && setBits ! (i,j+1)]
    -- apply union find
    unionFind edges grid
    -- get total regions including isolated ones
    totalRegions <- regions grid
    -- subtract isolated nodes (all - the set ones)
    return $ totalRegions - (128 * 128 - bitsSet)
  
-- count the bits in one knot hash
countBits :: [Word8] -> Int
countBits = sum . map popCount

-- turn knot hash to bit list
toBits :: [Word8] -> [Bool]
toBits = concatMap (\word8 -> [testBit word8 i | i <- [7,6..0]])
  

  