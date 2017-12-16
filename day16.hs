import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Control.Monad
import Data.Bifunctor
import Debug.Trace

-- read input an dprint solutions
main :: IO ()
main = do
  let file = "input/day16.txt"
  commands <- map parse <$> splitStringOn ',' <$> readFile file
  let result1 = solvePart1 commands
  print result1
  let result2 = solvePart2 commands
  print result2
 
-- command data type
data Command = Spin Int |
  Exchange Int Int |
  Partner Char Char
  
-- parse a command 
parse :: String -> Command
parse ('s':i) = Spin (read i)
parse ('x':rest) = Exchange (read i) (read j) where
  [i,j] = splitStringOn '/' rest
parse ['p',i,'/',j] = Partner i j

-- solve part 1 by simly executing commands
solvePart1 :: [Command] -> String
solvePart1 commands = foldl execute ['a'..'p'] commands

-- execute a command
execute :: String -> Command -> String
execute s (Spin i) = rotate s i
execute s (Exchange i j) = swapIndex s i j
execute s (Partner a b) = swap s a b

-- rotate a list
rotate :: [a] -> Int -> [a]
rotate xs i = let n = length xs
                  (hd, tl) = splitAt (n - i) xs in
                    tl ++ hd
                    
-- swap elements at given indices
swapIndex :: (Eq a) => [a] -> Int -> Int -> [a]    
swapIndex xs i j = swap xs (xs !! i) (xs !! j)

-- swap two given elements
swap :: (Eq a) => [a] -> a -> a -> [a]
swap xs a b = map (update a b) xs where
  update a b c = if c == a 
    then b 
    else if c == b 
      then a 
      else c
      
-- split a string on a given character
splitStringOn :: Char -> String -> [String]
splitStringOn c = words . map (\x -> if x == c then ' ' else x)

-- solve part 2 by first simplifying commands
solvePart2 :: [Command] -> String
solvePart2 cmds = 
  -- remove shifts
  let (fewer,shift) = simplifyShift cmds in runST $ do
    -- build permutations for partner and exchange operations independently
    indexMap <- newListArray (0,15) [0..15] :: ST s (STUArray s Int Int)
    charMap <- newListArray ('a','p') ['a'..'p'] :: ST s (STUArray s Char Char)
    buildIndexMap indexMap (filter isExchange $ reverse fewer)
    buildCharMap charMap (filter (not . isExchange) $ reverse fewer)
    -- apply one billion rounds
    accu <- newListArray (0,15) ['a'..'p']
    --it is enough to run 1000 rounds because the computation cycles
    replicateM_ 1000 $ applyRound accu indexMap charMap shift
    res <- getElems accu
    return res 
      where isExchange (Exchange _ _) = True
            isExchange _ = False
  
-- remove all shifts and compute total shift
simplifyShift :: [Command] -> ([Command],Int)
simplifyShift cmds = go 0 cmds where
  go i [] = ([], i)
  go i (cmd:cmds) = case cmd of
    Spin j -> go ((i + j) `mod` 16) cmds
    Exchange j k -> first ((Exchange ((j - i) `mod` 16) ((k - i) `mod` 16)) :) (go i cmds)
    Partner a b -> first (Partner a b :) (go i cmds)
    
-- build exchange permutation
buildIndexMap :: STUArray s Int Int -> [Command] -> ST s ()
buildIndexMap _ [] = return ()
buildIndexMap indexMap ((Exchange i j) : cmds) = do
  vi <- readArray indexMap i
  vj <- readArray indexMap j
  writeArray indexMap i vj
  writeArray indexMap j vi
  buildIndexMap indexMap cmds

-- build char permutation
buildCharMap :: STUArray s Char Char -> [Command] -> ST s ()
buildCharMap _ [] = return ()
buildCharMap charMap ((Partner a b) : cmds) = do
  va <- readArray charMap a
  vb <- readArray charMap b
  writeArray charMap a vb
  writeArray charMap b va
  buildCharMap charMap cmds

-- apply a dance round
applyRound :: STUArray s Int Char -> STUArray s Int Int -> STUArray s Char Char -> Int -> ST s ()
applyRound accu indexMap charMap spin = do
  -- index permutation
  tempAccu <- freeze accu
  forM_ [0..15] $ \i -> do 
    j <- readArray indexMap i
    let vi = tempAccu ! i
    writeArray accu j vi
  -- char permutation
  forM_ [0..15] $ \i -> do 
    a <- readArray accu i
    b <- readArray charMap a
    writeArray accu i b
  -- spin
  tempAccu <- freeze accu
  forM_ [0..15] (\i -> writeArray accu i (tempAccu ! ((i - spin) `mod` 16)))
  