import Data.Bifunctor

-- read input and print solution
main :: IO ()
main = do
  let file = "input/day9.txt"
  stream <- readFile file
  let (simplified, garbageCount) = process stream
  print $ score simplified
  print garbageCount
  
-- simplify string to nested parantheses
process :: String -> (String, Int)
process = dropGarbage . removeEscapes

-- remove ignored characters and their escapes
removeEscapes :: String -> String
removeEscapes [] = []
removeEscapes ('!':c:s) = (removeEscapes s)
removeEscapes (c:s) = c : (removeEscapes s)

-- drop garbage from string, counting the removed characters
dropGarbage :: String -> (String, Int)
dropGarbage s = go False 0 s where
  go _ i [] = ([], i)
  go False i ('<':s) = go True i s
  go False i (c:s) = first (c:) (go False i s)
  go True i ('>':s) = go False i s
  go True i (_:s) = go True (i+1) s
  
-- calculate score from parantheses string
score :: String -> Int
score s = score' 1 s where
  score' _ [] = 0
  score' i (',':s) = score' i s
  score' i ('}':s) = score' (i-1) s
  score' i ('{':s) = i + score' (i+1) s