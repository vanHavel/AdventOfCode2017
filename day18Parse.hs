module Day18Parse where
  
-- a value can either be a constant or a variable reference
data Value = Constant Int | Reference Char
-- data type for instructions
data Instruction = 
  Sound Value |
  Set Char Value |
  Add Char Value |
  Mul Char Value |
  Mod Char Value |
  Recover Char |
  JumpGZ Value Value
  
-- parse an instruction
parse :: String -> Instruction
parse s = let ws = words s in 
  case ws of
    "snd" : [x] -> Sound (parseValue x)
    "set" : [c] : [x] -> Set c (parseValue x)
    "add" : [c] : [x] -> Add c (parseValue x)
    "mul" : [c] : [x] -> Mul c (parseValue x)
    "mod" : [c] : [x] -> Mod c (parseValue x)
    "rcv" : [[c]] -> Recover c
    "jgz" : x : [y] -> JumpGZ (parseValue x) (parseValue y)

-- parse a value    
parseValue :: String -> Value
parseValue s = case (readMaybe s :: Maybe Int) of
  Just i -> Constant i
  Nothing -> Reference (head s)
    
-- safe read
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing