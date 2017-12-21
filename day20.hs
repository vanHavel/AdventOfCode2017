import Data.List 
import Data.Function

-- read input and print solutions
main :: IO ()
main = do
  -- for part 1 the solution is obviously the particle with 0 acceleration and least total velocity, 
  -- which can simply be searched for in the file
  print 157
  -- part2
  let file = "input/day20.txt"
  ps <- map parse <$> lines <$> readFile file
  let finalPs = (applyN update 1000) ps
  print $ length finalPs
  
-- triple type and selector
type Triple a = (a,a,a)
sel1 :: Triple a -> a
sel1 (a,b,c) = a
type Particle = Triple (Triple Int)

-- parse a line
parse :: String -> Particle
parse s = 
  let [i1,i2,i3,i4,i5,i6,i7,i8,i9] = map read $ words $ map (\c -> if c == ',' then ' ' else c) $ filter (\c -> not (elem c "pva=<>")) $ s
  in ((i1,i2,i3),(i4,i5,i6),(i7,i8,i9))
  
-- update the particles
update :: [Particle] -> [Particle]
update = dropEqual . sortBy (compare `on` sel1) . map move where
  move ((px,py,pz),(vx,vy,vz),(ax,ay,az)) = ((px+(vx+ax),py+(vy+ay),pz+(vz+az)),(vx+ax,vy+ay,vz+az),(ax,ay,az))
  
-- apply function multiple times
applyN :: (a -> a) -> Int -> (a -> a)
applyN f 0 = id
applyN f i = f . (applyN f (pred i))

-- drop particles with equal positions
dropEqual :: [Particle] -> [Particle]
dropEqual [] = []
dropEqual [p] = [p]
dropEqual [p,q] = if sel1 p == sel1 q then [] else [p,q]
dropEqual (p:q:r:xs) = 
  if sel1 p == sel1 q && sel1 q == sel1 r
    then dropEqual (q:r:xs)
    else if sel1 p == sel1 q
      then dropEqual (r:xs)
      else p:(dropEqual (q:r:xs))
  