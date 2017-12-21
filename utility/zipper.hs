module Utility.Zipper where
  
-- circular zipper data type for nonempty lists
data Zipper a = Zipper [a] [a]

-- advance the zipper one step
advance :: Zipper a -> Zipper a
advance (Zipper xs [y]) = Zipper [] (reverse (y:xs))
advance (Zipper xs (y:ys)) = Zipper (y:xs) ys

-- insert into zipper
insert :: a -> Zipper a -> Zipper a
insert y (Zipper xs ys) = Zipper xs (y:ys)

-- get current element
current :: Zipper a -> a
current (Zipper _ (y:_)) = y