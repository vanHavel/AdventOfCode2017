module Queue where
  
-- simple queue data structure (FIFO)
data Queue a = Queue ([a],[a])

-- empty queue
emptyQueue :: Queue a
emptyQueue = Queue ([],[])

-- add an element to queue
enqueue :: a -> Queue a -> Queue a
enqueue a (Queue (xs, ys)) = Queue (a:xs, ys)

-- pop first element
dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Queue ([],[])) = (Nothing, q)
dequeue (Queue (xs, ys)) = case ys of
  [] -> dequeue (Queue ([], reverse xs))
  (z:zs) -> (Just z, Queue (xs, zs))