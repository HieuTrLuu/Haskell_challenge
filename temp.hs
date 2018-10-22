{--compare1 :: [(a,b)] -> [Ordering]
compare1 _ = []
compare1 (x:xs) = [compare a b : (compare1 xs) | (a,b) <- x ] --}

a = [0..10]
b = zip a (tail a)

orderList :: Ord a => [(a,a)] -> [Ordering]
orderList a = [ compare x y | (x,y) <- a  ]

c = orderList

splitOrdering :: [Ordering] -> [[Ordering]]
splitOrdering [] = [[]]
splitOrdering (x:nx) | (x == head nx) = [[x, head nx],nx]
