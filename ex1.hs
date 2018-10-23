-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal

--HELPER function

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ index | (y, index) <- zip xs [0..], x==y]

-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
orderList :: Ord a => [(a,a)] -> [Ordering]
orderList a = [ compare x y | (x,y) <- a  ]

helperFunc :: [(Ordering,Ordering)] -> [Bool]
helperFunc a = [ (x == y) | (x,y) <- a]


splitSort :: Ord a => [a] -> [[a]] 
splitSort ns = splitPlaces (positions False (list2 list(ns))) ns
    where list ns = positions False (helperFunc (orderList ns),
          list2 ns = [ x+1 | x <- ns]

-- NEED to fix the syntax of where
-- list ns = positions False (helperFunc (orderList ns) : find the position where the element is False