-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal

--HELPER function
import Data.List.Split

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ index | (y, index) <- zip xs [0..], x==y]

-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal
orderList :: Ord a => [(a,a)] -> [Ordering]
orderList a = [ compare x y | (x,y) <- a  ]

boolTransform :: [(Ordering,Ordering)] -> [Bool]
boolTransform a = [ (x == y) | (x,y) <- a]

splitSort :: Ord a => [a] -> [[a]]
splitSort ns = splitPlaces truePlaces ns
 where tupleNumber = zip ns (tail ns)
       tupleOrd = zip (orderList tupleNumber) (tail (orderList tupleNumber))
       oneDown = positions False (boolTransform tupleOrd)
       truePlaces = [x+1 | x <- oneDown ]


-- NEED to fix the syntax of where
-- list ns = positions False (helperFunc (orderList ns) : find the position where the element is False
