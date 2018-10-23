
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
boolTransform a = [ x==y | (x,y) <- a]

splitSort :: Ord a => [a] -> [[a]]
splitSort ns = splitPlaces truePlaces ns
 where tupleNumber = zip ns (tail ns)
       tupleOrd = zip (orderList tupleNumber) (tail (orderList tupleNumber))
       oneDown = positions False (boolTransform tupleOrd)
       truePlaces = [ x+2 | x <- oneDown ]

test = [1,2,3,2,1,1,1]
test1 = zip test (tail test)
test2 = orderList test1
test3 = zip test2 (tail test2)
test4 = boolTransform test3
test5 = [ x+2 | x <- (positions False test4)]


splitAtList :: [a] -> [a] -> [[a]]
splitAtList _ [] = [[]]
splitAtList [] _ = [ _ ]
-- splitAtList (x:xs) y = (splitAt x y) : splitAtList xs y

-- splitAt' :: Int -> [a] -> ([a], [a])
-- splitAt' 0 xs     = ([], xs)
-- splitAt' _ []     = ([], [])
-- splitAt' n (x:xs) = (x:xs', xs'')
--   where
--     (xs', xs'') = splitAt (n - 1) xs




-- NEED to fix the syntax of where
-- list ns = positions False (helperFunc (orderList ns) : find the position where the element is False
