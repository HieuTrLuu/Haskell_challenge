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


--TODO: change back the type of splirSort
splitSort :: [Int] -> [[Int]]
splitSort ns = customListSplit (reverse truePlaces) ns
 where tupleNumber = zip ns (tail ns)
       tupleOrd = zip (orderList tupleNumber) (tail (orderList tupleNumber))
       oneDown = positions False (boolTransform tupleOrd)
       truePlaces = [ x + 2 | x <- oneDown ]

test = [1,2,3,2,1,1,1]
test1 = zip test (tail test)
test2 = orderList test1
test3 = zip test2 (tail test2)
test4 = boolTransform test3
test5 = [ x+2 | x <- (positions False test4)]

customListSplit :: [Int] -> [Int] -> [[Int]]
--customListSplit [] _ = [ _ ]
customListSplit _ [] = [[]]
customListSplit (x:indexes) list = firstElt:(customListSplit indexes secondElt)
 where
  tuple = splitAt x list
  firstElt = fst tuple
  secondElt = snd tuple


-- changeIndexes :: [Int] -> Int -> [Int]
-- changeIndexes indexs length = foldr   

-- TODO: use foldr to write the helper function and splitAt to finish the ex


-- NEED to fix the syntax of where
-- list ns = positions False (helperFunc (orderList ns) : find the position where the element is False
