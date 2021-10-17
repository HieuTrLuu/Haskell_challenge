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

test = [1,2,3,2,1,1,1,4,5,6,7,8,8,8,8,8,1,2,5,2,3,4,6,7,8]
test1 = zip test (tail test)
test2 = orderList test1
test3 = zip test2 (tail test2)
test4 = boolTransform test3
test5 = [ x+2 | x <- (positions False test4)]

customListSplit :: [Int] -> [Int] -> [[Int]]
customListSplit [] list = [list]
customListSplit indexs list = reverse ((snd tuple):(customListSplit (reverse $ tail reverseIndexs) (fst tuple)))
 where
  reverseIndexs = reverse indexs
  tuple = splitAt (head reverseIndexs) list




  merge :: [a] -> [a] -> [a]
  merge xs     []     = xs
  merge []     ys     = ys
  merge (x:xs) (y:ys) = x : y : merge xs ys
-- changeIndexes :: [Int] -> Int -> [Int]
-- changeIndexes indexs length = foldr   

-- TODO: use foldr to write the helper function and splitAt to finish the ex


-- NEED to fix the syntax of where
-- list ns = positions False (helperFunc (orderList ns) : find the position where the element is False
