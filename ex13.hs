-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method

import Data.List
differentStream :: [[Int]] -> [Int]
differentStream ss = [ get (head x - 1) x | x<-ss]

-- customGet :: [[Int]] -> Int -> [Int]
-- customGet ss num = (get (length ss - num) (get (length ss - num) ss)): customGet ss (num-1)
customGet :: [[Int]] -> Int -> [Int]
customGet ss num = []


get :: Int -> [Int] -> Int
get _ [] = error "emptyIntList"
get 0 xs = head xs
get n (x:xs) = get (n-1) xs

-- findPos :: [Int] -> [[Int]] -> Int
-- findPos list [] = error "emptyIntList"
-- findPos [] ss = error "emptyIntList"
-- findPos list ss = [ num | x<-ss ]
--  where
--   num=0

testList = [1,2,3,4,5]
testGet = get 4 testList

testElt = [1]
test = get 0 testElt


index = elemIndex [1..] [[1..],[2..],[3..]]

index2 = elemIndex 1 [1,2,3]
