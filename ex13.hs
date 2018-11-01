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

emptyIntList :: [Int]
emptyIntList = []

emptyIntListList :: [[Int]]
emptyIntListList = []

-- a couple of streams to test
testStream1 :: [[Int]]
testStream1 = [0..]:testStream1
testStream2 :: [[Int]]
testStream2 = repeatStream 0
repeatStream :: Int -> [[Int]]
repeatStream x = [x..]:(repeatStream (x+1))

-- test stream is actually different (looking at first n elements only)
isDifferentStream :: [[Int]] -> [Int] -> Int -> Bool
isDifferentStream nns ns n =
    let ns' = take n ns
        nns' = map (take n) (take n nns)
    in length ns' == 10 &&
       all (\xs -> length xs == 10) nns' &&
       not (ns' `elem` nns')

-- coarse test for approximation floating point equality
approxEqual :: Float -> Float -> Bool
approxEqual x y = abs (x - y) < 1e-2

-- Main program checks the results of the tests and produces scores

-- Auxiliary functions to support testing
message :: [(String,Bool)] -> String
message ts =
  let failures = [(s,b) | (s,b) <- ts , b == False] in
  if failures == [] then "All test cases passed"
  else "Failed " ++ (intToString (length failures)) ++ " out of " ++ (intToString (length ts)) ++ " test cases"

intToString :: Int -> String
intToString 0 = "0"
intToString n =
  if n < 10 then digitToString n
  else (intToString (n `div` 10)) ++ (digitToString (n `mod` 10))

digitToString :: Int -> String
digitToString 0 = "0"
digitToString 1 = "1"
digitToString 2 = "2"
digitToString 3 = "3"
digitToString 4 = "4"
digitToString 5 = "5"
digitToString 6 = "6"
digitToString 7 = "7"
digitToString 8 = "8"
digitToString 9 = "9"
