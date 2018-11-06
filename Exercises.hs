-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS

-- This module statement makes public only the specified functions and types
-- please do not change these lines either


module Exercises (splitSort, longestCommonSubList,
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond,
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate,
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence,
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse,
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)
import Data.Ord (comparing)
import Data.List
import Data.List.Split

-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal
elementIndexes :: Eq a => a -> [a] -> [Int]
elementIndexes x xs = [ index | (y, index) <- zip xs [0..], x==y]

orderList :: Ord a => [(a,a)] -> [Ordering]
orderList a = [ compare x y | (x,y) <- a  ]

boolTransform :: [(Ordering,Ordering)] -> [Bool]
boolTransform a = [ x==y | (x,y) <- a]

splitSort :: [Int] -> [[Int]]
splitSort ns = customListSplit (reverse truePlaces) ns
 where tupleNumber = zip ns (tail ns)
       tupleOrd = zip (orderList tupleNumber) (tail (orderList tupleNumber))
       oneDown = elementIndexes False (boolTransform tupleOrd)
       truePlaces = [ x+2 | x <- oneDown ]


customListSplit :: [Int] -> [Int] -> [[Int]]
customListSplit _ [] = [[]]
customListSplit (x:indexes) list = firstElt:(customListSplit indexes secondElt)
 where
  tuple = splitAt x list
  firstElt = fst tuple
  secondElt = snd tuple

-- Exercise 2
-- longest common sub-list of a finite list of finite list
lcs :: Ord a => [a] -> [a] -> [a]
lcs xs ys = snd $ lcs' xs ys

lcs' :: Ord a => [a] -> [a] -> (Int, [a])
lcs' (x:xs) (y:ys)
 | x == y = case lcs' xs ys of
                (len, zs) -> (len + 1, x:zs)
 | otherwise = let r1@(l1, _) = lcs' (x:xs) ys
                   r2@(l2, _) = lcs' xs (y:ys)
               in if l1 >= l2 then r1 else r2
lcs' [] _ = (0, [])
lcs' _ [] = (0, [])

--helper function
helper :: Ord a => [[a]] -> [[a]]
helper xs = [lcs lists1 lists2 | lists1 <- xs, lists2 <- xs, lists1/=lists2]

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)



longestCommonSubList :: Ord a => [[a]] -> [a]
longestCommonSubList [[]] = []
longestCommonSubList input = head (helper2 common input)
 where
  common = reverse (lsort (helper input))


helper2 :: Ord a => [[a]] -> [[a]] -> [[a]]
helper2 common input = [ x | x <- common, y <-input, lcs x y == x]


-- Exercise 3
-- check whether the given results are sufficient to pass the year
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show

isEnoughCredit :: [ModuleResult] -> Bool
isEnoughCredit xs = totalcredit >= 60
 where
  totalcredit = sum [mark x | x <- xs ]

--enoughCredit :: [ModuleResult] -> Bool
--enoughCredit xs = foldr (+) 0

isPass :: ModuleResult -> Bool
isPass result | mark result >= 40 = True
              | otherwise = False

isQualify :: ModuleResult -> Bool
isQualify result | mark result >= 25 = True
                 | otherwise = False

yearPass :: [ModuleResult] -> Bool
yearPass [] = True
yearPass (x:xs) = (isPass x) && (yearPass xs)

yearQualify :: [ModuleResult] -> Bool
yearQualify [] = False
yearQualify (x:xs) = (isQualify x) && (yearQualify xs)

isEnoughCompensate :: Bool -> [ModuleResult] -> Bool
isEnoughCompensate False [] = False
isEnoughCompensate False xs = False
isEnoughCompensate True xs | averageMark xs >= 40 = True
                           | otherwise = False

totalMark ::[ModuleResult] -> Int
totalMark [] = 0
totalMark (x:xs) = mark x + totalMark xs

averageMark :: [ModuleResult] -> Int
averageMark [] = 0
averageMark list = (totalMark list ) `div` (length list)

canProgress :: [ModuleResult] -> Bool
canProgress [] = False
canProgress list | (yearPass list) && (isEnoughCredit list)  == True = True
                 | isEnoughCompensate (yearQualify list) list && (isEnoughCredit list) == True = True
                 | yearQualify list == False = False
                 | otherwise = False


-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms = Third

-- Exercise 5
-- search for the local maximum of f nearest x using an
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = 0.0

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = 0.0

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
-- data InstructionTemp = Add | Pop deriving (Eq, Show)

executeOperations :: [Int] -> Instruction -> [Int]
executeOperations [] _ = []
executeOperations (x:xs) Pop = xs
executeOperations (x:xs) Add = (x + head xs):(tail xs)
executeOperations (x:xs) Multiply = (x * head xs):(tail xs)
executeOperations (x:xs) Duplicate = x:x:xs
-- TODO: include case of add and multiply when there is only one int in the int list

executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence [] ins = []
executeInstructionSequence list [] = list
executeInstructionSequence list (x:xs) = executeInstructionSequence (executeOperations list x) xs


-- Exercise 8
log2 :: Int -> Int
log2 n
    | n < 1     = error "agument of logarithm must be positive"
    | otherwise = go 0 1
      where
        go exponent prod
            | prod < n  = go (exponent + 1) (2*prod)
            | otherwise = exponent

non2 :: Int -> Int
non2 n | upperDistance < lowerDistance = upperDistance
       | upperDistance > lowerDistance = lowerDistance
 where upperDistance = 2^(log2 n) - n
       lowerDistance = n - 2^((log2 n) - 1)




optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []




-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers ns = []

isEvenNegative :: [Int] -> Bool
isEvenNegative xs = (length $ filter (<0) xs) `mod` 2 == 0

helper9 :: Int -> Bool -> Instruction
helper9 input False
          | input < 0 = Pop
          | input == 0 = Duplicate --, Add
          | input >0 = Multiply
          -- | otherwise = []
helper9 input True
          | input < 0 = Multiply
          | input == 0 = Duplicate --, Add
          | input >0 = Multiply
          -- | otherwise = []
--TODO: using Duplicate as the temp thing
--TODO: the case when Multiply and Add is similar

transform9 :: [Int] -> [Instruction]
transform9 [] = []
transform9 (x:xs) = (helper9 x boo) : (transform9 xs)
 where
   boo = isEvenNegative (x:xs)

transform91 :: [Instruction] -> [Instruction]
transform91 [] = []
transform91 (x:xs) = (changeTemp1 x) : xs

transform92 :: [Instruction] -> [Instruction]
transform92 [] = []
transform92 (x:xs) = (changeTemp2 x) : xs


changeTemp1 :: Instruction -> Instruction
changeTemp1 x | x == Duplicate = Pop
              | otherwise = x

changeTemp2 :: Instruction -> Instruction
changeTemp2 x | x == Duplicate = Add
              | otherwise = x



-- final9 :: [Instruction] -> [[Instruction]]
-- final9 [] = [[]]
-- final9 xs =
--  where
--   dupList = elementIndexes Duplicate xs
--   grayList = g $ length dupList
--   tupleIns = zip grayList [repeat xs]
mergeIns :: Int -> Instruction -> [Instruction] -> [Instruction]
mergeIns index x ins = merge (fst tuple) (x:(tail $ snd tuple))
 where
  tuple = splitAt index ins



change :: [Int] -> (String, [Instruction]) -> [Instruction]
change [] (io, ins) = ins
change indexs (io, ins) | (head io) == '0' = change (tail indexs) (tail io, mergeIns index Pop ins)
                       | (head io) == '1' = change (tail indexs) (tail io, mergeIns index Add ins)
 where
  tuples = splitAt (head indexs) ins
  str = fst tuples
  ins = snd tuples
  index = head indexs

mapChange :: [Instruction] -> [[Instruction]]
mapChange xs = map (change dupList) tupleIns
 where
  dupList = elementIndexes Duplicate xs
  grayList = g $ length dupList
  tupleIns = zip grayList (repeat xs)


g 0 = [""]
g n = (map ('0':)) (g (n-1)) ++ (map ('1':)) (reverse (g (n-1)))



-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = []

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- createRectangles :: (Int, Int) -> [Rectangle]
-- createRectangles (0,_) = [Rectangle (0,0) (0,0)]
-- createRectangles (_,0) = [Rectangle (0,0) (0,0)]
-- createRectangles (a,b) = Rectangle (-a,-b) (a,b): ( merge (createRectangles (a-1,b)) (createRectangles (a,b-1)) )
--
-- finalRectangleList :: [Rectangle] -> [Rectangle]
-- finalRectangleList = Set.toList . Set.fromList
--
-- isInEclipse :: Float -> Float -> Float -> Float -> Rectangle -> Bool
-- isInEclipse x y a b (Rectangle (mx,nx) (px,qx)) | (m-x)^2 / a^2 + (n-x)^2 / b^2 > 1 = False
--                                                 | (p-x)^2 / a^2 + (q-x)^2 / b^2 > 1 = False
--                                                 | otherwise = True
--  where
--   m = fromIntegral mx
--   n = fromIntegral nx
--   p = fromIntegral px
--   q = fromIntegral qx
--isInEclipse :: Rectangle -> Bool
-- TODO : finish this method than using filter to reduce the number of rectangles then use the result of ex10 to finish this

-- drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
-- drawEllipse x y a b = filter (isInEclipse x y a b) list
--  where
--   ax = floor a
--   bx = floor b
--   list = finalRectangleList $ createRectangles (ax,bx)

drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []


-- Exercise 12
-- extract a message hidden using a simple steganography technique

extractMessage :: String -> String
extractMessage s = outputString
 where
  numList = seperateString (filter isEncoded s)
  outputString = [decoded x | x<- numList]

isEncoded :: Char -> Bool
isEncoded '0' = True
isEncoded '1' = True
isEncoded _ = False

seperateString :: String -> [String]
seperateString s = splitPlaces (generateDuplicate2List (length s)) s

generateDuplicate2List :: Int -> [Int]
generateDuplicate2List x = 2:generateDuplicate2List (x-1)

decoded :: String -> Char
decoded "00" = 'a'
decoded "01" = 'b'
decoded "10" = 'c'
decoded "11" = 'd'

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method
differentStream :: [[Int]] -> [Int]
differentStream ss = []

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f x y
 where
  x = fst $ unPair n
  y = snd $ unPair n

unPair :: Int -> (Int,Int)
unPair z | (z - m^2) < m = (z - m^2, m)
         | otherwise = (m , m^2 + 2*m -z)
 where m = isqrt z

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- Exercise 15
listOfNode :: Int -> [Int]
listOfNode 0 = [0]
listOfNode n = x:(listOfNode x)
 where
  x = fst $ unPair n

isShellTreeSum :: Int -> Bool
isShellTreeSum n = (sum $ tail $ listOfNode n) == snd (unPair n)

-- TODO∷
-- 1. rename and comment
-- 2. change the type def of ex2 to original
-- 3. know why ex11 can not compile and fix the problem of giving incorrect output
-- 5. understand why it can't find the maximum point ?
-- 6. do ex13
-- 7. do ex9
-- 8. check ex6, ex15
