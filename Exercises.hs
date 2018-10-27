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

-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]]
splitSort ns = [[]]

-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList xs = []

-- Exercise 3
-- check whether the given results are sufficient to pass the year
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress ms = False

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
findBusyBeavers [] = [[]]
findBusyBeavers list = final $ transform9 list
 where
  n = length $ filter (==Duplicate)
  num = length $ filter (==Duplicate) $ transform9 list
-- findBusyBeavers x:xs = [helper9 x boo ]
--  where
--   boo = isEvenNegative (x:xs)

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

transform9 :: [Int] -> [Instruction]
transform9 [] = [[]]
transform9 (x:xs) = (helper9 x) : transform9 xs

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



final9 :: [Instruction] -> ([Instruction], [Instruction])
final9 [] = ([],[])
final9 xs = ([transform91 xs],[transform92 xs])





-- transform9 :: [[Instruction]] -> [[Instruction]]
-- transform9 list = filter (\xs -> length xs >1) list



-- TODO: does this mean we can not use duplicate in this ?
-- TODO: we do not pop the int that is <0 because if the number of number <0 is even that it can increases the value of seq

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = []

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []

-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = ""

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method
differentStream :: [[Int]] -> [Int]
differentStream ss = []

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f 0 0

-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n = False
