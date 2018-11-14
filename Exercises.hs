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
import Control.Monad

-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal
elementIndexes :: Eq a => a -> [a] -> [Int]
elementIndexes x [] = []
elementIndexes x xs = [ index | (y, index) <- zip xs [0..], x==y]

orderList :: Ord a => [(a,a)] -> [Ordering]
orderList a = [ compare x y | (x,y) <- a  ]

boolTransform :: [(Ordering,Ordering)] -> [Bool]
boolTransform a = [ x==y | (x,y) <- a]

splitSort :: [Int] -> [[Int]]
splitSort [] =[]
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
lcs :: Eq a => [a] -> [a] -> [a]
lcs xs ys = snd $ lcs' xs ys

lcs' :: Eq a => [a] -> [a] -> (Int, [a])
lcs' (x:xs) (y:ys)
 | x == y = case lcs' xs ys of
                (len, zs) -> (len + 1, x:zs)
 | otherwise = let r1@(l1, _) = lcs' (x:xs) ys
                   r2@(l2, _) = lcs' xs (y:ys)
               in if l1 >= l2 then r1 else r2
lcs' [] _ = (0, [])
lcs' _ [] = (0, [])

helper :: Eq a => [[a]] -> [[a]]
helper xs = [lcs lists1 lists2 | lists1 <- xs, lists2 <- xs, lists1/=lists2]

helper2 :: Eq a => [[a]] -> [[a]] -> [[a]]
helper2 common input = [ x | x <- common, y <-input, lcs x y == x]

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)


longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList [[]] = []
longestCommonSubList input = head (helper2 common input)
 where
  common = reverse (lsort (helper input))
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
goldenRatio = (1 + sqrt 5)/2
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb f x y tol | (b - a) <= tol = a
                    | (f c) >= (f d) = hillClimb f a d tol
                    | (f c) < (f d) = hillClimb f c b tol
 where
  a = min x y
  b = max x y
  c = a + (b-a)/(goldenRatio+1)
  d = b - (b-a)/(goldenRatio+1)

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb' f x x' eps
 where
  f = (^2) . (list2Polinomial xs (length xs))

list2Polinomial :: [Float] -> Int -> Float -> Float
list2Polinomial [] _ _ = 0
list2Polinomial (x:xs) n input = result
 where
  result = x*input^( n- (length xs)) + (list2Polinomial xs n input )

invphi = (sqrt(5) - 1) / 2 -- 1/phi
invphi2 = (3 - sqrt(5)) / 2 -- 1/phi^2

gssRecursive :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
gssRecursive f x y tol h c d fc fd | h <= tol = a
                                   | fc < fd = gssRecursive f a d tol (h*invphi) (a + invphi2*h) c (f c) fc
                                   | otherwise = gssRecursive f c b tol (h*invphi) d (a + invphi*h) fd (f d)
 where
  a = (min x y)
  b = (max x y)
  h = b-a
  c = a + h*invphi2
  d = a + h*invphi
  fc = f c
  fd = f d
  
hillClimb' :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb' f x y tol = gssRecursive f a b tol h c d fc fd
 where
     a = (min x y)
     b = (max x y)
     h = b-a -- b is max, a is min
     c = a + h*invphi2
     d = a + h*invphi
     fc = f c
     fd = f d
  
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

finalUpUntilN :: [Int] -> [Instruction] -> Int
finalUpUntilN listInt listIns | length buffer == 1 = head buffer
                              | otherwise = error("not correct ex 9")
 where
   buffer = executeInstructionSequence listInt listIns


bruteForce9 :: [Int] -> [([Instruction],Int)]
bruteForce9 inputList = [(x,(finalUpUntilN inputList x)) |x <- possibleIns ]
 where
  possibleIns = replicateM (length inputList - 1) [Add, Pop, Multiply]
  --buffer = (finalUpUntilN inputList x)

getHighestFinal :: [([Instruction],Int)] -> Int
getHighestFinal xs = maximum [snd x | x<-xs ]

compareSnd :: Int -> (a,Int) -> Bool
compareSnd max (x,num) = (max == num)

filter9 :: [([Instruction],Int)] -> [[Instruction]]--[([Instruction],Int)]
filter9 xs = map fst (filter (compareSnd max) xs)
 where
  max = getHighestFinal xs

findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [] = []
findBusyBeavers ns = filter9 (bruteForce9 ns)

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Ord, Show)
--TODO: I have include Ord in the definition of Rectangle, need to remove it
data Position = Position (Int, Int) deriving (Eq, Show)

-- isOverlapse :: Rectangle -> Rectangle -> Bool
-- isOverlapse x y =
getX :: Position -> Int
getX (Position(x,y)) = x

getY :: Position -> Int
getY (Position(x,y)) = y


getTopRight :: Rectangle -> (Int, Int)
getTopRight (Rectangle (a,b) (c,d)) = (c,d)

getBottomLeft :: Rectangle -> (Int, Int)
getBottomLeft (Rectangle (a,b) (c,d)) = (a,b)

acceptRectangle :: Rectangle -> Bool
acceptRectangle input = (fst $ getTopRight input) > (fst $ getBottomLeft input) && (snd $ getTopRight input) > (snd $ getBottomLeft input)

isOverlapse :: Rectangle -> Rectangle -> [Rectangle]
-- isOverlapse a b | (getTopRight a == getTopRight b) && (xa2 <= xb2) && (ya2 <= yb2)  = [a]
--                 | (getTopRight a == getTopRight b) && (xa2 >= xb2) && (ya2 >= yb2) = [b]
--                 | (getBottomLeft a == getBottomLeft b) && (xa1 >= xb1) && (ya1 >= yb1) = [a]
--                 | (getBottomLeft a == getBottomLeft b) && (xa1 <= xb1) && (ya1 <= yb1) = [b]
isOverlapse a b | (ya1 >= yb1) && (xa2 <= xb2) = [a]
                | (ya1 <= yb1) && (xa2 >= xb2) = [b]
                | otherwise = [a,b]
-- isOverlapse :: Rectangle -> Rectangle -> Rectangle
-- isOverlapse a b | (ya1 >= yb1) && (xa2 <= xb2) = a
--                 | (ya1 <= yb1) && (xa2 >= xb2) = b
 where
  -- xa2 = fst $ getBottomLeft a
  -- xb2 = fst $ getBottomLeft b
  -- ya1 = snd $ getTopRight a
  -- yb1 = snd $ getTopRight b
  xa1 = fst $ getTopRight a
  xb1 = fst $ getTopRight b
  xa2 = fst $ getBottomLeft a
  xb2 = fst $ getBottomLeft b
  ya1 = snd $ getTopRight a
  yb1 = snd $ getTopRight b
  ya2 = snd $ getBottomLeft a
  yb2 = snd $ getBottomLeft b

a = Rectangle (10, 10) (0,0)
b = Rectangle (20, 10) (10,0)
test = isOverlapse a b

c = Rectangle (20, 20) (0,0)
d = Rectangle (20, 20) (10,10)
test2 = isOverlapse c

e = Rectangle (20, 10) (5,0)
f = Rectangle (10, 10) (0,0)
test3 = isCombined e f

isCombined :: Rectangle -> Rectangle -> [Rectangle]
isCombined a b
 | (ya1 == yb1) && (ya2 == yb2) && (xa1 > xb1) = [Rectangle (xa1, ya1) (xb2, yb2)]
 | (ya1 == yb1) && (ya2 == yb2) && (xa1 < xb1) = [Rectangle (xb1, ya1) (xa2, yb2)]
 | (xa1 == xa2) && (xa2 == xb2) && (ya1 > yb1) = [Rectangle (xa1, ya1) (xb2, yb2)]
 | (xa1 == xa2) && (xa2 == xb2) && (ya1 < yb1) = [Rectangle (xb1, yb1) (xa2, ya2)]
 | otherwise = [a,b]
  where
    xa1 = fst $ getTopRight a
    xb1 = fst $ getTopRight b
    xa2 = fst $ getBottomLeft a
    xb2 = fst $ getBottomLeft b
    ya1 = snd $ getTopRight a
    yb1 = snd $ getTopRight b
    ya2 = snd $ getBottomLeft a
    yb2 = snd $ getBottomLeft b

-- testCombined = isCombined a b

getCentre :: Rectangle -> Position
getCentre rectangle = Position (x,y)
 where
  x = ((fst $ getBottomLeft rectangle) + (fst $ getTopRight rectangle)) `div` 2
  y = ((snd $ getBottomLeft rectangle) + (snd $ getTopRight rectangle)) `div` 2

getCentreList :: [Rectangle] -> [Position]
getCentreList (x:xs) = getCentre x : (getCentreList xs)

compareXcentre :: Rectangle -> Rectangle -> Ordering
compareXcentre a b = compare (getX $ getCentre a) (getX $ getCentre b)

compareYcentre :: Rectangle -> Rectangle -> Ordering
compareYcentre a b = compare (getY $ getCentre a) (getY $ getCentre b)

sortedList = sortBy (compareXcentre ) [Rectangle (0,0) (2,2), Rectangle (0,0) (10,10)]

checkCombined :: [Rectangle] -> [Rectangle]
checkCombined list = list2Set $ [ head $ isCombined x y| x<-list, y<-list,x/=y ]
--TODO : checkCombined is incorrect !!!

checkOverlapse :: [Rectangle] -> [Rectangle]
checkOverlapse list = list2Set $ [ head $ isOverlapse x y| x<-list, y<-list,x/=y ]

simplifyRectangleList :: [Rectangle] -> [Rectangle]
-- TODO: do i check overlapse first or combine first ?
simplifyRectangleList list = checkOverlapse $ checkCombined list

list2Set :: Ord a => [a] -> [a]
list2Set list = Set.toList $ Set.fromList list

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

createRectangles :: (Int, Int) -> [Rectangle]
--What arguments does it takes ? (a,b)
createRectangles (0,_) = [Rectangle (0,0) (0,0)]
createRectangles (_,0) = [Rectangle (0,0) (0,0)]
createRectangles (a,b) = merge [Rectangle (-a,-b) (a,b), Rectangle (-a,0) (a,0), Rectangle (0,-b) (0,b) ] ( merge (createRectangles (a-1,b)) (createRectangles (a,b-1)) )


isInEclipse :: Float -> Float -> Float -> Float -> Rectangle -> Bool
isInEclipse x y a b (Rectangle (mx,nx) (px,qx)) | (m-x)^2 / a^2 + (n-x)^2 / b^2 > 1 = False
                                                | (p-x)^2 / a^2 + (q-x)^2 / b^2 > 1 = False
                                                | otherwise = True
 where
  m = fromIntegral mx
  n = fromIntegral nx
  p = fromIntegral px
  q = fromIntegral qx
-- TODO: might have to use isCombined and isOverlapse from ex10

drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = filter (isInEclipse x y a b) list
 where
  ax = floor a
  bx = floor b
  list = list2Set $ checkOverlapse $ createRectangles (ax,bx)

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
differentStream ss = [ (recurseHead (snd x) (fst x) +1 )|x<-buffer]
 where
  buffer = zip [0..] ss

recurseHead :: [Int] -> Int -> Int
recurseHead [] _ = error "empty list"
recurseHead ss 0 = head ss
recurseHead ss n = recurseHead (tail ss) (n-1)

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
-- 5. understand why it can't find the maximum point ?
-- 6. do ex13
-- 8. check ex3, ex4, ex5, ex15
-- ex5 does not load ?
-- some exercises get wrong (1,2) ???