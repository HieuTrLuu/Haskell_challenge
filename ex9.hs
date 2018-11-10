data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
-- Exercise 9
-- findBusyBeavers :: [Int] -> [[Instruction]]
-- findBusyBeavers [] = [[]]
-- findBusyBeavers list = final9 $ transform9 list
 --
 -- where
 --  n = length $ filter (==Duplicate)
 --  num = length $ filter (==Duplicate) $ transform9 list
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



final9 :: [Instruction] -> [[Instruction]]
final9 [] = [[]]
final9 xs = (transform91 xs,transform92 xs)

g 0 = [""]
g n = (map ('0':)) (g (n-1)) ++ (map ('1':)) (reverse (g (n-1)))






-- transform9 :: [[Instruction]] -> [[Instruction]]
-- transform9 list = filter (\xs -> length xs >1) list
