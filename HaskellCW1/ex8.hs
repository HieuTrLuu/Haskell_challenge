-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)

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
    | n < 1     = error "argument of logarithm must be positive"
    | otherwise = go 0 1
      where
        go exponent prod
            | prod < n  = go (exponent + 1) (2*prod)
            | otherwise = exponent

non2 :: Int -> (Int,Int)
non2 n | upperDistance <= lowerDistance = (log2 n , upperDistance )
       | upperDistance > lowerDistance = ((log2 n) - 1, lowerDistance )
 where upperDistance = 2^(log2 n) - n
       lowerDistance = n - 2^((log2 n) - 1)

-- non2 return a tuple of (fst ,snd )
-- where fst is the "closest power of 2 to the variable" and snd is the remainder




optimalSequence :: Int -> [Instruction]
optimalSequence 0 = [Pop]
optimalSequence 1 = []
optimalSequence n = concat [top , mid, bot]
 where
  top = concat $ replicate  (snd $ non2 n) [Duplicate]
  mid = concat $ replicate  (fst $ non2 n) [Duplicate, Multiply]
  bot = concat $ replicate  (snd $ non2 n) [Multiply]
