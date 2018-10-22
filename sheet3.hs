--ex 1
--sum ([x^3 | x <- [0,2..100]])

--ex 2
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

--ex 3
replicate' ::Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate (n-1) a
-- replicate' a b = [b | _ <- [1..a]]

--ex 4 wrong as it returns an empty list

helper x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], (a^2 + b^2 +c^2 )== 10]

--pyths :: Int ->  [(Int,Int,Int)]
--pyths n = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], (a^2 + b^2 +c^2 )== 10]


--ex 5
--perfect :: Int ->  [Int]

--ex 6
helper6 :: [a] -> [(a,Int)]
helper6 a = zip a [0..]

find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (helper6 xs)

--ex7
--scalarproducts :: Num a => [a] -> [a] -> a
--scalarproducts a b | length 

dotp :: Num a => [a] -> [a] -> a 
dotp a b | length a == length b = sum (zipWith (*) a b)
         | otherwise = error "Vector sizes must match"
 
main = print $ dotp [1, 3, -5] [4, -2, -1] -- prints 3

--Or, using the Maybe monad to avoid exceptions and keep things composable:

dotp
  :: Num a
  => [a] -> [a] -> Maybe a
dotp a b
  | length a == length b = Just $ sum (zipWith (*) a b)
  | otherwise = Nothing
 
main :: IO ()
main = mbPrint $ dotp [1, 3, -5] [4, -2, -1] -- prints 3
 
mbPrint
  :: Show a
  => Maybe a -> IO ()
mbPrint (Just x) = print x
mbPrint n = print n




