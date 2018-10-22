{--compare1 :: [(a,b)] -> [Ordering]
compare1 _ = []
compare1 (x:xs) = [compare a b : (compare1 xs) | (a,b) <- x ] --}

import System.Random (randomRIO)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 

a = [1,3,5,4,2,5,4,6,1,2]
b = zip a (tail a)

orderList :: Ord a => [(a,a)] -> [Ordering]
orderList a = [ compare x y | (x,y) <- a  ]

c = orderList b

{--
splitOrdering :: [Ordering] -> [[Ordering]]
splitOrdering [] = [[]]
splitOrdering (x:nx) | (x == head nx) = [[x, head nx],nx]
--}

d = zip c (tail c)

helperFunc :: [(Ordering,Ordering)] -> [Bool]
helperFunc a = [ (x == y) | (x,y) <- a]

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

