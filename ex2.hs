--import Data.List (maximumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List
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
longestCommonSubList [[]] = []
longestCommonSubList input = head (helper2 common input)
 where
  common = reverse (lsort (helper input))
