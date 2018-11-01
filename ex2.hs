--import Data.List (maximumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List
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

-- Exercise 2
-- longest common sub-list of a finite list of finite list

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

test = [[1,2,3,4], [0,1,3,4], [1,3,4],[1],[1,2,3]]
test2 = maximumBy (compare `on` length)  (helper test)
test3 = reverse (lsort (helper test))

helper2 :: Ord a => [[a]] -> [[a]] -> [[a]]
helper2 common input = [ x | x <- common, y <-input, lcs x y == x]

--TODO: rename and check with the lecturer whether it is ok to change the type of a from Eq a to Ord a
