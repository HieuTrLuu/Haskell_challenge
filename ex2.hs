import Data.List (maximumBy)
import Data.Function (on)
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

{--
What is r1 and r2, l1 and l2 in this case ?
r1 and r2 with @ is the result of the function that have first index is l1 or l2

good example, remember this

Hence, to do ex2 using list comprehension and recursion to go compare every elt while saving the largest sub set
--}


a = [1,3,5,4,2,5,4,6,1,2]

b = [6,2,3,4]

c = [1,2,3]


-- Exercise 2
-- longest common sub-list of a finite list of finite list

--helper function
helper :: Ord a => [[a]] -> [[a]]
helper xs = [lcs lists1 lists2 | lists1 <- xs, lists2 <- xs, lists1/=lists2]

-- longestCommonSubList :: Eq a => [[a]] -> [a]
-- longestCommonSubList [[]] = []
-- longestCommonSubList xs =  maximumBy (compare `on` length)  (helper xs)

test = [[1,2,3], [0,1,3], [1,3,4]]

test2 = maximumBy (compare `on` length)  (helper test)
