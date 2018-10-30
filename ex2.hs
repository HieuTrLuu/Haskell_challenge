import Data.List
import Data.Function (on)
data Set a = Empty | Set a (Set a)
  deriving (Read, Show)


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

fromList :: [a] -> Set a
fromList []     = Empty
fromList (x:xs) = Set x (fromList xs)

a = [1,3,5,4,2,5,4,6,1,2]
test = fromList a --WTF tho ?



{--
What is r1 and r2, l1 and l2 in this case ?
r1 and r2 with @ is the result of the function that have first index is l1 or l2

good example, remember this

Hence, to do ex2 using list comprehension and recursion to go compare every elt while saving the largest sub set
--}

-- test = [[1,2,3], [0,1,3], [1,3,4]]
-- test2 = maximumBy (compare `on` length)  (helper test)
--
-- a = [1,3,5,4,2,5,4,6,1,2]
--
-- b = [6,2,3,4]
--
-- c = [1,2,3]


-- Exercise 2
-- longest common sub-list of a finite list of finite list

longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [[]] = []
-- longestCommonSubList xs =
--  where
--   list = sort $ concat xs

getList :: Eq a => [[a]] -> [a]
getList [[]] = []
getList (x:xs) =
 where
  list = sort $ concat (x:xs)
