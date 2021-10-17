
import Data.Set (Set)
import qualified Data.Set as Set

longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [[]] = []
longestCommonSubList xs =  maximumBy (compare `on` length)  (helper xs)
longestCommonSubList xs = elem concat xs
[x | x]

isInList :: [[a]] -> [a]
isInList _ [[]] = []
isInList x xs | x `elem`
where
  setELt = Set.fromList $ concat xs
-- Not work because it might have 2 elemt of the same number
