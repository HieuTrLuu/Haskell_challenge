-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]] 
splitSort ns = [[]]

(++) :: String -> String -> String


helperFunc x:nx = compare x head(nx) helperFunc(tail nx)

map compare (x:xs) 
fold etc