fetch :: Int -> [a] -> a
fetch 0 [] = error "Empty list"
fetch n [] = error "Empty list"
fetch 0 (x:_) = x
fetch n (_:xs) = fetch (n-1) xs

--using head and tail
{--
third :: [a] -> a
third [] = error "Empty list"
third (x:xs) = head (tail xs)
--}

--using pattern matching
{--
third :: [a] -> a
third [] = error "Empty list"
third (a:b:c:d) = c
--}

--using list indexing
{--
third :: [a] -> a
third (x:xs) = xs !! 1
--}