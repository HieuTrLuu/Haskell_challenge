--halve :: [a] -> ([a],[a])
--halve x:xs = [[x],[tail xs]]
{--
third :: [a] -> a
third [xs] = head (tail (tail xs))

-- using pattern matching

third :: [a] -> a
third [_,_,a] = a

-- using indexing

--}

--third :: Num a => [a] -> a
--third [] = []
--third (x:xs) = xs !! 1
-- safetail :: [a] -> [a]

fetch :: Int -> [a] -> a 
fetch 0 [] = error “Empty List” 
fetch n [] = error “Empty List” 
fetch 0 (x:_) = x 
fetch n (_:xs) = fetch (n-1) xs