fetch :: Int -> [a] -> a
fetch 0 [] = error "Empty list"
fetch n [] = error "Empty list"
fetch 0 (x:_) = x
fetch n (_:xs) = fetch (n-1) xs

--exercise5
split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList


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

--using pattern matching
{--
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = [last xs]
--}

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

--using guard equation

safetail :: [a] -> [a]
safetail (x:xs) 
    | listLength (x:xs) == 0 = []
    | listLength (x:xs) >= 1 = [last xs]

--exercise8

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

--exercise9

--enc :: Int -> String -> String


--exercise10


