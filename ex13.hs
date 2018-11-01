-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method
differentStream :: [[Int]] -> [Int]
differentStream ss = customGet ss (length ss)

customGet :: [[Int]] -> Int -> [Int]
customGet ss num = (get (length ss - num) (get (length ss - num) ss)): customGet ss (num-1)

get :: Int -> [Int] -> Int
get _ [] = error "emptyIntList"
get 0 xs = head xs
get n (x:xs) = get (n-1) xs

-- findPos :: Int -> [Int] -> Int
-- findPos list elt = [index | (index, e) <- zip [0..] list, e == elt]

testList = [1,2,3,4,5]
testGet = get 4 testList

testElt = [1]
test = get 0 testElt
