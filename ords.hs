ords :: [Char] -> [Int]
ords [] = []
ords (x:xs) = ord : ords xs