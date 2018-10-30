-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f 0 0

unPair :: Int -> (Int,Int)
unPair z | (z - m^2) < m = (z - m^2, m)
         | otherwise = (m , m^2 + 2*m -z)
 where m = fromIntegral $ sqrt z
