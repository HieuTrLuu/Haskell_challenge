hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = 0.0

nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb (list2Polinomial xs (length xs)) x x' eps


list2Polinomial :: [Float] -> Int -> Float -> Float
list2Polinomial [] _ _ = 0
list2Polinomial (x:xs) n input = input^( n- length xs) + (list2Polinomial xs n input )

test6 = list2Polinomial [1,1,1] 3 1
