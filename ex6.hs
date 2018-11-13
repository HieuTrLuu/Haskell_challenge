goldenRatio = (1 + sqrt 5)/2
invphi = (sqrt(5) - 1) / 2 -- 1/phi
invphi2 = (3 - sqrt(5)) / 2 -- 1/phi^2
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb f x y tol | (b - a) <= tol = a
                    | (f c) >= (f d) = hillClimb f a d tol
                    | (f c) < (f d) = hillClimb f c b tol   
 where 
  a = min x y
  b = max x y
  c = a + (b-a)/(goldenRatio+1)
  d = b - (b-a)/(goldenRatio+1)

nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb' (list2Polinomial xs (length xs)) x x' eps


list2Polinomial :: [Float] -> Int -> Float -> Float
list2Polinomial [] _ _ = 0
list2Polinomial (x:xs) n input = result^2
 where
  result = x*input^( n- (length xs)-1) + (list2Polinomial xs n input )

test6 = list2Polinomial [1,1,1] 3 1

gssRecursive :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
gssRecursive f x y tol h c d fc fd | h <= tol = a
                                   | fc < fd = gssRecursive f a d tol (h*invphi) (a + invphi2*h) c (f c) fc
                                   | otherwise = gssRecursive f c b tol (h*invphi) d (a + invphi*h) fd (f d)
 where
  a = (min x y)
  b = (max x y)
  h = b-a
  c = a + invphi2*h
  d = a + invphi*h
  fc = f c
  fd = f d

hillClimb' :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb' f x y tol = gssRecursive f a b tol h (a + invphi2*h) (a + invphi*h) (f (a + invphi2*h)) (f (a + invphi*h))
 where
  a = min (x,y)
  b = max (x,y)
  h = b - a
  c = a + invphi2*h
  d = a + invphi*h
  fc = f c
  fd = f d

