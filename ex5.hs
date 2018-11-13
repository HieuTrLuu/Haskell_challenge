-- -- Exercise 5
-- -- search for the local maximum of f nearest x using an
-- -- approximation margin delta and initial step value s
-- goldenRatio = (1+ sqrt 5) / 2
-- invphi = (sqrt(5) - 1) / 2 -- 1/phi
-- invphi2 = (3 - sqrt(5)) / 2 -- 1/phi^2



-- -- TODO: the incorrect part is in the transfering argument of 2 functions

-- tol=1e-5
-- test = gssRecursive (\x -> 4.0-x*x) (-2.5) 2.5 tol 0 0 0 0 0
-- test3 = hillClimb (\x -> 4.0-x*x) (-2.5) 2.5 1e-3

goldenRatio = (1 + sqrt 5)/2
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb f x y tol | (b - a) <= tol = a
                    | (f c) >= (f d) = hillClimb f a d tol
                    | (f c) < (f d) = hillClimb f c b tol   
 where 
  a = min x y
  b = max x y
  c = a + (b-a)/(goldenRatio+1)
  d = b - (b-a)/(goldenRatio+1)

  