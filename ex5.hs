-- Exercise 5
-- search for the local maximum of f nearest x using an
-- approximation margin delta and initial step value s
goldenRatio = (1+ sqrt 5) / 2
invphi = (math.sqrt(5) - 1) / 2 -- 1/phi
invphi2 = (3 - math.sqrt(5)) / 2 -- 1/phi^2
--
--
-- data Acc = Acc (Int, Float)
-- -- fst Acc = the turn
-- -- snd Acc = the prev result
-- --third Acc = the epxilon
-- helper5 :: (Float -> Float) -> Float -> Float -> Acc-> Float
-- helper5 d x x' acc | (odd $ fst acc) = helper5 d (x+ diff/(1+ goldenRatio)) x' (fst acc +1, "the result of prev call")
--                    | (even $ fst acc) = helper5 d x (x+ diff/(1+ goldenRatio)) ()
--  where
--   diff = (d x') - (d x)


gssRecursive :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
gssRecursive f a b tol h c d fc fd | h <= tol = h
                                   | fc < fd = gssRecursive f a d tol (h*(goldenRatio-1)) 0 c 0 fc
                                   | otherwise = gssRecursive f c b tol (h*(goldenRatio-1)) d 0 fd 0
 where
  h = (max a b) - (min a b) -- b is max, a is min
  c = a + (goldenRatio -1)*h
  d = a + (goldenRatio +1)*h
  fc = f c
  fd = f d



hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb f a b tol | fc < fd = gssRecursive f a d tol (h*(goldenRatio-1)) 0 c 0 fc
                    | otherwise = gssRecursive f c b tol (h*(goldenRatio-1)) d 0 fd 0
 where
   h = (max a b) - (min a b) -- b is max, a is min
   c = a + (goldenRatio -1)*h
   d = a + (goldenRatio +1)*h
   fc = f c
   fd = f d
-- TODO: the incorrect part is in the transfering argument of 2 functions
-- gssRecursive (sin) 0.5 2.0 1e-10 0 0 0 0 0
