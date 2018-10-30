-- Exercise 5
-- search for the local maximum of f nearest x using an
-- approximation margin delta and initial step value s
goldenRatio = (1+ sqrt 5) / 2
invphi = (sqrt(5) - 1) / 2 -- 1/phi
invphi2 = (3 - sqrt(5)) / 2 -- 1/phi^2
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
gssRecursive f x y tol h c d fc fd | h <= tol = a
                                   | fc <= fd = gssRecursive f a d tol (h*invphi) (a + invphi2*h) c (f c) fc
                                   | otherwise = gssRecursive f c b tol (h*invphi) d (a + invphi*h) fd (f d)
 where
  a = (min x y)
  b = (max x y)
  h = b-a -- b is max, a is min
  c = a + invphi2*h
  d = a + invphi*h
  fc = f c
  fd = f d

--(f,a,b,tol=1e-5,h=None,c=None,d=None,fc=None,fd=None)



hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb f x y tol | fc < fd = gssRecursive f a d tol (h*(goldenRatio-1)) 0 c 0 fc
                    | otherwise = gssRecursive f c b tol (h*(goldenRatio-1)) d 0 fd 0
 where
   a = (min x y)
   b = (max x y)
   h = b-a -- b is max, a is min
   c = a + invphi2*h
   d = a + invphi*h
   fc = f c
   fd = f d
-- TODO: the incorrect part is in the transfering argument of 2 functions
-- gssRecursive (sin) 0.5 2.0 1e-10 0 0 0 0 0
-- a=0.5
-- b=2
-- h=1.5
tol=1e-5
-- c = (0.5 + invphi2*1.5)
-- d = (0.5 + invphi*1.5)
-- fc = sin c
-- fd = sin d

-- test = gssRecursive (sin) 0.5 2.0 1e-10 1.5 (0.5 + invphi2*1.5) (0.5 + invphi*1.5) (sin (0.5 + invphi2*1.5)) (sin (0.5 + invphi*1.5))

--test = gssRecursive (sin) a b tol h c d fc fd

test = gssRecursive (\x -> 4.0-x*x) (-2.5) 2.5 tol 0 0 0 0 0
-- test2 = hillClimb (sin) a b tol
test3 = hillClimb (\x -> 4.0-x*x) (-2.5) 2.5 1e-3
