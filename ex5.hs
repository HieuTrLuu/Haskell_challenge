-- -- Exercise 5
-- -- search for the local maximum of f nearest x using an
-- -- approximation margin delta and initial step value s
-- goldenRatio = (1+ sqrt 5) / 2
-- invphi = (sqrt(5) - 1) / 2 -- 1/phi
-- invphi2 = (3 - sqrt(5)) / 2 -- 1/phi^2

-- gssRecursive :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
-- gssRecursive f x y tol h c d fc fd | h <= tol = a
--                                    | fc < fd = gssRecursive f a d tol (h*invphi) (a + invphi2*h) c (f c) fc
--                                    | otherwise = gssRecursive f c b tol (h*invphi) d (a + invphi*h) fd (f d)
--  where
--   a = (min x y)
--   b = (max x y)
--   h = b-a
--   c = a + h*(1/(1+goldenRatio))
--   d = b - h*(1/(1+goldenRatio))
--   fc = f c
--   fd = f d

--   -- gss :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
--   -- gss f x y tol h c d fc fd | h <= tol = a
--   --                           | fc < fd =   gss f a d tol h*invphi (a + invphi2*h) c (f c) fc
--   --                           | otherwise = gss f c b tol h*invphi d (a + invphi*h) fd (f d)
--   --  where
--   --   a = min (x,y)
--   --   b = max (x,y)
--   --   h = b - a
--   --   c = a + invphi2*h
--   --   d = a + invphi*h
--   --   fc = f c
--   --   fd = f d
--   --



-- hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
-- hillClimb f x y tol = gssRecursive f a b tol h c d fc fd
--  where
--    a = (min x y)
--    b = (max x y)
--    h = b-a -- b is max, a is min
--    c = a + h*(1/(1+goldenRatio))
--    d = b - h*(1/(1+goldenRatio))
--    fc = f c
--    fd = f d

-- -- hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
-- -- hillClimb f x y tol = gssRecursive f a b tol h (a + invphi2*h) (a + invphi*h) (f (a + invphi2*h)) (f (a + invphi*h))
-- --  where
-- --   a = min (x,y)
-- --   b = max (x,y)
-- --   h = b - a
--   -- c = a + invphi2*h
--   -- d = a + invphi*h
--   -- fc = f c
--   -- fd = f d
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

  