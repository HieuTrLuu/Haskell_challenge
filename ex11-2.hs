-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
import Data.Set (Set)
import qualified Data.Set as Set

data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Ord,Eq, Show)

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

createRectangles ::  (Float, Float) -> (Int, Int) -> [Rectangle]
createRectangles (xC,yC) (var1,var2)
 | var1 == floor xC =[]
 | var2 == floor yC = []
 | otherwise = (Rectangle (var1,var2) (op1,op2)): ( merge (createRectangles (xC,yC) (var1 - h,var2)) (createRectangles (xC,yC) (var1,var2 - v)) )
  where
   x = floor xC
   y = floor yC
   h | x >= 0 = 1
     | x< 0 = -1
   v | y>= 0 = 1
     | y<0 = -1
   op1 = x - var1
   op2 = y - var2

finalRectangleList :: [Rectangle] -> [Rectangle]
finalRectangleList = Set.toList . Set.fromList

isInEclipse :: Float -> Float -> Float -> Float -> Rectangle -> Bool
isInEclipse x y a b (Rectangle (mx,nx) (px,qx)) | (m-x)^2 / a^2 + (n-x)^2 / b^2 > 1 = False
                                                | (p-x)^2 / a^2 + (q-x)^2 / b^2 > 1 = False
                                                | otherwise = True
 where
  m = fromIntegral mx
  n = fromIntegral nx
  p = fromIntegral px
  q = fromIntegral qx

-- drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
-- drawEllipse x y a b = filter (isInEclipse x y a b) list
--  where
--   ax = floor a
--   bx = floor b
--   list = finalRectangleList $ createRectangles (ax,bx) (x,y)

-- getPoint :: Float -> Float -> Float -> Float -> (Int,Int)
-- getPoint xC yC a b = (x , y)
 -- where
 --  x = floor (xC + a)
 --  y = floor (yC + b)

test = createRectangles (0,0) (2,2)
-- test = drawEllipse 0.0 0.0 1.0 1.0
