-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
import Data.Set (Set)
import qualified Data.Set as Set

data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Ord,Eq, Show)

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


createRectangles :: (Int, Int) -> [Rectangle]
createRectangles (0,_) = []
createRectangles (_,0) = []
createRectangles (a,b) = Rectangle (-a,-b) (a,b): ( merge (createRectangles (a-1,b)) (createRectangles (a,b-1)) )

finalRectangleList :: [Rectangle] -> [Rectangle]
finalRectangleList = Set.toList . Set.fromList

isInEclipse :: Float -> Float -> Float -> Float -> Rectangle -> Bool
isInEclipse x y a b (Rectangle (mx,nx) (px,qx)) | (m-x)^2 `div` a^2 + (n-x)^2 `div` b^2 > 1 = False
                                                | (p-x)^2 `div` a^2 + (q-x)^2 `div` b^2 > 1 = False
                                                | otherwise = True
 where
  m = fromIntegral mx
  n = fromIntegral nx
  p = fromIntegral px
  q = fromIntegral qx
--isInEclipse :: Rectangle -> Bool
-- TODO : finish this method than using filter to reduce the number of rectangles then use the result of ex10 to finish this

-- drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
-- drawEllipse x y a b = filter (isInEclipse x y a b) list
--  where
--   list = finalRectangleList $ createRectangles (a,b)
