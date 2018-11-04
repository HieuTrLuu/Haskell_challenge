-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


createRectangles :: (Int, Int) -> [Rectangle]
createRectangles (a,b) = Rectangle (-a,-b) (a,b): ( merge (createRectangles (a-1,b)) (createRectangles (a,b-1)) )

isInEclipse :: Rectangle -> Bool
-- TODO : finish this

drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []
