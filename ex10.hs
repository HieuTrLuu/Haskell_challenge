import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Ord,Eq, Show)
data Position = Position (Int, Int) deriving (Eq, Show)

-- isOverlapse :: Rectangle -> Rectangle -> Bool
-- isOverlapse x y =
getX :: Position -> Int
getX (Position(x,y)) = x

getY :: Position -> Int
getY (Position(x,y)) = y


getTopRight :: Rectangle -> (Int, Int)
getTopRight (Rectangle (a,b) (c,d)) = (c,d)

getBottomLeft :: Rectangle -> (Int, Int)
getBottomLeft (Rectangle (a,b) (c,d)) = (a,b)

acceptRectangle :: Rectangle -> Bool
acceptRectangle input = (fst $ getTopRight input) > (fst $ getBottomLeft input) && (snd $ getTopRight input) > (snd $ getBottomLeft input)

isOverlapse :: Rectangle -> Rectangle -> [Rectangle]
-- isOverlapse a b | (getTopRight a == getTopRight b) && (xa2 <= xb2) && (ya2 <= yb2)  = [a]
--                 | (getTopRight a == getTopRight b) && (xa2 >= xb2) && (ya2 >= yb2) = [b]
--                 | (getBottomLeft a == getBottomLeft b) && (xa1 >= xb1) && (ya1 >= yb1) = [a]
--                 | (getBottomLeft a == getBottomLeft b) && (xa1 <= xb1) && (ya1 <= yb1) = [b]
isOverlapse a b | (ya1 >= yb1) && (xa2 <= xb2) = [a]
                | (ya1 <= yb1) && (xa2 >= xb2) = [b]
                | otherwise = [a,b]
-- isOverlapse :: Rectangle -> Rectangle -> Rectangle
-- isOverlapse a b | (ya1 >= yb1) && (xa2 <= xb2) = a
--                 | (ya1 <= yb1) && (xa2 >= xb2) = b
 where
  -- xa2 = fst $ getBottomLeft a
  -- xb2 = fst $ getBottomLeft b
  -- ya1 = snd $ getTopRight a
  -- yb1 = snd $ getTopRight b
  xa1 = fst $ getTopRight a
  xb1 = fst $ getTopRight b
  xa2 = fst $ getBottomLeft a
  xb2 = fst $ getBottomLeft b
  ya1 = snd $ getTopRight a
  yb1 = snd $ getTopRight b
  ya2 = snd $ getBottomLeft a
  yb2 = snd $ getBottomLeft b

a = Rectangle (10, 10) (0,0)
b = Rectangle (20, 10) (10,0)
test = isOverlapse a b

c = Rectangle (20, 20) (0,0)
d = Rectangle (20, 20) (10,10)
test2 = isOverlapse c

e = Rectangle (20, 10) (5,0)
f = Rectangle (10, 10) (0,0)
test3 = isCombined e f

--TODO: check is combined 1st

-- isFit :: Rectangle -> Rectangle -> Rectangle
-- isFit a b =
--  where
--   horizontalA = fst $ getTopRight a - fst $ get getBottomLeft a
--   vertocalA = snd $ getTopRight a - snd $ get getBottomLeft a
--   horizontalB = fst $ getTopRight b - fst $ get getBottomLeft b
--   verticalB = snd $ getTopRight b - snd $ get getBottomLeft b

isCombined :: Rectangle -> Rectangle -> [Rectangle]
isCombined a b
 | (ya1 == yb1) && (ya2 == yb2) && (xa1 > xb1) = [Rectangle (xa1, ya1) (xb2, yb2)]
 | (ya1 == yb1) && (ya2 == yb2) && (xa1 < xb1) = [Rectangle (xb1, ya1) (xa2, yb2)]
 | (xa1 == xa2) && (xa2 == xb2) && (ya1 > yb1) = [Rectangle (xa1, ya1) (xb2, yb2)]
 | (xa1 == xa2) && (xa2 == xb2) && (ya1 < yb1) = [Rectangle (xb1, yb1) (xa2, ya2)]
 | otherwise = [a,b]
  where
    xa1 = fst $ getTopRight a
    xb1 = fst $ getTopRight b
    xa2 = fst $ getBottomLeft a
    xb2 = fst $ getBottomLeft b
    ya1 = snd $ getTopRight a
    yb1 = snd $ getTopRight b
    ya2 = snd $ getBottomLeft a
    yb2 = snd $ getBottomLeft b

testCombined = isCombined a b

getCentre :: Rectangle -> Position
getCentre rectangle = Position (x,y)
 where
  x = ((fst $ getBottomLeft rectangle) + (fst $ getTopRight rectangle)) `div` 2
  y = ((snd $ getBottomLeft rectangle) + (snd $ getTopRight rectangle)) `div` 2

getCentreList :: [Rectangle] -> [Position]
getCentreList (x:xs) = getCentre x : (getCentreList xs)

compareXcentre :: Rectangle -> Rectangle -> Ordering
compareXcentre a b = compare (getX $ getCentre a) (getX $ getCentre b)

compareYcentre :: Rectangle -> Rectangle -> Ordering
compareYcentre a b = compare (getY $ getCentre a) (getY $ getCentre b)

sortedList = sortBy (compareXcentre ) [Rectangle (0,0) (2,2), Rectangle (0,0) (10,10)]

-- simplifyRectangleList :: [Rectangle] -> [Rectangle]
-- simplifyRectangleList rs = [isOverlapse head sortedRectangles (head $ tail sortedRectangles), simplifyRectangleList (tail sortedRectangles)]
--  where
--   sortedRectangles = sortBy (compareXcentre ) rs

checkCombined :: [Rectangle] -> [Rectangle]
checkCombined list = [ head $ isCombined x y| x<-list, y<-list , x /= y, (length $ isCombined x y) == 1]

checkOverlapse :: [Rectangle] -> [Rectangle]
checkOverlapse list = [ head $ isOverlapse x y| x<-list, y<-list, x /= y, (length $ isOverlapse x y) == 1]

list = [Rectangle (0,0) (2,2), Rectangle (0,0) (2,2), Rectangle (0,0) (1,1)]
testList = checkOverlapse list

-- TODO :using Set.toList and Set.fromList , finish this question
-- TODO : write the getCentre methods, then the function to sort the list of rectangles base of x or y cordinate of the list, then write a recursive function to combine rectangles
