-- Exercise 5
-- search for the local maximum of f nearest x using an
-- approximation margin delta and initial step value s
goldenRatio = (1+ sqrt 5) / 2

hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = helper5 d x x' 0
 where
  diff = (d x) - (d x')

data Acc = Acc (Int, Float)
-- fst Acc = the turn
-- snd Acc = the prev result
--third Acc = the epxilon
helper5 :: (Float -> Float) -> Float -> Float -> Acc-> Float
helper5 d x x' acc | (odd $ fst acc) = helper5 d (x+ diff/(1+ goldenRatio)) x' (fst acc +1, "the result of prev call")
                   | (even $ fst acc) = helper5 d x (x+ diff/(1+ goldenRatio)) ()
 where
  diff = (d x') - (d x)


helper5_2 :: Float -> Float


-- the acc will store the turn and the prev result
