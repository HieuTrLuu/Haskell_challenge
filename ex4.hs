-- Exercise 3
-- check whether the given results are sufficient to pass the year
-- and progress using the University of Southampton Calendar regulations

data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show

isEnoughCredit :: [ModuleResult] -> Bool
isEnoughCredit xs = totalcredit >= 60
 where
  totalcredit = sum [mark x | x <- xs ]

--enoughCredit :: [ModuleResult] -> Bool
--enoughCredit xs = foldr (+) 0

isPass :: ModuleResult -> Bool
isPass result | mark result >= 40 = True
              | otherwise = False

isQualify :: ModuleResult -> Bool
isQualify result | mark result >= 25 = True
                 | otherwise = False

yearPass :: [ModuleResult] -> Bool
yearPass [] = True
yearPass (x:xs) = (isPass x) && (yearPass xs)

yearQualify :: [ModuleResult] -> Bool
yearQualify [] = False
yearQualify (x:xs) = (isQualify x) && (yearQualify xs)

isEnoughCompensate :: Bool -> [ModuleResult] -> Bool
isEnoughCompensate False [] = False
isEnoughCompensate False xs = False
isEnoughCompensate True xs | averageMark xs >= 40 = True
                           | otherwise = False

totalMark ::[ModuleResult] -> Int
totalMark [] = 0
totalMark (x:xs) = mark x + totalMark xs

averageMark :: [ModuleResult] -> Int
averageMark [] = 0
averageMark list = (totalMark list ) `div` (length list)

canProgress :: [ModuleResult] -> Bool
canProgress [] = False
canProgress list | (yearPass list) && (isEnoughCredit list)  == True = True
                 | isEnoughCompensate (yearQualify list) list && (isEnoughCredit list) == True = True
                 | yearQualify list == False = False
                 | otherwise = False


test = canProgress [(ModuleResult 20.0 50), (ModuleResult 20.0 50), (ModuleResult 20.0 30)]
test2 = canProgress [(ModuleResult 40.0 50), (ModuleResult 20.0 50)]
threeYear = [(ModuleResult 20.0 50), (ModuleResult 20.0 50), (ModuleResult 20.0 30)]

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms | averageMark >= 70 = First
            | averageMark >= 60 = UpperSecond
            | averageMark >= 50 = LowerSecond
            | averageMark >= 40 = Third
 where averageMark = calculateDegree ms (length ms)

calculateDegree :: [[ModuleResult]] -> Int -> Int
calculateDegree (x:xs) 3 = ((averageMark $ head xs) + (yearThree * 2)) / 3
calculateDegree (x:xs) 4 = ((averageMark $ head xs) + (yearThree * 2) + (yearFour * 2) )/ 5
 where yearThree = averageMark $ head $ tail xs
       yearFour = averageMark $ head $ tail $ tail xs
