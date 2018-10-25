{--
Required methods∷
1. isEnoughCompensate
2. isPass
3. isQualify
4. isFinal

to create an object of ModuleResult
test = ModuleResult 60 15

access attribute
credit test

--}

data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show

test = ModuleResult 15 60

--checkModule :: ModuleResult -> Bool
--checkModule [] = False
-- checkModule x |

isPass :: ModuleResult -> Bool
isPass result | mark result >= 40 = True
              | otherwise = False

isQualify :: ModuleResult -> Bool
isQualify result | mark result >= 25 = True
                 | otherwise = False

isEnoughCompensate :: Bool -> [ModuleResult] -> Bool
isEnoughCompensate False [] = False
isEnoughCompensate True xs | averageMark xs >= 40 = True
                           | otherwise = False

totalMark :: [ModuleResult] -> Int
totalMark [] = 0
totalMark (x:xs) = mark x + totalMark xs

averageMark :: Num a => [ModuleResult] -> a
averageMark [] = 0
averageMark list = (totalMark list ) / (length list)

canProgress :: [ModuleResult] -> Bool
canProgress [] = False
canProgress list | isPass list == True = True
                 | isQualify list == False = False
                 | isEnoughCompensate (isQualify list) list == True = True
                 | otherwise = False







-- OO helps to reuse type while functional programming even help to reuse behaviour








-- isEnoughCompensate :: [ModuleResult] -> Bool -> Bool
