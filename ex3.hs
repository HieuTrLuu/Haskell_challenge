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
isQualify result | mark result <= 25 = True
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



-- OO helps to reuse type while functional programming even help to reuse behaviour








-- isEnoughCompensate :: [ModuleResult] -> Bool -> Bool
