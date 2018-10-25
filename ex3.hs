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
canProgress :: [ModuleResult] -> Bool
canProgress ms = False

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






-- isEnoughCompensate :: [ModuleResult] -> Bool -> Bool
