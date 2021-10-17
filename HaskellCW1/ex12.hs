import Data.List.Split

extractMessage :: String -> String
extractMessage s = outputString
 where
  numList = seperateString (filter isEncoded s)
  outputString = [decoded x | x<- numList]

isEncoded :: Char -> Bool
isEncoded '0' = True
isEncoded '1' = True
isEncoded _ = False

seperateString :: String -> [String]
seperateString s = splitPlaces (generateDuplicate2List (length s)) s

generateDuplicate2List :: Int -> [Int]
generateDuplicate2List x = 2:generateDuplicate2List (x-1)

decoded :: String -> Char
decoded "00" = 'a'
decoded "01" = 'b'
decoded "10" = 'c'
decoded "11" = 'd'


--test2 = seperateString test12
test = extractMessage "HI H0W ARE YOU DO1NG?  I AM D0ING FINE, 0K!  1S 1T TIME TO GO?"
