-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2018
-- DO NOT RE-DISTRIBUTE OR RE-POST
-- sample test cases by Andy Gravell, Julian Rathke

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Challenges


-- Main program, testing constants and functions start here
--
-- Test Suites, one per exercise
tests :: [(String, [(String, Bool)])]
tests =
  [
   ("test cbv",
     [ ("Test 1: call by value expr5",
          (turn2Maybe (evalcbv lambdaExpr5)) `equivLam2` (LamAbs 1 (LamAbs 2 (LamVar 2)))
       )


     ]
   )
  ]

-- Main program checks the results of the tests and produces scores
main :: IO ()
main =
  do
    putStr ""
    testSuite tests

testSuite :: [(String, [(String,Bool)])] -> IO ()
testSuite [] = putStr ""
testSuite ((s,tc):ts) =
  do
    putStrLn (outPrefix (s ++ ": " ++ (message tc)))
    testCases tc
    testSuite ts

testCases :: [(String,Bool)] -> IO ()
testCases [] = putStr ""
testCases ((s,False):ts) =
  do
    putStr (outPrefix "Did not satisfy assertion: ")
    putStrLn s
    testCases ts
testCases ((s,True):ts) =
  do
    testCases ts

-- Auxiliary functions to support testing and scoring
outPrefix msg = "  " ++ msg

message :: [(String,Bool)] -> String
message ts =
  let failures = [(s,b) | (s,b) <- ts , b == False] in
  if failures == [] then "all test cases passed"
  else "failed " ++ (show (length failures)) ++ " out of " ++ (show (length ts)) ++ " test cases"


-- lambda calculus expressions test values
lambdaExpr1 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))
lambdaExpr2 = LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamAbs 3 (LamVar 3)) (LamAbs 4 (LamVar 4)))
lambdaExpr3 = LamApp lambdaExpr2 lambdaExpr1
lambdaExpr4 = LamApp lambdaExpr1 lambdaExpr2
lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))
-- Smullyan's mockingbird(s)
lambdaOmega = LamAbs 1 (LamApp (LamVar 1) (LamVar 1))
lambdaOmega1 = LamApp lambdaOmega lambdaOmega
-- lambda calculus propositional logic constants and functions
lambdaTrue = LamAbs 1 (LamAbs 2 (LamVar 1))
lambdaFalse = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaAnd = LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 2) (LamVar 1)) (LamVar 2)))
lambdaAnd1 = LamApp (LamApp lambdaAnd lambdaFalse) lambdaFalse
lambdaAnd2 = LamApp (LamApp lambdaAnd lambdaTrue) lambdaTrue
-- test cases for the church numerals
lambdaZero = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaSucc = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))
lambdaOne = LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))
lambdaSuccZero = LamApp lambdaSucc lambdaZero

-- test for equivalent lambda expressions
equivLam :: LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for alpha equivalence
equivLam m n = m == n

-- test for equivalent lambda expressions where the first may be Nothing
equivLam2 :: Maybe LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for beta equivalence
equivLam2 Nothing n = False
equivLam2 (Just m) n = m == n

-- test for two let strings being equivalent
equivLetString :: String -> String -> Bool
-- at present just check string equality modulo extra spaces
-- may need to be replaced by some more sophisticated test
equivLetString s t = remSpaces(s) == remSpaces(t)

-- test for two let expressions being equivalent, where the first may be Nothing
-- may need to be replaced by some more sophisticated test
equivLet :: Maybe Expr -> Expr -> Bool
equivLet Nothing e = False
equivLet (Just d) e = d == e

-- removed duplicated spaces
remSpaces :: String -> String
remSpaces "" = ""
remSpaces (' ':' ':s) = remSpaces (' ':s)
remSpaces (c:s) = c:(remSpaces s)

turn2Maybe :: LamExpr -> Maybe LamExpr
turn2Maybe s = (Just s)