-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)



-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
-- replace the definition below with your solution
convertLet (Var int) = (LamAbs int (LamVar int))
convertLet (Let list expr1 expr2)
  | length list == 1 = LamApp (LamAbs (head list) (convert expr2)) (convert expr1)
  | length list == 2 = LamApp (LamAbs (head list) (convert expr2)) (convertLet expr1)
  | otherwise = LamApp (LamAbs (head list) (convert expr2)) (helperLet (convert expr1) (tail list)) --TODO: this is not correct and need to fix although the test return true
--convertLet (Let list expr1 expr2) = LamApp (LamAbs (head list) (convert expr2))(LamAbs (head $ tail list) (convert expr1))


helperLet :: LamExpr -> [Int] -> LamExpr
--helperLet (Var int) list = LamApp (LamAbs (head list) (convert expr2)) (convertLet expr1) --TODO: because if the expression only has 1 var, this is the way to go, can it be var and the list has more than 1 elt ?
helperLet (LamApp expr1 expr2) [] = (LamApp expr1 expr2)
helperLet (LamApp expr1 expr2) list = (LamAbs (head list) (helperLet (LamApp expr1 expr2) (tail list)))



convertVar :: Expr -> LamExpr
convertVar (Var int) = LamVar int
convertVar expr = convert expr

convertApp :: Expr -> LamExpr
convertApp (App (Var int1) (Var int2)) = LamApp (convert (Var int1)) (convert (Var int2))
convertApp expr =  convert expr


convert :: Expr -> LamExpr
convert expr | getType expr == "Var" = convertVar expr
             | getType expr == "Let" = convertLet expr
             | getType expr == "App" = convertApp expr

-- "Test 3: convertLet(let x1 x2 x3 = x3 x2 in x1 x4) equivLam LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))))",
-- convertLet (Let [1,2,3] (App (Var 3) (Var 2)) (App (Var 1) (Var 4))) `equivLam` LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))))
                                    

getType :: Expr ->  String
getType (Var _) = "Var"
getType (Let i1 i2 i3) = "Let"
getType (App i1 i2) = "App"



-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
-- replace the definition below with your solution
prettyPrint e = convert2String e

list2String :: [Int] -> String
list2String [] = ""
list2String (x:xs) = "x" ++ (show x) ++ " " ++ list2String xs

checkBracket :: Expr -> Expr -> Bool
checkBracket expr1 expr2 | getType expr1 == "Let" =  True
                         | getType expr2 == "App" =  True
                         | otherwise = False

convert2String :: Expr -> String
convert2String (Var int) = "x" ++ (show int)
convert2String (Let list expr1 expr2) = "let " ++ (list2String list) ++ " = " ++ convert2String(expr1) ++ " in " ++ convert2String(expr2)
convert2String (App expr1 expr2)
  | getType expr1 == "Let" = "(" ++ (convert2String expr1) ++ ") " ++ (convert2String expr2)
  | getType expr2 == "App" = (convert2String expr1) ++ " (" ++ (convert2String expr2) ++")"
  | otherwise = (convert2String expr1) ++ " " ++ (convert2String expr2)

-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
-- replace the definition below with your solution
parseLet s = Nothing

-- Challenge 4
-- count reductions using two different strategies 
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution
countReds e limit = (Nothing, Nothing)

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith s = Nothing

qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])