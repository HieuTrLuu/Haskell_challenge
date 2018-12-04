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
import Eval

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)



-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
-- replace the definition below with your solution
convertLet (Let [1] expr1 expr2) = LamApp (LamAbs 1 (convert expr2)) (convert expr1)

convertLet (Let list expr1 expr2) = LamApp (LamAbs (head list) (convert expr2))(LamAbs (head $ tail list) (convert expr1))

-- helperCLet :: 

convertVar :: Expr -> LamExpr
convertVar (Var int) = LamVar int
convertVar expr = convert expr

convertApp :: Expr -> LamExpr
convertApp (App (Var int1) (Var int2)) = LamApp (convert (Var int2)) (convert (Var int1)) 
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

getInt :: Expr -> Int
getInt (Var int) = int



-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
-- replace the definition below with your solution
prettyPrint e = ""

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