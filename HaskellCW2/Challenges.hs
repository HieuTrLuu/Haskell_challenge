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

--data App a = App Expr Expr deriving (Show,Eq)
--data Let a = Let [Int] Expr Expr
--data Var a = Var Int
--
--data LambApp a = LamApp LamExpr LamExpr
--data LambAbs a =  LamAbs Int LamExpr
--data LambVar a = LambVar Int

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
-- replace the definition below with your solution

convertLet e = (LamVar 0)

--convertLet (Var int) = (LamVar int)
--convertLet (Let list expr1 expr2) =
-- where
--  expr1 =
--  expr2 =
--



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