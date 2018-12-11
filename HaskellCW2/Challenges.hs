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
--import Sheet7

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)
--data Maybe a = Just a | Nothing
--     deriving (Eq, Ord)



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
parseLet s = Just (solveChallenge3 s)
--parseLet s | expr ==  = Nothing
--           | otherwise = Just expr
-- TODO: fix the nothing in challenge 3

solveChallenge3 :: String -> Expr
solveChallenge3 input = fst $ head $ parse exprC input


token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v


var :: Parser Expr
var = do symbol "x"
         n <- nat
         symbol "x"
         m <- nat
         return (App (Var n) (Var m))
        <|>
      do symbol "x"
         n <- nat
         return (Var n)

recursionOnVar :: (Expr, String) -> (Expr, String)
recursionOnVar (ex,"") = (ex,"")
--case recursionOnVar (ex,"")of
--  (n, []) -> n

recursionOnVar (expr,str) = (App expr (fst $ head $ (parse factor str)), snd $ head (parse factor str))

factor :: Parser Expr
factor = do symbol "("
            e <- exprX
            symbol ")"
            return e
          <|> exprX


exprX :: Parser Expr
exprX = do symbol "x"
           n <- nat
           symbol "x"
           m <- nat
           return (App (Var n) (Var m))
         <|>
          do symbol "x"
             n <- nat
             return (Var n)


exprA :: Parser Expr
exprA = do
  x <- factor
  do e <- exprA
     return (App x e)
  <|>
  do x <- factor
     return x

exprL :: Parser Expr
exprL = do symbol "let"
           a <- exprA
           let l = prettyPrint a
           let list = getIntList(l,[])
           symbol "="
           do c1 <- exprC
              symbol "in"
              do c2 <- exprC
                 return (Let list c1 c2)





exprC :: Parser Expr
exprC = do a <- exprA
           return a
    <|> do l <- exprL
           return l

--exprD :: Parser Expr
--exprD = do symbol "x"
--           n <- nat
--           return (Var n)
--
--exprE :: Parser [Expr]
--exprE = do d <- exprD
--           do e <- exprE
--              return e
--           <|>
--           do x <- factor
--              return x

getInt :: Expr -> [Int]
getInt (Var x) = [x]
getInt (App (Var x) (Var y)) = [x,y]

getIntList :: (String, [Int]) -> [Int]
getIntList (input, list) | str == "" = append list num
                         | otherwise = getIntList (str, append list num)
 where
  temp = parse exprX input
  str =  snd $ head temp
  num = getInt $ fst (head temp)
  --buffer = parse exprA str

append :: [Int] -> [Int] -> [Int]
append xs ys = foldr (\x y -> x:y) ys xs



-- Challenge 4
-- count reductions using two different strategies 
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution

countReds e limit = (Nothing, Nothing)

-- data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

subst :: LamExpr -> Int ->  LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x' = rename x in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

free :: Int -> LamExpr -> Bool
free x (LamVar y) =  x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2)  = (free x e1) || (free x e2)

rename x = x++"\'"

-- Performs a single step of call-by-name reduction
eval1cbn :: LamExpr -> LamExpr
eval1cbn (LamAbs x e) = (LamAbs x e)
eval1cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn (LamApp e1 e2) = LamApp (eval1cbn e1) e2

-- Peforms multiple steps of call-by-name reduction until no change in term is observed
reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: (LamExpr -> LamExpr) -> LamExpr -> LamExpr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (LamExpr -> LamExpr) -> LamExpr -> [LamExpr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss

eval1cbv :: LamExpr -> LamExpr
eval1cbv (LamAbs x e) = (LamAbs x e)
eval1cbv (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
eval1cbv (LamApp e@(LamAbs x e1) e2) = LamApp e (eval1cbv e2)
eval1cbv (LamApp e1 e2) = LamApp (eval1cbv e1) e2

evalcbn = eval eval1cbn
tracecbn = trace eval1cbn
evalcbv = eval eval1cbv
tracecbv = trace eval1cbv


-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith s = Nothing

qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])