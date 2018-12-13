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
  | otherwise =        LamApp (LamAbs (head list) (convert expr2)) (helperLet (convert expr1) (tail list)) --TODO: why the recursion is true for more than 2 ?
--convertLet (Let list expr1 expr2) = LamApp (LamAbs (head list) (convert expr2))(LamAbs (head $ tail list) (convert expr1))
--convertLet (Let [1,2,3,4,5] (Var 2) (Var 1)) = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamVar 2))))) --TODO: fix this

helperLet :: LamExpr -> [Int] -> LamExpr
--helperLet (Var int) list = LamApp (LamAbs (head list) (convert expr2)) (convertLet expr1)
--TODO: because if the expression only has 1 var, this is the way to go, can it be var and the list has more than 1 elt ?
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


getType :: Expr ->  String
getType (Var _) = "Var"
getType (Let i1 i2 i3) = "Let"
getType (App i1 i2) = "App"

getLambdaType :: LamExpr ->  String
getLambdaType (LamVar _) = "Var"
getLambdaType (LamAbs int e) = "Abs"
getLambdaType (LamApp e1 e2) = "App"


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

-- (x1 let x2=x3 in x4) x5" is equivalent to "x1 (let x2=x3 in x4) x5
--TODO: how to check that the way i deal with bracket here is correct ?
-- lambda calculus is left associate, convert back to LamdaCalculus to check


-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
parseLet s = Just (solveChallenge3 s)
--parseLet s | expr ==  = Nothing
--           | otherwise = Just expr
-- TODO: fix the nothing in challenge 3

solveChallenge3 :: String -> Expr
solveChallenge3 input = fst $ head $ parse exprS input


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

--recursionOnVar :: (Expr, String) -> (Expr, String)
--recursionOnVar (ex,"") = (ex,"")
--recursionOnVar (expr,str) = (App expr (fst $ head $ (parse factor str)), snd $ head (parse factor str))



exprX' :: Parser Expr
exprX' = do symbol "x"
            n <- nat
            symbol "x"
            o <- nat
            return (App (Var n) (Var o))
         <|>
         do symbol "x"
            n <- nat
            return (Var n)

--exprX' :: Parser Expr
--exprX' = do symbol "x"
--            n <- nat
--            return (Var n)
--         do symbol "x"
--            n <- nat
--            symbol "x"
--            m <- nat
--            return (Var n)

exprS :: Parser Expr
exprS = do symbol "let"
           a <- exprApp
           let l = prettyPrint a
           let list = getIntList(l,[])
           symbol "="
           do f1 <- exprF
              symbol "in"
              do f2 <- exprF
                 return (Let list f1 f2)
        <|>
        do f <- exprF
           return f


exprA :: Parser Expr
exprA = do x <- exprX'
           a' <- exprA'
           return (App x a')
        <|>
        do x <- exprX'
           return x


exprF :: Parser Expr
exprF = do symbol "("
           a <- exprA
           symbol ")"
           return a
        <|>
        do x <- exprX'
           a' <- exprA'
           return (App x a')
        <|>
        do x <- exprX'
           return x

exprA' :: Parser Expr
exprA' = do a <- exprA
            a' <- exprA'
            return (App a a')

exprApp :: Parser Expr
exprApp = do
  x <- factor
  do e <- exprA
     return (App x e)
  <|>
  do x <- factor
     return x

-- the way to implement it is to make App ::= SubExpr Expr, where SubExpr is the same as Expr but without the App
--TODO: fix the parseLet"x1 (x2 x3)" - exprA suppose to deal with this but not sure what happens ?
exprList :: Parser Expr
exprList = do
  x <- factor
  do e <- exprList
     return (App x e)
  <|>
  do x <- factor
     return x

factor :: Parser Expr
factor = do symbol "("
            e <- exprX'
            symbol ")"
            return e
          <|> exprX'






getInt :: Expr -> [Int]
getInt (Var x) = [x]
getInt (App (Var x) (Var y)) = [x,y]

getIntList :: (String, [Int]) -> [Int]
getIntList (input, list) | str == "" = append list num
                         | otherwise = getIntList (str, append list num)
 where
  temp = parse exprX' input
  str =  snd $ head temp
  num = getInt $ fst (head temp)
  --buffer = parse exprA str

-- combine 2 [Int] list in correct order
--e.g append [1,2] [3,4] = [1,2,3,4]
append :: [Int] -> [Int] -> [Int]
append xs ys = foldr (\x y -> x:y) ys xs



-- Challenge 4
-- count reductions using two different strategies
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution
countReds e limit = (first, second)
  where
    first = countLI e limit
    second = countRI e limit

countRI ::LamExpr -> Int -> Maybe Int
countRI e limit | count <= limit = Just (count)
                | otherwise = Nothing
  where
    count = (length $ reductions evalRI e) +1

countLI ::LamExpr -> Int -> Maybe Int
countLI e limit | count <= limit = Just (count)
                | otherwise = Nothing
  where
    count = (length $ reductions evalLI e) +1
--TODO: fix the part nothing part in challange4

subst :: LamExpr -> Int ->  LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x' = rename x in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

testSubst1 = LamAbs 1 (LamVar 2)

free :: Int -> LamExpr -> Bool
free x (LamVar y) =  x == y
--free or bound depend on a lambda expression why do we need an integer as argument ?
-- we check whether a variable (int argument) is free in an lambda expression
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2)  = (free x e1) || (free x e2)

rename :: Int -> Int
rename x = (-x)
--TODO: write your own eval1cbn and eval1cbv

-- Performs a single step of call-by-name reduction
eval1cbn :: LamExpr -> LamExpr
eval1cbn (LamAbs x e) = (LamAbs x e)
eval1cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn (LamApp e1 e2) = (LamApp (eval1cbn e1) e2)

eval1cbv :: LamExpr -> LamExpr
eval1cbv (LamAbs x e) = (LamAbs x e)
eval1cbv (LamApp (LamAbs x e1) e@(LamApp y e2)) = subst e1 x e
eval1cbv (LamApp e@(LamAbs x e1) e2) = LamApp e (eval1cbv e2)
eval1cbv (LamApp e1 e2) = LamApp (eval1cbv e1) e2




evalLI :: LamExpr -> LamExpr
evalLI (LamVar x) = LamVar x
evalLI (LamAbs x e) = (LamAbs x e)
evalLI (LamApp (LamAbs int e1@(LamVar v1)) e2@(LamVar v2)) = subst e1 int e2
evalLI (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
evalLI (LamApp e@(LamAbs int e1) e2) = subst e1 int e2
evalLI (LamApp e1@(LamVar int) e2) = LamApp e1 (evalLI e2)
evalLI (LamApp e1 e2) = LamApp (evalLI e1) e2

evalRI :: LamExpr -> LamExpr
evalRI (LamVar x) = LamVar x
evalRI (LamAbs int x) = (LamAbs int x)
evalRI (LamApp expr (LamApp (LamAbs int e1@(LamVar v1)) e2@(LamVar v2))) = LamApp expr (subst e1 int e2)
evalRI (LamApp expr (LamApp (LamAbs x e1) e@(LamAbs y e2))) = LamApp expr (subst e1 x e)
evalRI (LamApp (LamAbs int1 expr) e2) = subst expr int1 e2
evalRI (LamApp e1 e2@(LamVar int)) = LamApp (evalRI e1) e2
evalRI (LamApp e1 e2) = LamApp e1 (evalRI e2)





-- custom lambda calculus expressions test values
lam1 = (LamAbs 1 (LamVar 1))
--lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6rhs = (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))

wrong = (LamApp (LamAbs 2 (LamVar 3)) (LamVar 5))



--reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
--reductions ss e = [ p | p <- zip evals (tail evals) ]
--   where evals = takeWhile (\x -> (getLambdaType x) /= "Var") $ iterate ss e

reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = takeWhile (\x -> (ss e) == (ss $ ss e)) $ iterate ss e



--evalcbn = eval eval1cbn
--tracecbn = trace eval1cbn
--evalcbv = eval eval1cbv
--tracecbv = trace eval1cbv

lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))


lamTestSub1 = (LamAbs 1 (LamAbs 2 (LamVar 1)))
lamTestSub3 = LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)


lamTest = LamApp (LamApp lamTestSub1 (LamVar 3)) lamTestSub3

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
--TODO: write a parser for this.

compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith s = Nothing


encodding :: Int -> LamExpr
encodding 0 = (LamAbs 1 (LamAbs 2 (LamVar 2)))
encodding 1 = (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))
encodding n = (LamAbs 1 (LamAbs 2 (genEncodding n)))

genEncodding :: Int -> LamExpr
genEncodding 0 = LamVar 2
genEncodding 1 = (LamApp (LamVar 1) (LamVar 2))
genEncodding n = (LamApp (LamVar 1) (genEncodding (n-1)))

--succ1 = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))
--zero = (LamAbs 1 (LamAbs 2 (LamVar 2)))
--one = (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))
--two = (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))
--betaEq = (LamApp (encodding 1) succ1)
--
--lambdaExpr0 = (LamAbs 1 (LamAbs 2 (LamVar 2)))

qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

eval :: (LamExpr -> LamExpr) -> LamExpr -> LamExpr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (LamExpr -> LamExpr) -> LamExpr -> [LamExpr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss

evalcbn = eval eval1cbn
tracecbn = trace eval1cbn
evalcbv = eval eval1cbv
tracecbv = trace eval1cbv

--TODO: finish the reduction then we are done
--TODO: sorry I forgot about the parser on 5


main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
