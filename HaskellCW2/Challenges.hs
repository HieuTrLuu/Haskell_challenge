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
  | otherwise = LamApp (LamAbs (head list) (convert expr2)) (helperLet (convert expr1) (tail list))
   where
     temp = helperLet (LamAbs (head $ tail list) (convert expr1)) (tail $ tail list)

-- Additional test case
--convertLet (Let list expr1 expr2) = LamApp (LamAbs (head list) (convert expr2))(LamAbs (head $ tail list) (convert expr1))
--convertLet (Let [1,2,3,4,5] (Var 2) (Var 1)) = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamVar 2))))) --TODO: fix the order of this

helperLet :: LamExpr -> [Int] -> LamExpr
helperLet e@(LamVar int) list | length list == 0 = e
                              | otherwise = helperLet ((LamAbs (head list) e)) (tail list)

helperLet e@(LamApp e1 e2) list | length list == 0 = e
                                | otherwise = helperLet ((LamAbs (head list) e)) (tail list)

helperLet e@(LamAbs int expr) list | length list == 0 = e
                                   | otherwise = helperLet (LamAbs int (LamAbs (head list) expr)) (tail list)



convertVar :: Expr -> LamExpr
convertVar (Var int) = LamVar int
convertVar expr = convert expr

convertApp :: Expr -> LamExpr
convertApp (App (Var int1) (Var int2)) = LamApp (convert (Var int1)) (convert (Var int2))
convertApp expr = convert expr


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
-- lambda calculus is left associate, convert back to LamdaCalculus to check


-- Challenge 3
-- parse a let expression
--
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Initial grammar
--S -> let list e F i F | F
--X -> x
--A -> A A | X
--F -> ( F ) | A


-- without lieft recursion grammar
-- S -> let list e F i F
--    | F
-- X -> x
-- A -> x A' | x
-- F -> ( F )
--    | x A' |x
--A' -> A A'

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
parseLet :: String -> Maybe Expr
parseLet s = Just (solveChallenge3 s)


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
    count = length $ trace evalRI e

countLI ::LamExpr -> Int -> Maybe Int
countLI e limit | count <= limit = Just (count)
                | otherwise = Nothing
  where
    count = length $ trace evalLI e
--TODO: fix the part nothing part in challenge4

subst :: LamExpr -> Int ->  LamExpr -> LamExpr
--replace the leaf node
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
--have problem with variable capturing (problem with free and bounds var)
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
--alpha-conversion: solve the partial function problem
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x' = rename x in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

testSubst1 = LamAbs 1 (LamVar 2)

-- we check whether a variable (int argument) is free in an lambda expression
free :: Int -> LamExpr -> Bool
free x (LamVar y) =  x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2)  = (free x e1) || (free x e2)

--alpha conversion, change the, switch the sign of the var
rename :: Int -> Int
rename x = (-x)

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


reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: (LamExpr -> LamExpr) -> LamExpr -> LamExpr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (LamExpr -> LamExpr) -> LamExpr -> [LamExpr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss



--test value
lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))
lamTestSub1 = (LamAbs 1 (LamAbs 2 (LamVar 1)))
lamTestSub3 = LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)
lamTest = LamApp (LamApp lamTestSub1 (LamVar 3)) lamTestSub3

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Initial grammar
--S -> V | A
--I -> indent
--A -> o I p V c
--V -> A V | N | V p V | o V c
--N -> nat

-- with out left recursion and empty terminal
--S -> V | A
--I -> indent
--A -> o I p V c
--V -> o I p V c V V' | N V' | o V c V' | o I p V c V | N | o V c
--N -> nat
--V' -> p V V' |

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show



exprS5 :: Parser Int
exprS5 = do v <- exprV5
            return v
         <|>
         do a <- exprA5
            return a

exprA5 :: Parser Int
exprA5 = do symbol "("
            symbol "+"
            v <- exprV5
            symbol ")"
            return v

exprV5 :: Parser Int
exprV5 = do symbol "("
            symbol "+"
            v1 <- exprV5
            symbol ")"
            v2 <- exprV5
            v' <- exprV5'
            return (v1 + v2 + v') --TODO: what do i return here ?
         <|>
         do n <- nat
            v' <- exprV5'
            return (n) --TODO: what do i return here ?
         <|>
         do symbol "("
            v <- exprV5
            symbol ")"
            v' <- exprV5'
            return (v) --TODO: what do i return here ?
         <|>
         do symbol "("
            symbol "+"
            v1 <- exprV5
            symbol ")"
            v2 <- exprV5
            return (v1 + v2) --TODO: what do i return here ?
         <|>
         do n <- nat
            return n
         <|>
         do symbol "("
            v <- exprV5
            symbol ")"
            return v



exprV5' :: Parser Int
exprV5' = do v <- exprV5
             v'<- exprV5'
             return (v)

int2String :: Int -> String
int2String int = show int


compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith "(+1)" = (Just succ1)
compileArith s = encoding (compileString s)


compileString :: String -> Maybe Int
compileString str | (snd $ head $ parser str) /= [] = Nothing
                  | otherwise = Just (fst tuple)

  where
    parser =  parse exprS5
    tuple = head $ parser str

encoding :: Maybe Int -> Maybe LamExpr
encoding Nothing = Nothing
encoding (Just 0) = Just zero
encoding (Just 1) = Just one
encoding (Just n) = Just (LamAbs 1 (LamAbs 2 (genEncoding n)))

genEncoding :: Int -> LamExpr
genEncoding 1 = (LamApp (LamVar 1) (LamVar 2))
genEncoding n = (LamApp (LamVar 1) (genEncoding (n-1)))



-- constant
succ1 = (LamApp (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))))
zero  = (LamAbs 1 (LamAbs 2 (LamVar 2)))
one   = (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))
two   = (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))




qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)



main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
