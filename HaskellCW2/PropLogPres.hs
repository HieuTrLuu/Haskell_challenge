-- Example for lecture to demonstrate simple monadic parsing
import Parsing

data BExp = Tru | Fls | And BExp BExp | Or BExp BExp deriving (Eq,Show)

--data BExp = Tru | Fls | Var String | And BExp BExp | Or BExp BExp
--   deriving (Eq,Show)

-----------------------------------------------------------------------------
-- c3expr   ::= let var = var in var | var
-- var    ::= var var | x[int]

-- var    ::= x[int] var'
-- var'   ::= var var' | empty


-- factor ::=  ( c3expr ) | var

-----------------------------------------------------------------------------


data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int
   deriving (Eq,Show)

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
--           l <- string ""
           a <- exprA
           let l = "x1 x2 x3" --remember this is hard coded
           let list = getIntList(l,[])
           symbol "="
           do c1 <- exprC
              symbol "in"
              do c2 <- exprC
                 return (Let list c1 c2)

-- test: parse exprL "let = x2 in x1" need to do smt inbetween this place
--[(Let [1,2,3] (Var 2) (Var 1),"")]





exprC :: Parser Expr
exprC = do a <- exprA
           return a
    <|> do l <- exprL
           return l



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



