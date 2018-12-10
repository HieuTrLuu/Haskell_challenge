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

--factor :: Parse Expr
--factor = do symbol "("
--            e <-expr
--            symbol ")"
--            return e
--          <|> var


factor :: Parser Expr
factor = do symbol "("
            n <- var
            symbol ")"
            m <- var
            return (App n m)
         <|> var



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



