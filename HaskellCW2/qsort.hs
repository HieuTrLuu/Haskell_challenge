
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

qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])

--TODO there is something with this, it can debug here but not int the challenges file