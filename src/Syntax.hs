module Syntax where

type Name = String

binOps :: [String]
binOps =  ["+","*","-","/"]

bools :: [(String, Bool)]
bools =  [("#T", True)
         ,("#F", False)
         ]

data Expr
  = NumberExp Double
  | EmptyExp
  | BoolExp Bool
  | VarExp String
  | CallExp Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinOpExp Name Expr Expr
  deriving (Eq, Ord, Show)
