module Syntax where

type Name = String

data BinOp = Add
           | Sub
           | Mul
           | Div
  deriving (Eq, Ord, Show)

str2binOp :: [(String, BinOp)]
str2binOp  = [("+", Add)
             ,("-", Sub)
             ,("*", Mul)
             ,("/", Div)
             ]

str2bool :: [(String, Bool)]
str2bool =  [("T", True)
            ,("F", False)
            ]

data Expr
  = NumberExp Double
  | EmptyExp
  | BoolExp Bool
  | VarExp String
  | CallExp Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinOpExp BinOp Expr Expr
  deriving (Eq, Ord, Show)
