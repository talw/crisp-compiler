module Syntax where

type Name = String

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Lt
  deriving (Eq, Ord, Show)

str2binOp :: [(String, BinOp)]
str2binOp  =
  [("+", Add)
  ,("-", Sub)
  ,("*", Mul)
  ,("/", Div)
  ,("<", Lt)
  ]

str2bool :: [(String, Bool)]
str2bool =
  [("#t", True)
  ,("#f", False)
  ]

reservedWords :: [String]
reservedWords =
  ["define"
  ,"lambda"
  ,"if"
  ]

data Expr
  = NumberExp Integer
  | EmptyExp
  | CharExp Char
  | BoolExp Bool
  | VarExp String
  | GlbVarExp Name
  | DefExp Name Expr
  | IfExp Expr Expr Expr
  | FuncExp [Name] Expr
  | CallExp Expr [Expr]
  {-| Extern Name [Name]-}
  | BinOpExp BinOp Expr Expr
  deriving (Eq, Ord, Show)
