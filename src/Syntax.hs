module Syntax where

import Data.Set (union)
import qualified Data.Set as S
import Data.Foldable (foldl')

type Name = String

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Lt
  | Lte
  | Gt
  | Gte
  | Eq
  deriving (Eq, Ord, Show)

str2binOp :: [(String, BinOp)]
str2binOp  =
  [ ("+"  , Add)
  , ("-"  , Sub)
  , ("*"  , Mul)
  , ("/"  , Div)
  , ("<"  , Lt)
  , ("<=" , Lte)
  , (">"  , Gt)
  , (">=" , Gte)
  , ("="  , Eq)
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
  ,"and"
  ,"or"
  ,"not"
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

findFreeVars :: Expr -> [Name]
findFreeVars (FuncExp vars exp) = S.toList $ go vars exp
 where
  go vars (VarExp var) = if var `elem` vars then S.empty else S.singleton var
  go vars (BinOpExp _ e1 e2) = go vars e1 `union` go vars e2
  go vars (IfExp e1 e2 e3) = go vars e1 `union` go vars e2 `union` go vars e3
  go vars (CallExp e1 es) = go vars e1 `union` theRest
    where theRest = foldl' union S.empty $ map (go vars) es
  go _ _ = S.empty
findFreeVars _ = error "findFreeVars - not called with a function expression"
