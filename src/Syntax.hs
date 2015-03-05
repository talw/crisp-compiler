module Syntax where

import Data.Set (union)
import qualified Data.Set as S
import Data.Foldable (foldl')

type SymName = String

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
  ,envVarName
  ,"malloc"
  ,"memalign"
  ,"let"
  ]

primFuncs :: [(String, String)]
primFuncs =
  [("boolean?", "isBoolean")
  ,("char?", "isChar")
  ,("number?", "isNumber")
  ,("isTag", "isTag")
  ]

envVarName, suffPairStruct, suffLambda, suffEnvStruct :: String
suffPairStruct = "-pairStruct"
suffLambda = "-lambda"
suffEnvStruct = "-envStruct"
envVarName = "__env"

data Expr
  = NumberExp Integer
  | EmptyExp
  | CharExp Char
  | BoolExp Bool
  | VarExp SymName
  {-| GlbVarExp SymName-}
  | DefExp SymName Expr
  | IfExp Expr Expr Expr
  | FuncExp [SymName] Expr
  | CallExp Expr [Expr]
  | PrimCallExp SymName [Expr]
  {-| Extern SymName [SymName]-}
  | BinOpExp BinOp Expr Expr
  deriving (Eq, Ord, Show)

findFreeVars :: [SymName] -> Expr -> [SymName]
findFreeVars vars exp = S.toList $ go vars exp
 where
  go vars (VarExp var) = if var `elem` vars then S.empty else S.singleton var
  go vars (BinOpExp _ e1 e2) = go vars e1 `union` go vars e2
  go vars (IfExp e1 e2 e3) = go vars e1 `union` go vars e2 `union` go vars e3
  go vars (CallExp e1 es) = go vars e1 `union` theRest
    where theRest = foldl' union S.empty $ map (go vars) es
  go _ _ = S.empty
