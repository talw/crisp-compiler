module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (fromJust)
import Data.Functor ((<$>), (<$))
import Data.Function (on)
import Data.Foldable (asum)
import Data.List (sortBy)
import qualified Lexer as LX
import Syntax
import Control.Applicative (liftA3, (<*>), (*>), pure)

numberP :: Parser Expr
numberP = NumberExp . fromIntegral <$> LX.integer

oneOfReserved :: [String] -> Parser String
oneOfReserved = asum . map LX.reserved . sortBy (flip compare `on` length)

mapP :: [(String, a)] -> Parser a
mapP aList = fromJust . flip lookup aList <$> oneOfReserved (map fst aList)

charP :: Parser Expr
charP = CharExp <$> (string "#\\" *> anyChar)

boolP :: Parser Expr
boolP = BoolExp <$> mapP str2bool

emptyP :: Parser Expr
emptyP = do
  LX.reserved dataPrefix
  LX.parens (return EmptyExp)

binOpP :: Parser Expr
binOpP = LX.parens $ liftA3 BinOpExp
  (mapP str2binOp)
  exprP
  exprP

callP :: Parser Expr
callP = LX.parens $ CallExp
  <$> (variableP <|> lambdaP)
  <*> many exprP

listP :: Parser Expr
listP = do
  LX.reserved dataPrefix
  LX.parens $ do
    exprs <- many exprP
    return $ foldr (\a acc -> PrimCallExp "cons" [a,acc]) EmptyExp exprs

arrayP :: Parser Expr
arrayP = do
  LX.reserved vectorPrefix
  LX.parens $ ArrayExp <$> many exprP

stringP :: Parser Expr
stringP =
  StringExp <$> LX.stringLiteral

variableP :: Parser Expr
variableP = VarExp <$> LX.identifier

defineP :: Parser Expr
defineP = reservedFuncP "define" $
    DefExp <$> LX.identifier <*> exprP

lambdaP :: Parser Expr
lambdaP = reservedFuncP "lambda" $
  FuncExp <$> LX.parens (many LX.identifier) <*> many exprP

ifP :: Parser Expr
ifP = reservedFuncP "if" $
  liftA3 IfExp exprP exprP exprP

notP :: Parser Expr
notP = reservedFuncP "not" $ do
  expr <- exprP
  return $ IfExp expr (BoolExp False) (BoolExp True)

primFuncP :: Parser Expr
primFuncP = LX.parens $
  PrimCallExp <$> mapP primFuncs <*> many exprP

andP :: Parser Expr
andP = reservedFuncP "and" go
 where
  go = do
    expr <- exprP
    liftA3 IfExp (pure expr) go (pure (BoolExp False)) <|> return expr

orP :: Parser Expr
orP = reservedFuncP "or" go
 where
  go = do
    expr <- exprP
    let notExp = IfExp expr (BoolExp False) (BoolExp True)
    liftA3 IfExp (pure notExp) go (pure expr) <|> return expr

letP :: Parser Expr
letP = reservedFuncP "let" $ do
  nameExprPairs <- LX.parens $
    many $ LX.parens $ (,) <$> LX.identifier <*> exprP
  body <- many exprP
  return $ CallExp
    (FuncExp (map fst nameExprPairs) body)
    $ map snd nameExprPairs

setP :: Parser Expr
setP = reservedFuncP "set!" $
  SetExp <$> LX.identifier <*> exprP

reservedFuncP :: String -> Parser a -> Parser a
reservedFuncP name parser = LX.parens $ do
  LX.reserved name
  parser

exprP :: Parser Expr
exprP = LX.lexeme . asum . map try $
  [ binOpP
  , callP
  , defineP
  , primFuncP
  , ifP
  , notP
  , andP
  , orP
  , listP
  , arrayP
  , stringP
  , letP
  , setP
  , lambdaP
  , numberP
  , boolP
  , charP
  , variableP
  , emptyP
  ]

contentsP :: Parser a -> Parser a
contentsP p = do
  LX.whiteSpace
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError [Expr]
parseExpr s = parse (contentsP toplevel) "<stdin>" s

toplevel :: Parser [Expr]
toplevel = many exprP
