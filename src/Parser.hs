{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import ClassyPrelude hiding ((<|>), try)

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (liftA3)
import Data.Maybe (fromJust)

import qualified Lexer as Lex
import Syntax

numberP :: Parser Expr
numberP = NumberExp <$> Lex.float

boolP :: Parser Expr
boolP = BoolExp <$> mapParse bools
  where mapParse :: [(String, a)] -> Parser a
        mapParse aList = fromJust . flip lookup aList <$> oneOfStr (map fst aList)

emptyP :: Parser Expr
emptyP = EmptyExp <$ Lex.parens eof

binOpP :: Parser Expr
binOpP =  Lex.parens $ liftA3 BinOpExp (oneOfStr binOps) exprP exprP

oneOfStr :: [String] -> Parser String
oneOfStr strs = try $ do
  str <- Lex.stringLiteral
  if str `elem` strs
    then return str
    else mzero

callP :: Parser Expr
callP = Lex.parens $ CallExp <$> Lex.identifier <*> many exprP

variableP :: Parser Expr
variableP = VarExp <$> Lex.identifier

exprP :: Parser Expr
exprP = try numberP
      <|> try boolP
      <|> try callP
      <|> try variableP
      <|> binOpP

contentsP :: Parser a -> Parser a
contentsP p = do
  Lex.whiteSpace
  r <- p
  Lex.whiteSpace
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contentsP exprP) "<stdin>" s
