module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string)
import Data.Maybe (fromJust)
import Data.Functor ((<$>), (<$))
import Data.Foldable (asum)
import qualified Lexer as Lex
import Syntax
import Control.Applicative (liftA3, (<*>))
import Debug.Trace

numberP :: Parser Expr
numberP = NumberExp <$> (try Lex.float <|>
                         fromIntegral <$> Lex.integer)

oneOfReserved :: [String] -> Parser String
oneOfReserved = asum . map (\x -> trace x $ Lex.lexeme (string x))
{-oneOfReserved = asum . map (\x -> trace x $ Lex.reserved x)-}

mapP :: [(String, a)] -> Parser a
mapP aList = fromJust . flip lookup aList <$> oneOfReserved (map fst aList)

boolP :: Parser Expr
boolP = BoolExp <$> mapP str2bool

emptyP :: Parser Expr
emptyP = EmptyExp <$ Lex.parens eof

binOpP :: Parser Expr
binOpP = Lex.parens $ liftA3 BinOpExp (mapP str2binOp)
                                      exprP
                                      exprP

callP :: Parser Expr
callP = Lex.parens $ CallExp <$> Lex.identifier <*> many exprP

variableP :: Parser Expr
variableP = VarExp <$> Lex.identifier

exprP :: Parser Expr
exprP = try binOpP
      <|> try callP
      <|> try numberP
      <|> try boolP
      <|> variableP

contentsP :: Parser a -> Parser a
contentsP p = do
  Lex.whiteSpace
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contentsP exprP) "<stdin>" s

{-toplevel :: Parser [Expr]-}
{-toplevel = many $ do-}
    {-def <- defn-}
    {-reservedOp ";"-}
    {-return def-}

{-parseToplevel :: String -> Either ParseError [Expr]-}
{-parseToplevel s = parse (contents toplevel) "<stdin>" s-}

{-function :: Parser Expr-}
{-function = do-}
  {-reserved "def"-}
  {-name <- identifier-}
  {-args <- parens $ many identifier-}
  {-body <- expr-}
  {-return $ Function name args body-}

{-extern :: Parser Expr-}
{-extern = do-}
  {-reserved "extern"-}
  {-name <- identifier-}
  {-args <- parens $ many identifier-}
  {-return $ Extern name args-}

{-defn :: Parser Expr-}
{-defn = try extern-}
    {-<|> try function-}
    {-<|> expr-}
