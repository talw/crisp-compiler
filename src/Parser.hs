module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (fromJust)
import Data.Functor ((<$>), (<$))
import Data.Foldable (asum)
import qualified Lexer as LX
import Syntax
import Control.Applicative (liftA3, (<*>), (*>))

numberP :: Parser Expr
numberP = NumberExp <$> (try LX.float <|>
                         fromIntegral <$> LX.integer)

oneOfReserved :: [String] -> Parser String
oneOfReserved = asum . map (LX.lexeme . string)

mapP :: [(String, a)] -> Parser a
mapP aList = fromJust . flip lookup aList <$> oneOfReserved (map fst aList)

boolP :: Parser Expr
boolP = BoolExp <$> mapP str2bool

emptyP :: Parser Expr
emptyP = EmptyExp <$ LX.parens eof

binOpP :: Parser Expr
binOpP = LX.parens $ liftA3 BinOpExp
  (mapP str2binOp)
  exprP
  exprP

callP :: Parser Expr
callP = LX.parens $ CallExp
  <$> (GlbVarExp <$> LX.identifier)
  <*> many exprP

variableP :: Parser Expr
variableP = VarExp <$> LX.identifier

defineP :: Parser Expr
defineP = LX.parens $ DefExp
  <$> (LX.reserved "define" *> LX.identifier)
  <*> exprP

lambdaP :: Parser Expr
lambdaP  = LX.parens $ FuncExp
  <$> (LX.reserved "lambda" *> LX.parens (many LX.identifier))
  <*> exprP

exprP :: Parser Expr
exprP = LX.lexeme $
  try binOpP
  <|> try callP
  <|> try defineP
  <|> try lambdaP
  <|> try numberP
  <|> try boolP
  <|> variableP

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
