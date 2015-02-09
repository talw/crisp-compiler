{-# Options_GHC -fno-warn-missing-signatures #-}

module Lexer where

import Syntax

import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Char (letter, oneOf)
import Data.Functor ((<$))
import Control.Applicative ((<|>))

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
 where
  style = emptyDef
            { Tok.commentLine   = "#"
            , Tok.reservedNames = reservedWords
                             ++ map fst str2bool
                             ++ map fst str2binOp
            , Tok.identStart    = letter <|> oneOf "_#"
            }

lexeme        = Tok.lexeme lexer
float         = Tok.float lexer
integer       = Tok.integer lexer
parens        = Tok.parens lexer
commaSep      = Tok.commaSep lexer
identifier    = Tok.identifier lexer
whiteSpace    = Tok.whiteSpace lexer
reserved str  = str <$ Tok.reserved lexer str
reservedOp    = Tok.reservedOp lexer
operator      = Tok.operator lexer
stringLiteral = Tok.stringLiteral lexer
