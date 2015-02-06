{-# Options_GHC -fno-warn-missing-signatures #-}

module Lexer where

import Syntax

import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    reserved = bools ++ binOps
    style = emptyDef {
               Tok.commentLine = "#"
             , Tok.reservedNames = reserved
             }

float      = Tok.float lexer
parens     = Tok.parens lexer
commaSep   = Tok.commaSep lexer
identifier = Tok.identifier lexer
whiteSpace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
