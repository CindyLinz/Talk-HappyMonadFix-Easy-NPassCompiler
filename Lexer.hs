module Lexer
  ( Token (..)
  , lexer
  ) where

import Data.Char

data Token
  = TokenFunction
  | TokenParL
  | TokenParR
  | TokenCurL
  | TokenCurR
  | TokenPlus
  | TokenMinus
  | TokenLessEqual
  | TokenIf
  | TokenElse
  | TokenNum Int
  | TokenIdentity [Char]
  | TokenArg
  deriving Show

lexer :: [Char] -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('(':cs) = TokenParL : lexer cs
lexer (')':cs) = TokenParR : lexer cs
lexer ('{':cs) = TokenCurL : lexer cs
lexer ('}':cs) = TokenCurR : lexer cs
lexer ('<':'=':cs) = TokenLessEqual : lexer cs
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexIdentity (c:cs)
  | isDigit c = lexDigit (c:cs)

lexIdentity cs =
  case span isAlpha cs of
    ("function", cs') -> TokenFunction : lexer cs'
    ("if", cs') -> TokenIf : lexer cs'
    ("else", cs') -> TokenElse : lexer cs'
    ("arg", cs') -> TokenArg : lexer cs'
    (id, cs') -> TokenIdentity id : lexer cs'

lexDigit cs = TokenNum (read num) : lexer cs'
  where (num, cs') = span isDigit cs

