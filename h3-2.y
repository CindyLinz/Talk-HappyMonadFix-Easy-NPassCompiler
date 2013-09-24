{

{-# LANGUAGE DoRec #-}
-- vim: filetype=haskell

module Main where


import Control.Monad.State

import Lexer
import ParserState2
import Template

}

%name cheapParse
%tokentype { Token }
%monad { Parser }
%error { parseError }

%token
  'function' { TokenFunction }
  '(' { TokenParL }
  ')' { TokenParR }
  '{' { TokenCurL }
  '}' { TokenCurR }
  I_IDENTITY { TokenIdentity $$ }
  I_NUMBER { TokenNum $$ }
  '+' { TokenPlus }
  '-' { TokenMinus }
  '<=' { TokenLessEqual }
  'if' { TokenIf }
  'else' { TokenElse }
  'arg' { TokenArg }

%left '<='
%left '+' '-'

%%

program :: { [Char] }
  : code
    { programTmpl $1
    }

code :: { [Char] }
  : expr
    { mainTmpl $1
    }
  | func code
    { $1 ++
      $2
    }

func :: { [Char] }
  : 'function' I_IDENTITY '{' expr '}'
    {% do
      funcLabel <- newFunc $2
      return $
        funcLabel ++ ":\n" ++
        "pushq %rbp\n" ++
        "movq %rsp, %rbp\n" ++
        $4 ++
        "popq %rax\n" ++
        "popq %rbp\n" ++
        "ret\n"
    }

expr :: { [Char] }
  : 'arg'
    { "movq 16(%rbp), %rax\n" ++
      "pushq %rax\n"
    }
  | I_NUMBER
    { "pushq $" ++ show $1 ++ "\n"
    }
  | I_IDENTITY '(' expr ')'
    {% do
      funcLabel <- lookupFunc $1
      return $
        $3 ++
        "call " ++ funcLabel ++ "\n" ++
        "popq %rbx\n" ++
        "pushq %rax\n"
    }
  | expr '+' expr
    { $1 ++
      $3 ++
      "popq %rbx\n" ++
      "popq %rax\n" ++
      "addq %rbx, %rax\n" ++
      "pushq %rax\n"
    }
  | expr '-' expr
    { $1 ++
      $3 ++
      "popq %rbx\n" ++
      "popq %rax\n" ++
      "subq %rbx, %rax\n" ++
      "pushq %rax\n"
    }
  | expr '<=' expr
    {% do
      trueLabel <- nextLabel
      endLabel <- nextLabel
      return $
        $1 ++
        $3 ++
        "popq %rbx\n" ++
        "popq %rax\n" ++
        "cmpq %rbx, %rax\n" ++
        "jbe " ++ trueLabel ++ "\n" ++
        "pushq $0\n" ++
        "jmp " ++ endLabel ++ "\n" ++
        trueLabel ++ ":\n" ++
        "pushq $1\n" ++
        endLabel ++ ":\n"
    }
  | 'if' '(' expr ')' '{' expr '}' 'else' '{' expr '}'
    {% do
      elseLabel <- nextLabel
      endLabel <- nextLabel
      return $
        $3 ++
        "popq %rax\n" ++
        "cmpq $0, %rax\n" ++
        "jz " ++ elseLabel ++ "\n" ++
        $6 ++
        "jmp " ++ endLabel ++ "\n" ++
        elseLabel ++ ":\n" ++
        $10 ++
        endLabel ++ ":\n"
    }

{

parseError :: Monad m => [Token] -> ParserT m a
parseError tks = fail $ "parseError: " ++ show tks

main = do
  source <- getContents
  rec (result, parserState) <- runStateT (cheapParse (lexer source)) (initParserState (parserFuncTable parserState))
  putStr result

}
