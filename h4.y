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

program :: { Parser [Char] }
  : code
    { $1 >>= return . programTmpl
    }

code :: { Parser [Char] }
  : expr
    { $1 >>= return . mainTmpl
    }
  | func code
    { do
      func <- $1
      code <- $2
      return $ func ++ code
    }

func :: { Parser [Char] }
  : 'function' I_IDENTITY '{' expr '}'
    { do
      funcLabel <- newFunc $2
      body <- $4
      return $
        funcLabel ++ ":\n" ++
        "pushq %rbp\n" ++
        "movq %rsp, %rbp\n" ++
        body ++
        "popq %rax\n" ++
        "popq %rbp\n" ++
        "ret\n"
    }

expr :: { Parser [Char] }
  : 'arg'
    { return $
        "movq 16(%rbp), %rax\n" ++
        "pushq %rax\n"
    }
  | I_NUMBER
    { return $ "pushq $" ++ show $1 ++ "\n"
    }
  | I_IDENTITY '(' expr ')'
    { do
      funcLabel <- lookupFunc $1
      arg <- $3
      return $
        arg ++
        "call " ++ funcLabel ++ "\n" ++
        "popq %rbx\n" ++
        "pushq %rax\n"
    }
  | expr '+' expr
    { do
      arg1 <- $1
      arg2 <- $3
      return $
        arg1 ++
        arg2 ++
        "popq %rbx\n" ++
        "popq %rax\n" ++
        "addq %rbx, %rax\n" ++
        "pushq %rax\n"
    }
  | expr '-' expr
    { do
      arg1 <- $1
      arg2 <- $3
      return $
        arg1 ++
        arg2 ++
        "popq %rbx\n" ++
        "popq %rax\n" ++
        "subq %rbx, %rax\n" ++
        "pushq %rax\n"
    }
  | expr '<=' expr
    { do
      trueLabel <- nextLabel
      endLabel <- nextLabel
      arg1 <- $1
      arg2 <- $3
      return $
        arg1 ++
        arg2 ++
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
      cond <- $3
      positive <- $6
      negative <- $10
      return $
        cond ++
        "popq %rax\n" ++
        "cmpq $0, %rax\n" ++
        "jz " ++ elseLabel ++ "\n" ++
        positive ++
        "jmp " ++ endLabel ++ "\n" ++
        elseLabel ++ ":\n" ++
        negative ++
        endLabel ++ ":\n"
    }

{

parseError :: [Token] -> a
parseError tks = error $ "parseError: " ++ show tks

main = do
  source <- getContents
  rec (result, parserState) <- runStateT (cheapParse (lexer source)) (initParserState (parserFuncTable parserState))
  putStr result

}
