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

program :: { Parser () }
  : code
    { do
      liftIO $ putStr programTmplPrefix
      $1
      liftIO $ putStr programTmplPostfix
    }

code :: { Parser () }
  : expr
    { do
      liftIO $ putStr mainTmplPrefix
      $1
      liftIO $ putStr mainTmplPostfix
    }
  | func code
    { $1 >> $2
    }

func :: { Parser () }
  : 'function' I_IDENTITY '{' expr '}'
    { do
      funcLabel <- newFunc $2
      liftIO $ putStr $
        funcLabel ++ ":\n" ++
        "pushq %rbp\n" ++
        "movq %rsp, %rbp\n"
      $4
      liftIO $ putStr $
        "popq %rax\n" ++
        "popq %rbp\n" ++
        "ret\n"
    }

expr :: { Parser () }
  : 'arg'
    { liftIO $ putStr $
        "movq 16(%rbp), %rax\n" ++
        "pushq %rax\n"
    }
  | I_NUMBER
    { liftIO $ putStr $ "pushq $" ++ show $1 ++ "\n"
    }
  | I_IDENTITY '(' expr ')'
    { do
      funcLabel <- lookupFunc $1
      $3
      liftIO $ putStr $
        "call " ++ funcLabel ++ "\n" ++
        "popq %rbx\n" ++
        "pushq %rax\n"
    }
  | expr '+' expr
    { do
      $1
      $3
      liftIO $ putStr $
        "popq %rbx\n" ++
        "popq %rax\n" ++
        "addq %rbx, %rax\n" ++
        "pushq %rax\n"
    }
  | expr '-' expr
    { do
      $1
      $3
      liftIO $ putStr $
        "popq %rbx\n" ++
        "popq %rax\n" ++
        "subq %rbx, %rax\n" ++
        "pushq %rax\n"
    }
  | expr '<=' expr
    { do
      $1
      $3
      trueLabel <- nextLabel
      endLabel <- nextLabel
      liftIO $ putStr $
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
    { do
      elseLabel <- nextLabel
      endLabel <- nextLabel
      $3
      liftIO $ putStr $
        "popq %rax\n" ++
        "cmpq $0, %rax\n" ++
        "jz " ++ elseLabel ++ "\n"
      $6
      liftIO $ putStr $
        "jmp " ++ endLabel ++ "\n" ++
        elseLabel ++ ":\n"
      $10
      liftIO $ putStr $
        endLabel ++ ":\n"
    }

{

parseError :: [Token] -> a
parseError tks = error $ "parseError: " ++ show tks

main = do
  source <- getContents
  rec parserState <- execStateT (cheapParse (lexer source)) (initParserState (parserFuncTable parserState))
  return ()
}
