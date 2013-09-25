{

{-# LANGUAGE DoRec #-}
-- vim: filetype=haskell

module Main where

import Control.Monad.State
import Data.List

import Lexer
import ParserState3
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

program :: { Int -> Parser ([Char], Int) }
  : code
    { \offset -> do
      let
        [prefixSize, postfixSize] = map (length . filter (== '\n')) [programTmplPrefix, programTmplPostfix]
      (body, bodySize) <- $1 (offset + prefixSize)
      return (programTmplPrefix ++ body ++ programTmplPostfix, prefixSize + bodySize + postfixSize)
    }

code :: { Int -> Parser ([Char], Int) }
  : expr
    { \offset -> do
      let
        [prefixSize, postfixSize] = map (length . filter (== '\n')) [mainTmplPrefix, mainTmplPostfix]
      (body, bodySize) <- $1 (offset + prefixSize)
      return (mainTmplPrefix ++ body ++ mainTmplPostfix, prefixSize + bodySize + postfixSize)
    }
  | func code
    { \offset -> do
      (func, funcSize) <- $1 offset
      (code, codeSize) <- $2 (offset + funcSize)
      return (func ++ code, funcSize + codeSize)
    }

func :: { Int -> Parser ([Char], Int) }
  : 'function' I_IDENTITY '{' expr '}'
    { \offset -> do
      funcLabel <- newFunc $2 (offset + 1)
      (body, bodySize) <- $4 (offset + 3)
      return
        ( funcLabel ++ ":\n" ++
          "pushq %rbp\n" ++
          "movq %rsp, %rbp\n" ++
          body ++
          "popq %rax\n" ++
          "popq %rbp\n" ++
          "ret\n"
        , bodySize + 6
        )
    }

expr :: { Int -> Parser ([Char], Int) }
  : 'arg'
    { \_ -> return
      ( "movq 16(%rbp), %rax\n" ++
        "pushq %rax\n"
      , 2)
    }
  | I_NUMBER
    { \_ -> return ("pushq $" ++ show $1 ++ "\n", 1)
    }
  | I_IDENTITY '(' expr ')'
    { \offset -> do
      funcLabel <- lookupFunc $1
      (arg, argSize) <- $3 offset
      return
        ( arg ++
          "call " ++ funcLabel ++ "\n" ++
          "popq %rbx\n" ++
          "pushq %rax\n"
        , argSize + 3
        )
    }
  | expr '+' expr
    { \offset -> do
      (arg1, arg1Size) <- $1 offset
      (arg2, arg2Size) <- $3 (offset + arg1Size)
      return
        ( arg1 ++
          arg2 ++
          "popq %rbx\n" ++
          "popq %rax\n" ++
          "addq %rbx, %rax\n" ++
          "pushq %rax\n"
        , arg1Size + arg2Size + 4
        )
    }
  | expr '-' expr
    { \offset -> do
      (arg1, arg1Size) <- $1 offset
      (arg2, arg2Size) <- $3 (offset + arg1Size)
      return
        ( arg1 ++
          arg2 ++
          "popq %rbx\n" ++
          "popq %rax\n" ++
          "subq %rbx, %rax\n" ++
          "pushq %rax\n"
        , arg1Size + arg2Size + 4
        )
    }
  | expr '<=' expr
    { \offset -> do
      (arg1, arg1Size) <- $1 offset
      (arg2, arg2Size) <- $3 (offset + arg1Size)
      trueLabel <- nextLabel (offset + arg1Size + arg2Size + 7)
      endLabel <- nextLabel (offset + arg1Size + arg2Size + 9)
      return
        ( arg1 ++
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
        , arg1Size + arg2Size + 9
        )
    }
  | 'if' '(' expr ')' '{' expr '}' 'else' '{' expr '}'
    { \offset -> do
      (cond, condSize) <- $3 offset
      (positive, positiveSize) <- $6 (offset + condSize + 3)
      (negative, negativeSize) <- $10 (offset + condSize + 3 + positiveSize + 2)
      elseLabel <- nextLabel (offset + condSize + 3 + positiveSize + 2)
      endLabel <- nextLabel (offset + condSize + 3 + positiveSize + 2 + negativeSize + 1)
      return
        ( cond ++
          "popq %rax\n" ++
          "cmpq $0, %rax\n" ++
          "jz " ++ elseLabel ++ "\n" ++
          positive ++
          "jmp " ++ endLabel ++ "\n" ++
          elseLabel ++ ":\n" ++
          negative ++
          endLabel ++ ":\n"
        , condSize + 3 + positiveSize + 2 + negativeSize + 1
        )
    }

{

parseError :: [Token] -> a
parseError tks = error $ "parseError: " ++ show tks

main = do
  source <- getContents
  rec (result, parserState) <- runStateT (cheapParse (lexer source) 0) (initParserState (parserFuncTable parserState))
  putStr $ fst result

}
