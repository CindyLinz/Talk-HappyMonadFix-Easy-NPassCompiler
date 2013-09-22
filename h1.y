{

module Main where

-- vim: filetype=haskell

import Control.Monad.State

import Lexer
import ParserState

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

%left '<='
%left '+' '-'

%%

code :: { [Char] }
  : expr
    {%
      return $
        "_start:\n" ++
        $1
    }
  | func code
    {%
      return $
        $1 ++
        $2
    }

func :: { [Char] }
  : 'function' I_IDENTITY '(' I_IDENTITY ')' '{' expr '}'
    {% do
      funcLabel <- newFunc $2
      localVar $ do
        newVar $4
        return $
          funcLabel ++ ":\n" ++
          $7 ++
          "popq %rax\n"
    }

expr :: { [Char] }
  : I_IDENTITY
    {% do
      varOffset <- lookupVar $1
      return $
        "movq " ++ show varOffset ++ "(%rbp), %rax\n" ++
        "pushq %rax\n"
    }
  | I_NUMBER
    {%
      return $
        "pushq $" ++ show $1 ++ "\n"
    }
  | I_IDENTITY '(' expr ')'
    {% do
      funcName <- lookupFunc $1
      return $
        $3 ++
        "call " ++ funcName ++ "\n" ++
        "popq %rbx\n" ++
        "pushq %rax\n"
    }
  | expr '+' expr
    {%
      return $
        $1 ++
        $3 ++
        "popq %rbx\n" ++
        "popq %rax\n" ++
        "addq %rbx, %rax\n" ++
        "pushq %rax\n"
    }
  | expr '-' expr
    {%
      return $
        $1 ++
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
        "cmpq $rbx, $rax\n" ++
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
parseError = fail "parseError"

main = do
  putStrLn "Hi"

  source <- getContents
  result <- evalStateT (cheapParse (lexer source)) initParserState
  putStr result

}
