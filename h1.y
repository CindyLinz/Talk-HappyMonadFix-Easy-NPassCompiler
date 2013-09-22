{

module Main where

-- vim: filetype=haskell

import Data.Char
import qualified Data.Map as M
import Control.Monad.State

}

%name parser
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
    (id, cs') -> TokenIdentity id : lexer cs'

lexDigit cs = TokenNum (read num) : lexer cs'
  where (num, cs') = span isDigit cs

data ParserState = ParserState
  { parserVarTable :: M.Map [Char] Int
  , parserFuncTable :: M.Map [Char] [Char]
  , parserLabelCursor :: Int
  , parserOffsetCursor :: Int
  }

type ParserT m a = StateT ParserState m a
type Parser a = ParserT IO a

nextLabel :: Monad m => ParserT m [Char]
nextLabel = do
  oldState <- get
  let
    oldLabel = parserLabelCursor oldState
    newLabel = oldLabel + 1
    newState = oldState { parserLabelCursor = newLabel }
  put newState
  return $ "label_" ++ show newLabel

nextOffset :: Monad m => ParserT m Int
nextOffset = do
  oldState <- get
  let
    oldOffset = parserOffsetCursor oldState
    newOffset = oldOffset + 8
    newState = oldState { parserOffsetCursor = newOffset }
  put newState
  return newOffset

newFunc :: Monad m => [Char] -> ParserT m [Char]
newFunc name = do
  oldState <- get
  let
    oldTable = parserFuncTable oldState

  if M.member name oldTable
    then
      fail $ "function " ++ name ++ " redefined"
    else do
      newLabel <- nextLabel
      let
        newTable = M.insert name newLabel oldTable
        newState = oldState {parserFuncTable = newTable}

      put newState
      return newLabel

lookupFunc :: Monad m => [Char] -> ParserT m [Char]
lookupFunc name = do
  state <- get
  case M.lookup name (parserFuncTable state) of
    Just label -> return label
    Nothing -> fail $ "Unknown function " ++ name

localVar :: Monad m => ParserT m a -> ParserT m a
localVar m = do
  oldState <- get
  put $ oldState {parserVarTable = M.empty}

  res <- m

  newState <- get
  put $ newState {parserVarTable = parserVarTable oldState, parserOffsetCursor = 0}

  return res

newVar :: Monad m => [Char] -> ParserT m Int
newVar name = do
  oldState <- get
  let
    oldTable = parserVarTable oldState

  if M.member name oldTable
    then
      fail $ "variable " ++ name ++ " redefined"
    else do
      newOffset <- nextOffset
      let
        newTable = M.insert name newOffset oldTable
        newState = oldState {parserVarTable = newTable}

      put newState
      return newOffset

lookupVar :: Monad m => [Char] -> ParserT m Int
lookupVar name = do
  state <- get
  case M.lookup name (parserVarTable state) of
    Just offset -> return offset
    Nothing -> fail $ "Unknown variable " ++ name

code :: MonadIO m => [Char] -> ParserT m ()
code str = liftIO (putStr str)

parseError :: Monad m => [Token] -> ParserT m a
parseError = fail "parseError"


main = do
  putStrLn "Hi"

}
