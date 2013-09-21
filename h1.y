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

code :: { () }
  : expr
    {% do
      code "_start:\n"
      -- $1
      return ()
    }
  | func code
    {% do
      -- $1
      -- $2
      return ()
    }

func :: { () }
  : 'function' I_IDENTITY '(' I_IDENTITY ')' '{' expr '}'
    {% do
      funcLabel <- newFunc $2
      code funcLabel; code ":\n"
      localVar $ do
        newVar $4
        -- $7
        code "popq %rax\n"
    }

expr :: { () }
  : I_IDENTITY
    {% do
      varOffset <- lookupVar $1
      code "movq "; code (show varOffset); code "(%rbp), %rax\n"
      code "pushq %rax\n"
    }
  | I_NUMBER
    {% do
      code "pushq $"; code (show $1); code "\n"
    }
  | I_IDENTITY '(' expr ')'
    {% do
      funcName <- lookupFunc $1
      -- $3
      code "call "; code funcName; code "\n"
      code "popq %rbx\n"
      code "pushq %rax\n"
    }
  | expr '+' expr
    {% do
      -- $1
      -- $3
      code "popq %rbx\n"
      code "popq %rax\n"
      code "addq %rbx, %rax\n"
      code "pushq %rax\n"
    }
  | expr '-' expr
    {% do
      -- $1
      -- $3
      code "popq %rbx\n"
      code "popq %rax\n"
      code "subq %rbx, %rax\n"
      code "pushq %rax\n"
    }
  | expr '<=' expr
    {% do
      -- $1
      -- $3
      trueLabel <- nextLabel
      endLabel <- nextLabel
      code "popq %rbx\n"
      code "popq %rax\n"
      code "cmpq $rbx, $rax\n"
      code "jbe "; code trueLabel; code "\n"
      code "pushq $0\n"
      code "jmp "; code endLabel; code "\n"
      code trueLabel; code ":\n"
      code "pushq $1\n"
      code endLabel; code ":\n"
    }
  | 'if' '(' expr ')' '{' expr '}' 'else' '{' expr '}'
    {% do
      elseLabel <- nextLabel
      endLabel <- nextLabel
      -- $3
      code "popq %rax\n"
      code "cmpq $0, %rax\n"
      code "jz "; code elseLabel; code "\n"
      -- $6
      code "jmp "; code endLabel; code "\n"
      code elseLabel; code ":\n"
      -- $10
      code endLabel; code ":\n"
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
