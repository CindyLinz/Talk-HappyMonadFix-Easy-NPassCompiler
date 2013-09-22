module ParserState
  ( Parser
  , ParserT
  , ParserState
  , initParserState
  , nextLabel
  , newFunc
  , lookupFunc
  , newVar
  , lookupVar
  , localVar
  ) where

import qualified Data.Map as M
import Control.Monad.State
import Lexer (Token)

data ParserState = ParserState
  { parserVarTable :: M.Map [Char] Int
  , parserFuncTable :: M.Map [Char] [Char]
  , parserLabelCursor :: Int
  , parserOffsetCursor :: Int
  }

type ParserT m a = StateT ParserState m a
type Parser a = ParserT IO a

initParserState = ParserState
  { parserVarTable = M.empty
  , parserFuncTable = M.fromList [("input", "read"), ("output", "write")]
  , parserLabelCursor = 0
  , parserOffsetCursor = 0
  }

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


