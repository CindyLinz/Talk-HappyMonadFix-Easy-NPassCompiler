module ParserState
  ( Parser
  , ParserT
  , ParserState (..)
  , FuncTable
  , initParserState
  , nextLabel
  , newFunc
  , lookupFunc
  ) where

import qualified Data.Map as M
import Control.Monad.State
import Lexer (Token)

type FuncTable = M.Map [Char] [Char]
data ParserState = ParserState
  { parserFuncTable :: FuncTable
  , parserLabelCursor :: Int
  }

type ParserT m a = StateT ParserState m a
type Parser a = ParserT IO a

initParserState = ParserState
  { parserFuncTable = M.fromList [("input", "read"), ("output", "write")]
  , parserLabelCursor = 0
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
      oldState' <- get
      let
        newTable = M.insert name newLabel oldTable
        newState = oldState' {parserFuncTable = newTable}

      put newState
      return newLabel

lookupFunc :: Monad m => [Char] -> ParserT m [Char]
lookupFunc name = do
  state <- get
  case M.lookup name (parserFuncTable state) of
    Just label -> return label
    Nothing -> fail $ "Unknown function " ++ name

