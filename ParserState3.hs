module ParserState3
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
  , parserFinalFuncTable :: FuncTable
  }

type ParserT m a = StateT ParserState m a
type Parser a = ParserT IO a

initParserState finalFuncTable = ParserState
  { parserFuncTable = M.fromList [("input", "read"), ("output", "write")]
  , parserFinalFuncTable = finalFuncTable
  }

nextLabel :: Monad m => Int -> ParserT m [Char]
nextLabel offset = do
  return $ "line_" ++ show offset

newFunc :: Monad m => [Char] -> Int -> ParserT m [Char]
newFunc name offset = do
  oldState <- get
  let
    oldTable = parserFuncTable oldState

  if M.member name oldTable
    then
      fail $ "function " ++ name ++ " redefined"
    else do
      newLabel <- nextLabel offset
      oldState' <- get
      let
        newTable = M.insert name newLabel oldTable
        newState = oldState' {parserFuncTable = newTable}

      put newState
      return newLabel

lookupFunc :: Monad m => [Char] -> ParserT m [Char]
lookupFunc name = do
  state <- get
  return $ case M.lookup name (parserFinalFuncTable state) of
      Just label -> label
      Nothing -> "UnknownFunc" ++ name

