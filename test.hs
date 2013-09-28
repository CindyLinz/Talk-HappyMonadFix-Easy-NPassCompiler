module Main where

import System.Environment
import System.Posix.Files
import System.Process (system)
import System.Exit (ExitCode (..))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

newer :: (Monad m, MonadIO m) => FilePath -> [FilePath] -> m ()
newer a bs = do
  aExist <- liftIO $ fileExist a
  unless aExist (fail "")

  aMTime <- (liftIO $ getFileStatus a) >>= return . modificationTime
  forM_ bs
    (\b -> do
      bExist <- liftIO $ fileExist b
      when bExist $ do
        bMTime <- (liftIO $ getFileStatus b) >>= return . modificationTime
        when (aMTime < bMTime) (fail "")
    )

needThen :: (MonadIO m, Monad m) => String -> FilePath -> [FilePath] -> m ()
needThen command target deps = do
  maybeIgnore <- runMaybeT (newer target deps)
  if maybeIgnore == Nothing
    then
      alwaysThen command
    else do
      liftIO $ putStrLn $ "skip: " ++ command

alwaysThen :: (MonadIO m, Monad m) => String -> m ()
alwaysThen command = do
  liftIO $ putStrLn $ "execute: " ++ command
  exitCode <- liftIO $ system command
  unless (exitCode == ExitSuccess) (fail "exec fail")

main = do
  [compilerName, targetName] <- getArgs

  maybeResult <- runMaybeT $ do
    needThen ("happy -i -o " ++ compilerName ++ ".hs " ++ compilerName ++ ".y") (compilerName ++ ".hs") [compilerName ++ ".y"]
    needThen
      ("ghc --make " ++ compilerName)
      compilerName
      [ compilerName ++ ".hs"
      , "Template.hs"
      , "Lexer.hs"
      , "ParserState.hs"
      , "ParserState2.hs"
      , "ParserState3.hs"
      ]
    alwaysThen ("./" ++ compilerName ++ " < " ++ targetName ++ ".code > " ++ targetName ++ ".s")
    needThen ("as -o " ++ targetName ++ ".o " ++ targetName ++ ".s") (targetName ++ ".o") [targetName ++ ".s"]
    needThen ("ld -o " ++ targetName ++ " " ++ targetName ++ ".o") targetName [targetName ++ ".o"]

  if maybeResult == Nothing
    then
      putStrLn "Compile failed."
    else do
      putStrLn "Compile success. Run the test..."
      void $ runMaybeT $ alwaysThen ("./" ++ targetName)

