module Main where

import System.Environment
import System.Posix.Files
import System.Process (system)
import System.Exit (ExitCode (..))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

newer :: FilePath -> [FilePath] -> IO Bool
newer a bs = do
  aExist <- fileExist a
  if aExist
    then do
      aStatus <- getFileStatus a
      let
        aMTime = modificationTime aStatus
      foldM
        (\res b ->
          if res
            then do
              bExist <- fileExist b
              if bExist
                then do
                  bStatus <- getFileStatus b
                  return $ modificationTime bStatus <= aMTime
                else
                  return True
            else
              return res
        )
        True
        bs
    else
      return False

newerThen :: String -> FilePath -> [FilePath] -> IO (Maybe ())
newerThen command target deps = do
  ignore <- newer target deps
  if ignore
    then do
      putStrLn $ "skip: " ++ command
      return $ Just ()
    else
      alwaysThen command

alwaysThen :: String -> IO (Maybe ())
alwaysThen command = do
  putStrLn $ "execute: " ++ command
  exitCode <- system command
  return $ if exitCode == ExitSuccess
    then Just ()
    else Nothing

main = do
  [compilerName, targetName] <- getArgs

  maybeResult <- runMaybeT $ do
    lift $ newerThen ("happy -i -o " ++ compilerName ++ ".hs " ++ compilerName ++ ".y") (compilerName ++ ".hs") [compilerName ++ ".y"]
    lift $ newerThen
      ("ghc --make " ++ compilerName)
      compilerName
      [ compilerName ++ ".hs"
      , "Template.hs"
      , "Lexer.hs"
      , "ParserState.hs"
      , "ParserState2.hs"
      , "ParserState3.hs"
      ]
    lift $ alwaysThen ("./" ++ compilerName ++ " < " ++ targetName ++ ".code > " ++ targetName ++ ".s")
    lift $ newerThen ("as -o " ++ targetName ++ ".o " ++ targetName ++ ".s") (targetName ++ ".o") [targetName ++ ".s"]
    lift $ newerThen ("ld -o " ++ targetName ++ " " ++ targetName ++ ".o") targetName [targetName ++ ".o"]

  if maybeResult == Nothing
    then
      putStrLn "Compile failed."
    else do
      putStrLn "Compile success. Run the test..."
      _ <- runMaybeT $ lift $ alwaysThen ("./" ++ targetName)
      return ()

