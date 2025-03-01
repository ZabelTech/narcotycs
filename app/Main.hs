module Main where

import qualified Naive.Parser
import qualified Naive.Checker
import Control.Monad
import System.Environment
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

reportParsingError (Right res) = return $ Just  res
reportParsingError (Left err)  = do
  print $ "Parsing Error: " ++ show err
  return Nothing

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \file -> runMaybeT $ do
    ast      <- MaybeT $ reportParsingError =<< Naive.Parser.parseFile file
    problems <- return $ Naive.Checker.checkCode ast
    lift $ do
      putStrLn "Constraint Problems: "
      mapM_ print problems
