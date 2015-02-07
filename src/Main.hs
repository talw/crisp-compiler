module Main where

import Parser
import Syntax

import System.Console.Haskeline
import System.Environment (getArgs)

import qualified LLVM.General.AST as AST
import Data.Functor (void)
import Control.Monad.IO.Class (liftIO)

processFile :: FilePath -> IO (Maybe Expr)
processFile fname = readFile fname >>= process

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        _ <- liftIO $ process input
        loop

process :: String -> IO (Maybe Expr)
process source = do
  let res = parseExpr source
  case res of
    Left err -> print err >> return Nothing
    Right ast -> print ast >> return (Just ast)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    fname : _ -> void $ processFile fname



{-initModule :: AST.Module-}
{-initModule = emptyModule "my cool jit"-}

{-process :: AST.Module -> String -> IO (Maybe AST.Module)-}
{-process modo source = do-}
  {-let res = parseToplevel source-}
  {-case res of-}
    {-Left err -> print err >> return Nothing-}
    {-Right ex -> do-}
      {-ast <- codegen modo ex-}
      {-return $ Just ast-}

