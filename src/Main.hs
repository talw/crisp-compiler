module Main where

import Parser
import Emit
import Codegen

import System.Console.Haskeline
import System.Environment (getArgs)

import qualified LLVM.General.AST as AST
import Data.Functor (void)
import Control.Monad.IO.Class (liftIO)


initModule :: AST.Module
initModule = emptyModule "Default initial module"

processFile :: FilePath -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
 where loop modl = do
         minput <- getInputLine "ready> "
         case minput of
           Nothing -> outputStrLn "Goodbye."
           Just input -> do
             mmodl' <- liftIO $ process modl input
             case mmodl' of
               Just modl' -> loop modl'
               Nothing -> do
                 outputStrLn "Failed compiling."
                 loop modl

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modl source = do
  let res = parseExpr source
  case res of
    Left err -> print err >> return Nothing
    Right exprs -> do
      print exprs
      modl' <- codegen modl exprs
      return $ Just modl'

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    fname : _ -> void $ processFile fname

{-process :: AST.Module -> String -> IO (Maybe AST.Module)-}
{-process modl source = do-}
  {-let ast = parseExpr source-}
  {-case ast of-}
    {-Left err -> print err >> return Nothing-}
    {-Right ex -> do-}
      {-ast <- codegen modo ex-}
      {-return $ Just ast-}

