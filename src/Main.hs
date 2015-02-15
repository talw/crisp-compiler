module Main where

import Parser
import Emit
{-import Codegen-}

import System.Console.Haskeline
import System.Environment (getArgs)

import qualified LLVM.General.AST as AST
import Data.Functor (void)
import Control.Monad.IO.Class (liftIO)


processFile :: FilePath -> AST.Module -> IO (Maybe AST.Module)
processFile fname initMod = readFile fname >>= process initMod

repl :: AST.Module -> IO ()
repl initMod = runInputT defaultSettings . loop $ initMod
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
  emod <- initModule "default module"
  case emod of
    Left err -> putStrLn err
    Right mod ->
      case args of
        []      -> repl mod
        fname : _ -> void $ processFile fname mod

{-process :: AST.Module -> String -> IO (Maybe AST.Module)-}
{-process modl source = do-}
  {-let ast = parseExpr source-}
  {-case ast of-}
    {-Left err -> print err >> return Nothing-}
    {-Right ex -> do-}
      {-ast <- codegen modo ex-}
      {-return $ Just ast-}

