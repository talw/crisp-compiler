{-# LANGUAGE RecordWildCards #-}

module Main where

import Parser
import Emit
import Options

import System.Console.Haskeline
import System.Environment (getArgs)

import qualified LLVM.General.AST as AST
import Data.Functor (void)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))

processFile :: CompilerOptions -> AST.Module -> ExceptT String IO AST.Module
processFile opts@CompilerOptions{..} initMod = ExceptT $
  readFile optInputFilePath >>= process opts initMod

repl :: CompilerOptions -> AST.Module -> IO ()
repl opts@(CompilerOptions {..}) initMod =
  runInputT defaultSettings . loop $ initMod
 where loop modl = do
         minput <- getInputLine "ready> "
         case minput of
           Nothing -> outputStrLn "Goodbye."
           Just input -> do
             mmodl' <- liftIO $ process opts modl input
             case mmodl' of
               Right modl' -> loop modl'
               Left err -> do
                 outputStrLn err
                 loop modl

process :: CompilerOptions -> AST.Module -> String
        -> IO (Either String AST.Module)
process opts@CompilerOptions{..} modl source = do
  let res = parseExpr source
  case res of
    Left err -> return . Left $ show err
    Right exprs -> do
      print exprs
      modl' <- codegen opts modl exprs
      return . Right $ modl'

main :: IO ()
main = do
  args <- getArgs
  opts@CompilerOptions{..} <- getOptions args
  emodAction optReplMode $
    if optReplMode
      then repl opts
      else compile opts
  --case args of
    --[]      -> emodAction False repl
    --ifn : ofn : _ -> emodAction True $ compile optInputFilePath optOutputFilePath
 where
  emodAction isRepl action =
    either putStrLn action =<< initModule (not isRepl) "default module"

compile :: CompilerOptions -> AST.Module -> IO ()
compile opts@CompilerOptions{..} astMod = do
  eRes <- runExceptT $
    processFile opts astMod
    >>= writeTargetCode opts
  case eRes of
    Left err -> putStrLn err
    Right () -> putStrLn "Compiled successfully."

