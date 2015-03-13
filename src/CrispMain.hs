{-# LANGUAGE RecordWildCards #-}

module CrispMain
  ( main
  ) where

import Parser
import Emit
import Options
import Syntax

import System.Console.Haskeline
import System.Environment (getArgs)

import qualified LLVM.General.AST as AST
import Data.Functor (void)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))

data CrispModule = CrispModule
  { astModule :: AST.Module
  , defExprs :: [Expr]
  }

processFile :: CompilerOptions -> CrispModule -> ExceptT String IO CrispModule
processFile opts@CompilerOptions{..} initMod = ExceptT $
  readFile optInputFilePath >>= process opts initMod

repl :: CompilerOptions -> CrispModule -> IO ()
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

process :: CompilerOptions -> CrispModule -> String
        -> IO (Either String CrispModule)
process opts@CompilerOptions{..} modl source =
  case parseExpr source of
    Left err -> return . Left $ show err
    Right exprs -> do
      print exprs
      let defExprs'    = filter isDefinition exprs ++ defExprs modl
          nonDefExprs = filter (not . isDefinition) exprs
      updatedAstMod <- codegen opts (astModule modl) nonDefExprs defExprs'
      return . Right $ CrispModule updatedAstMod defExprs'
 where
  isDefinition (DefExp {}) = True
  isDefinition _ = False


main :: IO ()
main = do
  args <- getArgs
  opts@CompilerOptions{..} <- getOptions args
  emodAction optReplMode $
    if optReplMode
      then repl opts
      else compile opts
 where
  emodAction isRepl action = do
    eAstMod <- initModule (not isRepl) "default module"
    let eCrispMod = eAstMod >>= \astMod -> return $ CrispModule astMod []
    either putStrLn action eCrispMod

compile :: CompilerOptions -> CrispModule -> IO ()
compile opts@CompilerOptions{..} modl = do
  eRes <- runExceptT $ do
    modl' <- processFile opts modl
    writeTargetCode opts $ astModule modl'
  case eRes of
    Left err -> putStrLn err
    Right () -> putStrLn "Compiled successfully."

