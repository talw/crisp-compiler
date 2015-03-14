{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverlappingInstances #-}

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
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Control.Monad.Trans (MonadTrans(..), MonadIO)
import Control.Monad.Trans.Error (ErrorT(..))
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.State (get, put)
import Control.Monad.State.Strict (StateT(..), MonadState)
import Control.Monad.Reader (ReaderT(..), MonadReader, ask)

main :: IO ()
main = do
  args <- getArgs
  opts <- getOptions args
  either putStrLn (const $ return ()) =<< runCrispComputation cc opts emptyModule
 where
  cc = do
    CompilerOptions{..} <- ask
    initModule (not optReplMode) "default module"
    if optReplMode
      then repl
      else compile

compile :: CrispComputation ()
compile = do
  processFile
  writeTargetCode
  liftIO $ putStrLn "Compiled successfully."

repl :: CrispComputation ()
repl = do
  opts <- ask
  runInputT defaultSettings loop
 where
  loop = do
    modl <- lift get
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        lift $ catchError
          (process input)
          (liftIO . putStrLn)
        loop

processFile :: CrispComputation ()
processFile = do
  CompilerOptions{..} <- ask
  liftIO (readFile optInputFilePath) >>= process

process :: String -> CrispComputation ()
process source = do
  opts <- ask
  modl <- get

  exprs <- parseComputation
  let defExprs'    = filter isDefinition exprs ++ defExprs modl
      nonDefExprs = filter (not . isDefinition) exprs

  updatedAstMod <- liftIO $ codegen opts (astModule modl) nonDefExprs defExprs'
  put $ CrispModule updatedAstMod defExprs'
 where
  isDefinition (DefExp {}) = True
  isDefinition _           = False
  parseComputation = liftErrorT . ErrorT . return $ parseExpr source

