module Main where

import Parser
import Emit
{-import Codegen-}

import System.Console.Haskeline
import System.Environment (getArgs)

import qualified LLVM.General.AST as AST
import Data.Functor (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))


{-processFile :: FilePath -> AST.Module -> IO (Either String AST.Module)-}
processFile :: FilePath -> AST.Module -> ExceptT String IO AST.Module
processFile fname initMod = ExceptT $ readFile fname >>= process initMod

repl :: AST.Module -> IO ()
repl initMod = runInputT defaultSettings . loop $ initMod
 where loop modl = do
         minput <- getInputLine "ready> "
         case minput of
           Nothing -> outputStrLn "Goodbye."
           Just input -> do
             mmodl' <- liftIO $ process modl input
             case mmodl' of
               Right modl' -> loop modl'
               Left err -> do
                 outputStrLn err
                 loop modl

process :: AST.Module -> String -> IO (Either String AST.Module)
process modl source = do
  let res = parseExpr source
  case res of
    Left err -> return . Left $ show err
    Right exprs -> do
      print exprs
      modl' <- codegen modl exprs
      return . Right $ modl'

main :: IO ()
main = do
  args <- getArgs
  emod <- initModule "default module"
  case emod of
    Left err -> putStrLn err
    Right initMod ->
      case args of
        []      -> repl initMod
        fname : _ -> compile fname initMod

compile :: FilePath -> AST.Module -> IO ()
compile fname astMod = do
  eRes <- runExceptT $ writeTargetCode fname =<< processFile fname astMod
  case eRes of
    Left err -> putStrLn err
    Right () -> putStrLn "Compiled successfully."



{-process :: AST.Module -> String -> IO (Maybe AST.Module)-}
{-process modl source = do-}
  {-let ast = parseExpr source-}
  {-case ast of-}
    {-Left err -> print err >> return Nothing-}
    {-Right ex -> do-}
      {-ast <- codegen modo ex-}
      {-return $ Just ast-}

