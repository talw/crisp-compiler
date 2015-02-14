module JIT where

import Foreign.Ptr ( FunPtr, castFunPtr )
import Data.Word

import LLVM.General.Context
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager

import qualified LLVM.General.ExecutionEngine as EE
import Control.Monad.Trans.Except (ExceptT(..))

import Immediates (showImmediate)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Word32) -> IO Word32

run :: FunPtr a -> IO Word32
run fn = haskFun (castFunPtr fn :: FunPtr (IO Word32))

withEE :: Context -> (EE.MCJIT -> IO a) -> IO a
withEE  c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

optimize :: AST.Module -> ExceptT String IO AST.Module
optimize modl = ExceptT . withContext $ \context ->
  runExceptT . withModuleFromAST context modl $ \m ->
    withPassManager passes $ \pm -> do
      -- Optimization pass
      _ <- runPassManager pm m
      -- Return the optimized mode
      moduleAST m

-- For now the jit and optimizing after separate.
-- Even though it's less efficient, it's easier testing, that way.
jit :: AST.Module -> ExceptT String IO AST.Module
jit modl = ExceptT . withContext $ \context ->
  withEE context $ \executionEngine ->
    runExceptT . withModuleFromAST context modl $ \m -> do
      EE.withModuleInEngine executionEngine m $ \ee -> do
        mainfn <- EE.getFunction ee (AST.Name "main")
        case mainfn of
          Just fn -> do
            res <- run fn
            putStrLn $ "Evaluated to: " ++ showImmediate res
          Nothing -> putStrLn "no main function to evaluate"

      return modl
