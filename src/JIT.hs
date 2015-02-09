module JIT where

import Foreign.Ptr ( FunPtr, castFunPtr )

import LLVM.General.Context
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager

import qualified LLVM.General.ExecutionEngine as EE
import Control.Monad.Trans.Except (ExceptT(..))

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

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
optimize modl = ExceptT $ withContext $ \context ->
  runExceptT $ withModuleFromAST context modl $ \m ->
    withPassManager passes $ \pm -> do
      -- Optimization pass
      _ <- runPassManager pm m
      putStrLn =<< moduleLLVMAssembly m
      -- Return the optimized mode
      moduleAST m

jit :: AST.Module -> ExceptT String IO AST.Module
jit modl = ExceptT $ withContext $ \context ->
  withEE context $ \executionEngine ->
    runExceptT $ withModuleFromAST context modl $ \m -> do
      {-withPassManager passes $ \pm -> do-}
        -- Optimization Pass
        {-_ <- runPassManager pm m-}
        {-optmod <- moduleAST m-}
        {-s <- moduleLLVMAssembly m-}
        {-putStrLn s-}
      EE.withModuleInEngine executionEngine m $ \ee -> do
        mainfn <- EE.getFunction ee (AST.Name "main")
        case mainfn of
          Just fn -> do
            res <- run fn
            putStrLn $ "Evaluated to: " ++ show res
          Nothing -> putStrLn "Didn't receive c function ptr from EE"

      return modl
