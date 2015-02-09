module JIT where

import Foreign.Ptr ( FunPtr, castFunPtr )

import LLVM.General.Context
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager

import qualified LLVM.General.ExecutionEngine as EE
import Control.Monad.Trans.Except (runExceptT)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT modl = withContext $ \context ->
  jit context $ \executionEngine ->
    runExceptT $ withModuleFromAST context modl $ \m ->
      withPassManager passes $ \pm -> do
        -- Optimization Pass
        _ <- runPassManager pm m
        optmod <- moduleAST m
        s <- moduleLLVMAssembly m
        putStrLn s

        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              res <- run fn
              putStrLn $ "Evaluated to: " ++ show res
            Nothing -> return ()

        -- Return the optimized module
        return optmod
