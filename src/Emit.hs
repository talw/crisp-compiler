{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Emit where

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP
import LLVM.General (moduleLLVMAssembly, withModuleFromAST)
import LLVM.General.Context (withContext)

import Data.Traversable
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.State (modify, gets)

import Codegen
import Syntax
import JIT

false :: AST.Operand
false = constOpr $ C.Float (F.Double 0.0)
--TODO convert to the uint32 that represents 'false' boolean

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (uint, AST.Name x))

delPrevMain :: LLVM ()
delPrevMain = do
  md <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = filter filt md }
 where
  filt (AST.GlobalDefinition
    (AST.Function { G.name = AST.Name "main", .. })) = False
  filt _ = True

codegenTop :: Expr -> LLVM ()
codegenTop (DefExp name (FuncExp args body)) =
  define double name fnargs bls
 where
  fnargs = toSig args
  bls = createBlocks $ execCodegen $ do
    blk <- addBlock entryBlockName
    setBlock blk
    for args $ \a -> do
      var <- alloca uint
      store var (local (AST.Name a))
      assign a var
    cgen body >>= ret

codegenTop (Extern name args) = external uint name fnargs
 where fnargs = toSig args

codegenTop expr = define uint "main" [] blks
 where
  blks = createBlocks $ execCodegen $ do
    blk <- addBlock entryBlockName
    setBlock blk
    cgen expr >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = icmp IP.ULT a b

asIRbinOp :: BinOp -> AST.Operand -> AST.Operand -> Codegen AST.Operand
asIRbinOp Add = iadd
asIRbinOp Sub = isub
asIRbinOp Mul = imul
asIRbinOp Div = idiv
asIRbinOp Lt = lt

cgen :: Expr -> Codegen AST.Operand
cgen (VarExp x) = getvar x >>= load
cgen (NumberExp n) = return $ constOpr $ C.Int uintSize n
cgen (BinOpExp op a b) = do
  ca <- cgen a
  cb <- cgen b
  asIRbinOp op ca cb
cgen (CallExp (GlbVarExp name) args) = do
  operands <- traverse cgen args
  call (externf (AST.Name name)) operands
cgen (IfExp cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cond <- cgen cond
  test <- icmp IP.NE false cond
  cbr test ifthen ifelse -- Branch based on the condition

  (trval, ifthen) <- thenBr ifthen ifexit
  (flval, ifelse) <- elseBr ifelse ifexit

  setBlock ifexit
  phi uint [(trval, ifthen), (flval, ifelse)]
 where
  thenBr ifthen ifexit = do
    setBlock ifthen
    trval <- cgen tr
    br ifexit
    ifthen <- getBlock
    return (trval, ifthen)
  elseBr ifelse ifexit = do
    setBlock ifelse
    flval <- cgen fl
    br ifexit
    ifelse <- getBlock
    return (flval, ifelse)

cgen _ = error "cgen called with unexpected Expr"
--cgen (S.UnaryOp op a) = cgen $ S.Call ("unary" ++ op) [a]
--cgen (S.BinaryOp "=" (S.Var var) val) = do
  --a <- getvar var
  --cval <- cgen val
  --store a cval
  --return cval

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

printAsm :: AST.Module -> ExceptT String IO AST.Module
printAsm modl = ExceptT $ withContext $ \context ->
  runExceptT $ withModuleFromAST context modl $ \m -> do
    putStrLn =<< moduleLLVMAssembly m
    return modl

codegen :: AST.Module -> [Expr] -> IO AST.Module
codegen modl exprs = do
  let process = printAsm >=> optimize >=> jit

  res <- runExceptT $ process preOptiAst
  case res of
    Right newAst -> return newAst
    Left err     -> putStrLn err >> return preOptiAst
 where
  deltaModl  = delPrevMain >> traverse codegenTop exprs
  preOptiAst = runLLVM modl deltaModl
