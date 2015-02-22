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
import LLVM.General.AST.Type (ptr)
import LLVM.General (moduleLLVMAssembly, withModuleFromAST)
import LLVM.General.Context (withContext)
import LLVM.General.Module
import LLVM.General.Diagnostic (Diagnostic(..))
import LLVM.General.Target (withDefaultTargetMachine)

import Data.Traversable
import Data.Functor ((<$>))
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.State (modify, gets)
import Control.Monad.IO.Class (liftIO)

import Codegen
import qualified Codegen as CG
import Syntax
import JIT
import Immediates hiding (false, true)
import qualified Immediates as IM

import Paths_lc_hs (getDataDir)
import System.FilePath ((</>))

false :: AST.Operand
false = constOpr . C.Int uintSize . fromIntegral $ IM.false

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (uint, AST.Name x))

delPrevMain :: LLVM ()
delPrevMain = do
  md <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = filter filt md }
 where
  filt (AST.GlobalDefinition
    (AST.Function { G.name = AST.Name "entryFunc", .. })) = False
  filt _ = True

codegenTop :: Expr -> LLVM ()
codegenTop (DefExp name (FuncExp args body)) =
  define uint name fnargs bls
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

codegenTop expr = define uint "entryFunc" [] blks
 where
  blks = createBlocks $ execCodegen $ do
    blk <- addBlock entryBlockName
    setBlock blk
    cgen expr >>= ret

codegenExterns :: LLVM ()
codegenExterns {-(Extern name args)-} = external (ptr $ AST.IntegerType 8) "malloc" fnargs
 where fnargs = toSig ["size"]

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

comp :: IP.IntegerPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
comp ip a b = do
  res        <- zext uint =<< icmp ip a b
  resShifted <- shl res . constUint $ shiftWidthOfFormat boolFormat
  CG.or resShifted . constUint $ formatMasked boolFormat

binArithOp :: (AST.Operand -> AST.Operand -> Codegen AST.Operand)
           -> AST.Operand -> AST.Operand -> Codegen AST.Operand
binArithOp op a b = do
  a' <- shr a . constUint $ shiftWidthOfFormat fixnumFormat
  b' <- shr b . constUint $ shiftWidthOfFormat fixnumFormat
  res <- op a' b'
  resShifted <- shl res . constUint $ shiftWidthOfFormat fixnumFormat
  CG.or resShifted . constUint $ formatMasked fixnumFormat

asIRbinOp :: BinOp -> AST.Operand -> AST.Operand -> Codegen AST.Operand
asIRbinOp Add = binArithOp iadd
asIRbinOp Sub = binArithOp isub
asIRbinOp Mul = binArithOp imul
asIRbinOp Div = binArithOp idiv
asIRbinOp Lt  = comp IP.ULT
asIRbinOp Lte  = comp IP.ULE
asIRbinOp Gt   = comp IP.UGT
asIRbinOp Gte  = comp IP.UGE
asIRbinOp Eq   = comp IP.EQ

cgen :: Expr -> Codegen AST.Operand

cgen (VarExp x) = getvar x >>= load

cgen (BoolExp True) = return . constUint $ IM.true
cgen (BoolExp False) = return . constUint $ IM.false

cgen (NumberExp n) = return . constUint . toFixnum $ n

cgen (CharExp c) = return . constUint . toChar $ c

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

initModule :: String -> IO (Either String AST.Module)
initModule label = withContext $ \context -> do
  dataDir <- getDataDir
  let primModFilePath = File $ dataDir </> "c-src/try.ll"
      driverModFilePath = File $ dataDir </> "c-src/driver.ll"
  runExceptT $
    linkModule primModFilePath >=> linkModule driverModFilePath
    $ initialModAST
 where
  initialModAST = runLLVM
                    (AST.defaultModule { AST.moduleName = label })
                    codegenExterns

linkModule :: File -> AST.Module -> ExceptT String IO AST.Module
linkModule fp modlAST = ExceptT $ withContext $ \context -> do
  result <- runExceptT . withModuleFromLLVMAssembly context fp $
    \modToLink -> (join <$>) . runExceptT . withModuleFromAST context modlAST $
      \modl -> runExceptT $ do
        linkModules False modl modToLink
        liftIO $ moduleAST modl
  return $ either (Left . show) id result

writeTargetCode :: FilePath -> AST.Module -> ExceptT String IO ()
writeTargetCode fn astMod = ExceptT $
  withContext $ \context ->
    fmap join . runExceptT . withModuleFromAST context astMod $ \mod ->
      fmap join . runExceptT . withDefaultTargetMachine $ \target ->
        runExceptT $ writeObjectToFile target (File $ fn ++ ".o") mod

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
