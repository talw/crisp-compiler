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
import LLVM.General.AST (Instruction(GetElementPtr), moduleDefinitions)
import LLVM.General (moduleLLVMAssembly, withModuleFromAST)
import LLVM.General.Context (withContext)
import LLVM.General.Module
import LLVM.General.Diagnostic (Diagnostic(..))
import LLVM.General.Target (withDefaultTargetMachine)

import Data.Traversable
import Data.Functor ((<$>))
import Data.List (sort, delete)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Char (ord)
import Data.Bool (bool)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
import Control.Monad
import Control.Monad.State (modify, gets, get, put)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Error (ErrorT(..))
import Control.Monad.State.Strict (StateT(..), MonadState)
import Control.Monad.Reader (ReaderT(..), MonadReader, ask)
import System.Process

import Codegen
import qualified Codegen as CG
import Syntax
import JIT
import Options
import Immediates

import Paths_lc_hs (getDataDir)
import System.FilePath ((</>))
import Text.Printf (printf)
import Utils (readBinary)

type CrispComputation a = ReaderT CompilerOptions (StateT CompilerState (ErrorT String IO)) a

runCrispComputation :: CrispComputation a -> CompilerOptions -> CompilerState
  -> IO (Either String (a, CompilerState))
runCrispComputation cc opts crispMod =
  runErrorT (runStateT (runReaderT cc opts) crispMod)

liftErrorT :: ErrorT String IO a -> CrispComputation a
liftErrorT = lift . lift

data CompilerState = CompilerState
  { astModule :: AST.Module
  , defExprs :: [Expr]
  }

emptyModule :: CompilerState
emptyModule = CompilerState AST.defaultModule []

-------------------------------------------------------------------------------
-- Compilation to LLVM
-------------------------------------------------------------------------------

printLLVMasm :: AST.Module -> ExceptT String IO AST.Module
printLLVMasm modl = ExceptT $ withContext $ \context ->
  runExceptT $ withModuleFromAST context modl $ \m -> do
    putStrLn =<< moduleLLVMAssembly m
    return modl

codegen :: CompilerOptions -> AST.Module -> [Expr] -> [Expr] -> IO AST.Module
codegen CompilerOptions{..} modl nonDefExprs defExprs = do
  res <- runExceptT $ process preOptiAst
  case res of
    Right newAst -> return newAst
    Left err     -> putStrLn err >> return preOptiAst
 where
  preOptiAst = runLLVM modl deltaModl
  process = bool return printLLVMasm optPrintLLVM
        >=> optimize
        >=> bool return jit optReplMode

  deltaModl = delPrevMain >> codegenTop nonDefExprs defExprs
  delPrevMain = delFunc entryFuncName

codegenTop :: [Expr] -> [Expr] -> LLVM ()
codegenTop nonDefExprs defExprs  = do
  processDefinitons
  processExpressions
 where
  globalVars = flip map defExprs $ \(DefExp name _) -> name
  processExpressions =
    codegenFunction entryFuncName [] bodyPrelude [] globalVars nonDefExprs
   where bodyPrelude = call (funcOpr uint (AST.Name initGlobalsFuncName) []) []

  processDefinitons = do
    traverse processDefiniton defExprs

    delFunc initGlobalsFuncName
    codegenFunction initGlobalsFuncName [] (return ()) [] globalVars defExprs
   where
    processDefiniton (DefExp name expr) =
      codegenGlobalVar name

codegenGlobalVar :: SymName -> LLVM()
codegenGlobalVar = defineGlobalVar

codegenFunction :: SymName -> [AST.Type] -> Codegen a
                -> [SymName] -> [SymName] -> [Expr] -> LLVM ()
codegenFunction funcName argTys prologue args globalVars exprs = do
  defineFunc uint funcName fnargs blks
  sequence_ extraFuncsComputations
 where
  fnargs = zip argTys $ map AST.Name args
  cgst = execCodegen funcName globalVars $ do
    blk <- addBlock entryBlockName
    setBlock blk
    for args $ \a -> do
      var <- alloca uint
      store var (local (AST.Name a))
      assign a var

    prologue
    res <- cgenComputation
    ret res

  cgenComputation = do
    resList <- traverse codegenExpr exprs
    return $ if null resList
               then constUint nilValue
               else last resList
  blks = createBlocks cgst
  extraFuncsComputations = extraFuncs cgst

codegenType :: SymName -> AST.Type -> LLVM ()
codegenType = defineType

codegenExterns :: LLVM ()
codegenExterns =
  external uint "memalign" [(AST.IntegerType 64, AST.Name "alignment")
                           ,(AST.IntegerType 64, AST.Name "size") ]

-------------------------------------------------------------------------------
-- Translation of Expr values into llvm IR
-------------------------------------------------------------------------------

codegenExpr :: Expr -> Codegen AST.Operand

codegenExpr (BoolExp True) = return . constUint $ trueValue
codegenExpr (BoolExp False) = return . constUint $ falseValue
codegenExpr (NumberExp n) = return . constUint . toFixnum $ n
codegenExpr (CharExp c) = return . constUint . toChar $ c
codegenExpr EmptyExp = return . constUint $ nilValue

codegenExpr (StringExp str) = do
  vecPtr <- memalignRaw $ uintSizeBytes + strLen
  vecPtrC <- inttoptr vecPtr $ ptr uint
  store vecPtrC $ constUint strLen


  bytePtr <- flip bitcast i8ptr =<< getelementptrRaw vecPtrC [1]
  for (zip [0..] str) $ \(i, char) -> do
    let opr = constUintSize 8 $ ord char
    targetPtr <- getelementptrRaw bytePtr [i]
    store targetPtr opr

  iadd vecPtr $ constUint $ readBinary stringFormat
 where
  strLen = length str

codegenExpr (ArrayExp exprs) = do
  vecPtr <- memalign $ exprCount + 1
  vecPtrC <- inttoptr vecPtr $ ptr uint
  store vecPtrC $ constUint exprCount

  for (zip [1..] exprs) $ \(i, expr) -> do
    opr <- codegenExpr expr
    targetPtr <- getelementptrRaw vecPtrC [i]
    store targetPtr opr

  iadd vecPtr $ constUint $ readBinary vectorFormat
 where
  exprCount = length exprs

codegenExpr (BinOpExp op a b) = do
  ca <- codegenExpr a
  cb <- codegenExpr b
  asIRbinOp op ca cb

codegenExpr (VarExp varName) =
  maybe planB load =<< getvar varName
 where
  planB = load $ extern (AST.Name varName)

codegenExpr (DefExp defName expr) = do
  gvs <- gets globalVars
  modify $ \s -> s { globalVars = defName : gvs }
  codegenExpr (SetExp defName expr)

codegenExpr (SetExp symName expr) = do
  mVarPtr <- getvar symName
  let ptr = fromMaybe (extern $ AST.Name symName) mVarPtr
  store ptr =<< codegenExpr expr
  return $ constUint nilValue

codegenExpr (PrimCallExp primName args) = do
  operands <- traverse codegenExpr args
  call (extern $ AST.Name primName) operands

codegenExpr (CallExp func args) = do
  funcEnvPtr <- codegenExpr func
  funcEnvPtrC <- inttoptr funcEnvPtr $ ptr $ structType [uint, uint]
  envPtrPtr <- getelementptr funcEnvPtrC 0
  envPtr <- load envPtrPtr
  funcPtrPtr <- getelementptr funcEnvPtrC 1
  funcPtr <- load funcPtrPtr
  funcPtrC <- inttoptr funcPtr $
     ptr $ AST.FunctionType uint (argsTypeList $ length args + 1) False

  operands <- traverse codegenExpr args
  call funcPtrC $ envPtr : operands

codegenExpr fe@(FuncExp vars body) = do
  cgst <- get

  let
    (lambdaName, supply) =
      uniqueName (funcName cgst ++ suffLambda) $ names cgst
    freeVars = sort $ findFreeVars (globalVars cgst ++ vars) body
    est = envStructType freeVars
    atl = argsTypeList $ length vars + 1

    createFuncComputation =
      codegenFunction
        lambdaName atl prologue (envVarName : vars) (globalVars cgst) body
    envPtr =
      AST.LocalReference uint
      $ AST.Name envVarName
    prologue = do
      envPtrC <- inttoptr envPtr $ ptr est
      for (zip [0..] freeVars) $ \(ix,freeVar) -> do
        heapPtrPtr <- getelementptr envPtrC ix
        heapPtrPtrC <- inttoptr heapPtrPtr $ ptr uint
        heapPtr <- load heapPtrPtrC
        heapPtrC <- inttoptr heapPtr $ ptr uint
        assign freeVar heapPtrC

  --Adding llvm computations to add the lambda function and env struct
  --as globals in the llvm module
  modify $ \cgst -> cgst
    { extraFuncs = createFuncComputation
                 {-: createTypeComputation-}
                 : extraFuncs cgst
    , names = supply
    }

  --Setting up the operand to return
  --returnedOpr <- malloc 2
  returnedOpr <- memalign 2
  returnedOprC <- inttoptr returnedOpr $ ptr $ structType [uint, uint]

  --Instantiating an env struct and filling it
  --envPtr <- malloc $ length freeVars
  envPtr <- memalign $ length freeVars
  envPtrC <- inttoptr envPtr $ ptr est
  for (zip [0..] freeVars) $ \(ix,freeVar) -> do
    fvPtr <- flip liftM (getvar freeVar)
               $ fromMaybe
                   (error "bug - freevar filling")
    fvVal <- load fvPtr
    heapPtr <- memalign 1
    heapPtrC <- inttoptr heapPtr $ ptr uint
    store heapPtrC fvVal
    assign freeVar heapPtrC
    setElemPtr envPtrC ix heapPtr

  setElemPtr returnedOprC 0 envPtr

  funcOprC <- ptrtoint (funcOpr uint (AST.Name lambdaName) atl) uint
  setElemPtr returnedOprC 1 funcOprC

  return returnedOpr

codegenExpr (IfExp cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cond <- codegenExpr cond
  test <- icmp IP.NE falseOpr cond
  cbr test ifthen ifelse -- Branch based on the condition

  (trval, ifthen) <- thenBr ifthen ifexit
  (flval, ifelse) <- elseBr ifelse ifexit

  setBlock ifexit
  phi uint [(trval, ifthen), (flval, ifelse)]
 where
  falseOpr = constUint falseValue
  thenBr ifthen ifexit = do
    setBlock ifthen
    trval <- codegenExpr tr
    br ifexit
    ifthen <- getBlock
    return (trval, ifthen)
  elseBr ifelse ifexit = do
    setBlock ifelse
    flval <- codegenExpr fl
    br ifexit
    ifelse <- getBlock
    return (flval, ifelse)

codegenExpr _ = error "codegenExpr called with unexpected Expr"

-------------------------------------------------------------------------------
-- Composite Types
-------------------------------------------------------------------------------

emptyStructType :: AST.Type
emptyStructType = AST.StructureType True []

structType :: [AST.Type] -> AST.Type
structType tys = AST.StructureType True tys

envStructType :: [SymName] -> AST.Type
envStructType freeVars =
  AST.StructureType True . argsTypeList $ length freeVars

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

memalignRaw :: Int -> Codegen AST.Operand
memalignRaw  sizeInBytes =
  call (funcOpr uint (AST.Name "memalign") $ replicate 2 $ AST.IntegerType 64)
    $ map (constUintSize 64) [1, sizeInBytes]

memalign :: Int -> Codegen AST.Operand
memalign sizeInWords = memalignRaw $ sizeInWords * uintSizeBytes

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

-------------------------------------------------------------------------------
-- Linking LLVM modules
-------------------------------------------------------------------------------

linkModule :: File -> AST.Module -> ErrorT String IO AST.Module
linkModule fp modlAST = ErrorT $ withContext $ \context -> do
  result <- runExceptT . withModuleFromLLVMAssembly context fp $
    \modToLink -> (join <$>) . runExceptT . withModuleFromAST context modlAST $
      \modl -> runExceptT $ do
        linkModules False modl modToLink
        liftIO $ moduleAST modl
  return $ either (Left . show) id result

-------------------------------------------------------------------------------
-- Compilation to machine code
-------------------------------------------------------------------------------

writeTargetCode :: CrispComputation ()
writeTargetCode = do
  CompilerOptions{..} <- ask
  let objfn = optInputFilePath ++ ".o"
  writeObjFile objfn
  liftIO $ callProcess "gcc" ["-lm", objfn, "-o", optOutputFilePath]
 where
  writeObjFile objfn = do
    astMod <- gets astModule
    liftErrorT . ErrorT $
      withContext $ \context ->
        fmap join . runExceptT . withModuleFromAST context astMod $ \mod ->
          fmap join . runExceptT . withDefaultTargetMachine $ \target ->
            runExceptT $ writeObjectToFile target (File objfn) mod

-------------------------------------------------------------------------------
-- Initial AST module
-------------------------------------------------------------------------------

-- | The initial module comes with external declarations, and is linked
-- with primitive functions and constants defined in C.
-- If it is used for compilation
-- (and not for the initial module of a REPL session) the C driver is
-- linked as well.
initModule :: Bool -> String -> CrispComputation ()
initModule linkDriver label = do
  initialASTmod <- getLinkedASTmod
  put $ CompilerState initialASTmod []
 where
  initialASTmod = runLLVM
                    (AST.defaultModule { AST.moduleName = label })
                    codegenExterns
  getLinkedASTmod = liftErrorT $ do
    dataDir <- liftIO getDataDir
    let preCompModDir = dataDir </> "precompiled-modules"
        primModFilePath = File $ preCompModDir </> "primitives.ll"
        driverModFilePath = File $ preCompModDir </> "driver.ll"
        constsModFilePath = File $ preCompModDir </> "constants.ll"
    linkModule primModFilePath
      >=> linkModule constsModFilePath
      >=> (if linkDriver then linkModule driverModFilePath else return)
      $ initialASTmod

-------------------------------------------------------------------------------
-- Helper functions for emitting
-------------------------------------------------------------------------------

argsTypeList :: Int -> [AST.Type]
argsTypeList n = replicate n uint

setElemPtr :: AST.Operand -> Int -> AST.Operand -> Codegen AST.Operand
setElemPtr struct ix item =
  getelementptr struct ix >>= flip store item

