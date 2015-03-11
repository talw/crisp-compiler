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
import LLVM.General.AST (Instruction(GetElementPtr))
import LLVM.General (moduleLLVMAssembly, withModuleFromAST)
import LLVM.General.Context (withContext)
import LLVM.General.Module
import LLVM.General.Diagnostic (Diagnostic(..))
import LLVM.General.Target (withDefaultTargetMachine)

import Data.Traversable
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe)
import Data.Char (ord)
import Data.Bool (bool)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.State (modify, gets, get)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import System.Process

import Codegen
import qualified Codegen as CG
import Syntax
import JIT
import Options
import Immediates hiding (false, true)
import qualified Immediates as IM

import Paths_lc_hs (getDataDir)
import System.FilePath ((</>))
import Text.Printf (printf)
import Utils (readBinary)

argsTypeList :: Int -> [AST.Type]
argsTypeList n = replicate n uint

false :: AST.Operand
false = constOpr . C.Int uintSize . fromIntegral $ IM.false

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (uint, AST.Name x))

setElemPtr :: AST.Operand -> Int -> AST.Operand -> Codegen AST.Operand
setElemPtr struct ix item =
  getelementptr struct ix >>= flip store item

delPrevMain :: LLVM ()
delPrevMain = do
  md <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = filter filt md }
 where
  filt (AST.GlobalDefinition
    (AST.Function { G.name = AST.Name funcName, .. }))
    | funcName == entryFuncName = False
  filt _ = True

codegenTop :: [Expr] -> LLVM ()
codegenTop exprs = do
  traverse processDefiniton $ filter needsDefining exprs
  processExpressions exprs
  {-processExpressions $ filter needsExpressing exprs-}
 where
  needsDefining (DefExp {}) = True
  needsDefining _ = False

  genFunc name argTys argNms exprs =
    codegenFunction name argTys (return ()) argNms exprs

  processDefiniton (DefExp name expr) =
    codegenGlobalVar name

  processExpressions exprs =
    genFunc entryFuncName [] [] exprs

codegenGlobalVar :: SymName -> LLVM()
codegenGlobalVar = defineGlobalVar

codegenFunction :: SymName -> [AST.Type] -> Codegen a
                -> [SymName] -> [Expr] -> LLVM ()
codegenFunction funcName argTys prologue args exprs = do
  defineFunc uint funcName fnargs blks
  sequence_ extraFuncsComputations
 where
  fnargs = zip argTys $ map AST.Name args
  cgst = execCodegen funcName $ do
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
    resList <- traverse cgen exprs
    return $ if null resList
               then constUint nilValue
               else last resList
  blks = createBlocks cgst
  extraFuncsComputations = extraFuncs cgst

codegenType :: SymName -> AST.Type -> LLVM ()
codegenType = defineType

codegenExterns :: LLVM ()
codegenExterns = do
  external uint "malloc"   [(AST.IntegerType 64, AST.Name "size")]
  external uint "memalign" [(AST.IntegerType 64, AST.Name "alignment")
                           ,(AST.IntegerType 64, AST.Name "size") ]

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

malloc :: Int -> Codegen AST.Operand
malloc size =
  call (funcOpr uint (AST.Name "malloc") [AST.IntegerType 64])
              [constUintSize 64 $ uintSizeBytes * size]

memalignRaw :: Int -> Codegen AST.Operand
memalignRaw  sizeInBytes =
  call (funcOpr uint (AST.Name "memalign") $ replicate 2 $ AST.IntegerType 64)
    $ map (constUintSize 64) [1, sizeInBytes]

memalign :: Int -> Codegen AST.Operand
memalign sizeInWords = memalignRaw $ sizeInWords * uintSizeBytes
  {-call (funcOpr uint (AST.Name "memalign") $ replicate 2 $ AST.IntegerType 64)-}
    {-$ map (constUintSize 64) [1, uintSizeBytes * size]-}

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
-- Translation of Expr values into llvm IR
-------------------------------------------------------------------------------

cgen :: Expr -> Codegen AST.Operand

cgen (BoolExp True) = return . constUint $ IM.true
cgen (BoolExp False) = return . constUint $ IM.false
cgen (NumberExp n) = return . constUint . toFixnum $ n
cgen (CharExp c) = return . constUint . toChar $ c
cgen EmptyExp = return . constUint $ nilValue

cgen (StringExp str) = do
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

cgen (ArrayExp exprs) = do
  vecPtr <- memalign $ exprCount + 1
  vecPtrC <- inttoptr vecPtr $ ptr uint
  store vecPtrC $ constUint exprCount

  for (zip [1..] exprs) $ \(i, expr) -> do
    opr <- cgen expr
    targetPtr <- getelementptrRaw vecPtrC [i]
    store targetPtr opr

  iadd vecPtr $ constUint $ readBinary vectorFormat
 where
  exprCount = length exprs

cgen (BinOpExp op a b) = do
  ca <- cgen a
  cb <- cgen b
  asIRbinOp op ca cb

cgen (VarExp varName) =
  maybe planB load =<< getvar varName
 where
  planB = load $ extern (AST.Name varName)

cgen (DefExp defName expr) = do
  gvs <- gets globalVars
  modify $ \s -> s { globalVars = defName : gvs }
  cgen (SetExp defName expr)

cgen (SetExp symName expr) = do
  mVarPtr <- getvar symName
  let ptr = fromMaybe (extern $ AST.Name symName) mVarPtr
  store ptr =<< cgen expr
  return $ constUint nilValue

cgen (PrimCallExp primName args) = do
  operands <- traverse cgen args
  call (extern $ AST.Name primName) operands

cgen (CallExp func args) = do
  funcEnvPtr <- cgen func
  funcEnvPtrC <- inttoptr funcEnvPtr $ ptr $ structType [uint, uint]
  envPtrPtr <- getelementptr funcEnvPtrC 0
  envPtr <- load envPtrPtr
  funcPtrPtr <- getelementptr funcEnvPtrC 1
  funcPtr <- load funcPtrPtr
  funcPtrC <- inttoptr funcPtr $
     ptr $ AST.FunctionType uint (argsTypeList $ length args + 1) False

  operands <- traverse cgen args
  call funcPtrC $ envPtr : operands

cgen fe@(FuncExp vars body) = do
  cgst <- get

  let
    (lambdaName, supply) =
      uniqueName (funcName cgst ++ suffLambda) $ names cgst
    freeVars = sort $ findFreeVars (globalVars cgst ++ vars) body
    est = envStructType freeVars
    atl = argsTypeList $ length vars + 1

    createFuncComputation =
      codegenFunction
        lambdaName atl prologue (envVarName : vars) body
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
  returnedOpr <- malloc 2
  returnedOprC <- inttoptr returnedOpr $ ptr $ structType [uint, uint]

  --Instantiating an env struct and filling it
  envPtr <- malloc $ length freeVars
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
-- Compilation
-------------------------------------------------------------------------------

initModule :: Bool -> String -> IO (Either String AST.Module)
initModule linkDriver label = withContext $ \context -> do
  dataDir <- getDataDir
  let primModFilePath = File $ dataDir </> "c-src/primitives.ll"
      driverModFilePath = File $ dataDir </> "c-src/driver.ll"
      constsModFilePath = File $ dataDir </> "c-src/constants.ll"
  runExceptT $
    linkModule primModFilePath
    >=> linkModule constsModFilePath
    >=> (if linkDriver then linkModule driverModFilePath else return)
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

writeTargetCode :: CompilerOptions
                -> AST.Module -> ExceptT String IO ()
writeTargetCode CompilerOptions{..} astMod = do
  writeObjFile
  void . liftIO . createProcess $
    proc "gcc" ["-lm", objfn, "-o", optOutputFilePath]
 where
  objfn = optInputFilePath ++ ".o"
  writeObjFile = ExceptT $
    withContext $ \context ->
      fmap join . runExceptT . withModuleFromAST context astMod $ \mod ->
        fmap join . runExceptT . withDefaultTargetMachine $ \target ->
          runExceptT $ writeObjectToFile target (File objfn) mod


printAsm :: AST.Module -> ExceptT String IO AST.Module
printAsm modl = ExceptT $ withContext $ \context ->
  runExceptT $ withModuleFromAST context modl $ \m -> do
    putStrLn =<< moduleLLVMAssembly m
    return modl

codegen :: CompilerOptions -> AST.Module -> [Expr] -> IO AST.Module
codegen CompilerOptions{..} modl exprs = do
  res <- runExceptT $ process preOptiAst
  case res of
    Right newAst -> return newAst
    Left err     -> putStrLn err >> return preOptiAst
 where
  preOptiAst = runLLVM modl deltaModl
  process = bool return printAsm optPrintLLVM
        >=> optimize
        >=> bool return jit optReplMode
  --process = printAsm >=> optimize >=> jit

  deltaModl = delPrevMain >> codegenTop exprs
