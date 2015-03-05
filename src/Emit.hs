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
import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.State (modify, gets, get)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Codegen
import qualified Codegen as CG
import Syntax
import JIT
import Immediates hiding (false, true)
import qualified Immediates as IM

import Paths_lc_hs (getDataDir)
import System.FilePath ((</>))
import Debug.Trace (trace)

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
    (AST.Function { G.name = AST.Name "entryFunc", .. })) = False
  filt _ = True

codegenTop :: Expr -> LLVM ()
codegenTop (DefExp name (FuncExp args body)) =
  codegenFunction name argTys (return ()) (FuncExp (envVarName : args) body)
 where
  argTys = argsTypeList $ length args + 1

codegenTop expr = do
  defineFunc uint funcName [] funcBlks
  sequence_ extraFuncsComputations
 where
  funcName = "entryFunc"
  cgst = execCodegen funcName $ do
    blk <- addBlock entryBlockName
    setBlock blk
    cgen expr >>= ret
  funcBlks = createBlocks cgst
  extraFuncsComputations = extraFuncs cgst

codegenFunction :: SymName -> [AST.Type] -> Codegen a -> Expr -> LLVM ()
codegenFunction funcName argTys cmds (FuncExp args body) = do
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
    cmds
    cgen body >>= ret
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
              [constUintSize 64 size]

memalign :: Int -> Codegen AST.Operand
memalign size =
  call (funcOpr uint (AST.Name "memalign") $ replicate 2 $ AST.IntegerType 64)
    $ map (constUintSize 64) [1, size]

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

cgen (VarExp varName) =
  {-maybe funcWithEmptyEnv load =<< getvar varName-}
  maybe funcWithEmptyEnv load =<< getvar varName
 where
  funcWithEmptyEnv = do
    pair <- malloc (2 * uintSizeBytes)
    pairC <- inttoptr pair $ ptr $ structType [uint, uint]

    funcOprC <- ptrtoint (extern $ AST.Name varName) uint
    setElemPtr pairC 1 funcOprC
    return pair

cgen (BoolExp True) = return . constUint $ IM.true
cgen (BoolExp False) = return . constUint $ IM.false

cgen (NumberExp n) = return . constUint . toFixnum $ n

cgen (CharExp c) = return . constUint . toChar $ c
  {-do-}
  {-memalign 10-}
  {-memalign 20-}

cgen (BinOpExp op a b) = do
  ca <- cgen a
  cb <- cgen b
  asIRbinOp op ca cb

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

--((lambda(x)(+ x ((lambda(y)(+ y x)) 2))) 5)

cgen fe@(FuncExp vars body) = do
  cgst <- get

  let
    (lambdaName, supply) =
      uniqueName (funcName cgst ++ suffLambda) $ names cgst
    est = envStructType freeVars
    atl = argsTypeList $ length vars + 1

    createFuncComputation =
      codegenFunction lambdaName atl lambdaBodyPrelude $
      FuncExp (envVarName : vars) body
    lambdaBodyPrelude = do
      let envPtr =
            AST.LocalReference uint
            $ AST.Name envVarName
      envPtrC <- inttoptr envPtr $ ptr est
      for (zip [0..] freeVars) $ \(ix,freeVar) -> do
        localVar <- alloca uint
        elementPtr <- getelementptr envPtrC ix
        element <- load elementPtr
        store localVar element
        assign freeVar localVar
  --Adding llvm computations to add the lambda function and env struct
  --as globals in the llvm module
  modify $ \cgst -> cgst
    { extraFuncs = createFuncComputation
                 {-: createTypeComputation-}
                 : extraFuncs cgst
    , names = supply
    }


  --Setting up the operand to return
  returnedOpr <- malloc (2 * uintSizeBytes)
  returnedOprC <- inttoptr returnedOpr $ ptr $ structType [uint, uint]

  --Instantiating an env struct and filling it
  envPtr <- malloc (uintSizeBytes * length freeVars)
  envPtrC <- inttoptr envPtr $ ptr est
  for (zip [0..] freeVars) $ \(ix,freeVar) -> do
    fvPtr <- flip liftM (getvar freeVar)
               $ fromMaybe
                   (error "bug - freevar filling")
    fvVal <- load fvPtr
    setElemPtr envPtrC ix fvVal

  setElemPtr returnedOprC 0 envPtr

  funcOprC <- ptrtoint (funcOpr uint (AST.Name lambdaName) atl) uint
  setElemPtr returnedOprC 1 funcOprC

  return returnedOpr
 where
  freeVars = sort $ findFreeVars vars body

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
  deltaModl  = delPrevMain >> trace "im running" traverse codegenTop exprs
  preOptiAst = runLLVM modl deltaModl
