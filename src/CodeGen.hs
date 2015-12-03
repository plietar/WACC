{-# LANGUAGE TypeFamilies #-}

module CodeGen where

import Common.AST
import CodeGenTypes

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import Data.Tuple(swap)

-- Functions
genFunction :: Annotated FuncDef TypeA -> [IR]
genFunction (_, FuncDef _ fname params body)
  = execWriter (runReaderT (evalStateT generation initialState) topFrame)
    where
      topFrame = setVariables (map swap params) 4 rootFrame
      initialState = CodeGenState {
        variables = Prelude.map Var [0..],
        labels = Prelude.map UnnamedLabel [0..]
      }

      generation = do
        tell [ ILabel { iLabel = NamedLabel (show fname) }
             , IFunctionBegin ]
        genBlock body
        retVal <- allocateVar
        tell [ ILiteral { iDest = retVal, iLiteral = LitInt 0 }
             , IReturn { iValue = retVal } ]


-- Literals
genExpr :: Annotated Expr TypeA -> CodeGen Var
genExpr (_ , ExprLit literal) = do 
  litVar <- allocateVar
  tell [ ILiteral { iDest = litVar, iLiteral = literal} ]
  return litVar

-- UnOp
genExpr (_, ExprUnOp UnOpChr expr) =
  genExpr expr
genExpr (_, ExprUnOp UnOpOrd expr) = 
  genExpr expr
genExpr (_ , ExprUnOp operator expr) = do 
  unOpVar <- allocateVar
  valueVar <- genExpr expr
  tell [ IUnOp { iUnOp = operator, iDest = unOpVar, iValue = valueVar} ]
  return valueVar

-- BinOp
genExpr (_ , ExprBinOp operator expr1 expr2) = do 
  binOpVar <- allocateVar
  value1Var <- genExpr expr1
  value2Var <- genExpr expr2
  tell [ IBinOp { iBinOp = operator, iDest = binOpVar, iLeft = value1Var, iRight = value2Var } ]
  return binOpVar

-- Variable
genExpr (_, ExprVar ident) = genFrameRead ident

-- ArrayElem
genExpr (t, ExprArrayElem (_, ArrayElem ident xs)) = do
  frameVar <- genFrameRead ident
  outVar <- foldM (genArrayRead t) frameVar xs
  return outVar

-- Read from Frame
genFrameRead :: String -> CodeGen Var
genFrameRead ident = do
  outVar <- allocateVar
  offset <- variableOffset ident
  tell [ IFrameRead { iOffset = offset
                    , iDest   = outVar } ]
  return outVar

-- Read from Array
genArrayRead :: Type -> Var -> Annotated Expr TypeA -> CodeGen Var
genArrayRead arrayType arrayVar indexExpr = do
  outVar <- allocateVar
  arrayOffsetVar <- allocateVar 
  offsetedArray <- allocateVar
  indexVar <- genExpr indexExpr
  tell [ IBoundsCheck { iArray = arrayVar
                      , iIndex = indexVar }
        , ILiteral { iDest = arrayOffsetVar
                   , iLiteral = LitInt 4 }
        , IBinOp { iBinOp = BinOpAdd
                 , iDest = offsetedArray
                 , iLeft = arrayOffsetVar
                 , iRight = arrayVar } 
        , IHeapRead { iHeapVar = offsetedArray
                    , iDest = outVar
                    , iOperand = OperandVar arrayOffsetVar (typeShift arrayType) } ]
  return outVar

-- LHS Assign
genAssign :: Annotated AssignLHS TypeA -> Var -> CodeGen ()
genAssign (_, LHSVar ident) valueVar = do
  offset <- variableOffset ident
  tell [ IFrameWrite { iOffset = offset, iValue = valueVar } ]

-- LHS Pair 
-- Check which of the two annotations we need
genAssign (_, LHSPairElem (TyPair t1 _, PairElem side pairExpr)) valueVar = do
  pairVar <- genExpr pairExpr
  tell [ INullCheck { iValue = pairVar }
       , IHeapWrite { iHeapVar = pairVar, iValue = valueVar, iOperand = OperandLit offset } ]
    where
      offset = case side of
        PairFst -> 0
        PairSnd -> typeSize t1

-- LHS Array Indexing 
genAssign (t, LHSArrayElem (_, ArrayElem ident exprs)) valueVar = do
  let readIndexExprs  = init exprs
      writeIndexExpr  = last exprs

  arrayVar <- genFrameRead ident
  subArrayVar <- foldM (genArrayRead t) arrayVar readIndexExprs
  offsetedBase <- allocateVar

  writeIndexVar <- genExpr writeIndexExpr
  arrayOffsetVar <- allocateVar
  tell [ IBoundsCheck { iArray = subArrayVar
                      , iIndex = writeIndexVar }
       , ILiteral { iDest = arrayOffsetVar, iLiteral = LitInt 4 }
       , IBinOp { iBinOp = BinOpAdd, iDest = offsetedBase
                , iLeft = arrayOffsetVar, iRight = subArrayVar } 
       , IHeapWrite { iHeapVar = offsetedBase 
                    , iValue  = valueVar
                    , iOperand = OperandVar writeIndexVar (typeShift t) } ]
genAssign a b = error (show (a,b))

-- Shift depending on size of type
typeShift :: Type -> Int
typeShift TyChar = 0
typeShift TyBool = 0
typeShift _      = 2

-- Offset for a pair 
pairOffset :: PairSide -> Type -> Int
pairOffset PairFst _ = 0
pairOffset PairSnd (TyPair t _)  = typeSize t


-- RHS Expression Assignment
genRHS :: Annotated AssignRHS TypeA -> CodeGen Var
genRHS (_, RHSExpr expr) = genExpr expr
genRHS (TyArray t, RHSArrayLit exprs) = do
  arrayVar <- allocateVar
  arrayLen <- allocateVar
  tell [ IArrayAllocate { iDest = arrayVar, iSize = size }
       , ILiteral { iDest = arrayLen, iLiteral = LitInt (length exprs) }
       , IHeapWrite { iHeapVar = arrayVar
                    , iValue = arrayLen
                    , iOperand = OperandLit 0 }]
  forM (zip exprs [0..]) $ \(expr, index) -> do
    elemVar <- genExpr expr
    tell [ IHeapWrite  { iHeapVar = arrayVar
                       , iValue = elemVar
                       , iOperand = OperandLit (4 + index * tSize) } ]
  return arrayVar
    where
      size = 4 + tSize * (length exprs)
      tSize = typeSize t

genRHS (t, RHSNewPair fstExpr sndExpr) = do
  pairVar <- allocateVar
  fstVar <- genExpr fstExpr
  sndVar <- genExpr sndExpr
  tell [ IPairAllocate { iDest = pairVar }
       , IHeapWrite { iHeapVar = pairVar, iValue = fstVar, iOperand = OperandLit 0 }
       , IHeapWrite { iHeapVar = pairVar, iValue = sndVar, iOperand = OperandLit (pairOffset PairFst t) } ]
  return pairVar
   -- TODO: fix type of a snd pair(change semcheck to pass the list of previous types in the tuple (extension)
genRHS (_, RHSPairElem (_, PairElem side pairExpr)) = do
  outVar <- allocateVar
  pairVar <- genExpr pairExpr
  tell [ INullCheck { iValue = pairVar }
       , IHeapRead { iHeapVar = pairVar, iDest = outVar, iOperand = OperandLit (pairOffset side TyInt) } ]
  return outVar

genRHS (_, RHSCall name exprs) = do
  outVar <- allocateVar
  argVars <- forM exprs genExpr
  tell [ ICall { iLabel = NamedLabel name
               , iArgs = argVars
               , iDest = outVar } ]
  return outVar

genStmt :: Annotated Stmt TypeA -> CodeGen ()
genStmt (_, StmtSkip) = return ()
genStmt (st, StmtVar ty ident rhs)
  = genStmt (st, StmtAssign (ty, LHSVar ident) rhs)

genStmt (_, StmtAssign lhs rhs) = do
  v <- genRHS rhs
  genAssign lhs v

genStmt (_, StmtRead lhs@(ty, _)) = do
  v <- allocateVar
  tell [ IRead { iDest = v, iType = ty }]
  genAssign lhs v

genStmt (_, StmtFree expr@(ty, _)) = do
  v <- genExpr expr
  tell [ IFree { iValue = v, iType = ty }]

genStmt (_, StmtReturn expr) = do
  v <- genExpr expr
  s <- asks totalAllocatedFrameSize
  tell [ IFrameFree { iSize = s }
       , IReturn { iValue = v }]

genStmt (_, StmtExit expr) = do
  v <- genExpr expr
  tell [IExit { iValue = v }]

genStmt (_, StmtPrint expr@(ty, _) newline) = do
  v <- genExpr expr
  tell [IPrint { iValue = v, iType = ty, iNewline = newline }]

genStmt (_, StmtIf condition thenBlock elseBlock) = do
  thenLabel <- allocateLabel
  endLabel <- allocateLabel

  condVar <- genExpr condition
  tell [ICondJump { iLabel = thenLabel, iValue = condVar }]
  genBlock elseBlock
  tell [ IJump { iLabel = endLabel }
       , ILabel { iLabel = thenLabel }]
  genBlock thenBlock
  tell [ILabel { iLabel = endLabel }]

genStmt (_, StmtWhile condition block ) = do
  startLabel <- allocateLabel
  endLabel <- allocateLabel

  tell [IJump { iLabel = endLabel }]

  tell [ILabel { iLabel = startLabel }]
  genBlock block

  tell [ILabel { iLabel = endLabel }]
  condVar <- genExpr condition
  tell [ICondJump { iLabel = startLabel, iValue = condVar }]

genStmt (_, StmtScope block) = genBlock block



-- Block code generation
genBlock :: Annotated Block TypeA -> CodeGen ()
genBlock ((_, locals), Block stmts) = do
  local createFrame generation 
  where
      createFrame = setVariables locals 0 . childFrame
      generation = do 
        s <- asks frameSize
        tell [ IFrameAllocate { iSize = s } ]
        forM_ stmts genStmt
        tell [ IFrameFree { iSize = s } ]

