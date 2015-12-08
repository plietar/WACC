{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module CodeGen where

import Common.AST
import CodeGenTypes

import Control.Monad.RWS
import Control.Monad.State
import Control.Applicative

import Data.Tuple(swap)
import qualified Data.Set as Set
import Data.Maybe

genProgram :: Annotated Program TypeA -> [[IR]]
genProgram (_, Program fs)
  = let generate = sequence (map genFunction fs)
    in evalState generate (map UnnamedLabel [0..])

-- Functions
genFunction :: Annotated FuncDef TypeA -> State [Label] [IR]
genFunction (_, FuncDef _ fname params body) = do
    labs <- get
    let frame = setVariables (map swap params) 4 rootFrame
        topFrame = addDefinedVariables params frame
        initialState = CodeGenState {
          variables = Prelude.map Var [0..],
          labels = labs,
          frame = topFrame
        }
        (endState, irs) = (execRWS generation () initialState)
    put (labels endState)
    return irs
    where
      generation = do
        tell [ ILabel { iLabel = NamedLabel (show fname) }
             , IFunctionBegin ]
        genBlock body
        retVal <- allocateVar
        tell [ ILiteral { iDest = retVal, iLiteral = LitInt 0 }
             , IReturn { iValue = retVal } ]

addDefinedVariables :: [(Type, Identifier)] -> Frame -> Frame
addDefinedVariables args f
  = foldl (\f (_, id) -> f { definedVariables  = Set.insert id (definedVariables f) }) f args


-- This does not attempt to be 100% accurate
-- The register allocator affects a lot how many register are used
-- It only used as a rough metric for generating BinOps
exprWeight :: Annotated Expr TypeA -> Int
exprWeight (_, ExprLit _) = 1
exprWeight (_, ExprUnOp UnOpChr e) = exprWeight e
exprWeight (_, ExprUnOp UnOpOrd e) = exprWeight e
exprWeight (_, ExprUnOp _ e) = 1 + exprWeight e
exprWeight (_, ExprBinOp _ e1 e2)
  = let w1 = exprWeight e1
        w2 = exprWeight e2
    in min (max w1 (w2 + 1))
           (max (w1 + 1) w2)
exprWeight (_, ExprVar _) = 1
exprWeight (_, ExprArrayElem (_, ArrayElem _ exprs))
  = 1 + maximum (map exprWeight exprs)

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
  return unOpVar

-- BinOp
genExpr (_ , ExprBinOp operator expr1 expr2) = do
  binOpVar <- allocateVar
  (value1Var, value2Var) <- vars
  tell [ IBinOp { iBinOp = operator, iDest = binOpVar, iLeft = value1Var, iRight = value2Var } ]
  return binOpVar
  where
    vars = if exprWeight expr1 > exprWeight expr2
           then liftM2 (,) (genExpr expr1) (genExpr expr2)
           else swap <$> liftM2 (,) (genExpr expr2) (genExpr expr1)

-- Variable
genExpr (ty, ExprVar ident) = genFrameRead ty ident

-- ArrayElem
genExpr (elemTy, ExprArrayElem (_, ArrayElem ident exprs)) = do
  let initIndexExprs = init exprs
      lastIndexExpr  = last exprs

  -- Here we don't care / know what's inside the arrays
  -- The type is only used to determine the type of instruction (LDR vs LDRB)
  arrayVar <- genFrameRead (TyArray TyAny) ident
  subArrayVar <- foldM (genArrayRead (TyArray TyAny)) arrayVar initIndexExprs

  outVar <- genArrayRead elemTy subArrayVar lastIndexExpr

  return outVar

-- Read from Frame
genFrameRead :: Type -> String -> CodeGen Var
genFrameRead ty ident = do
  outVar <- allocateVar
  offset <- variableOffset ident
  tell [ IFrameRead { iOffset = offset
                    , iDest   = outVar
                    , iType   = ty} ]
  return outVar

-- Read from Array
genArrayRead :: Type -> Var -> Annotated Expr TypeA -> CodeGen Var
genArrayRead elemTy arrayVar indexExpr = do
  outVar <- allocateVar
  arrayOffsetVar <- allocateVar 
  offsetedArray <- allocateVar
  indexVar <- genExpr indexExpr
  tell [ IBoundsCheck { iArray = arrayVar
                      , iIndex = indexVar }
        , ILiteral { iDest    = arrayOffsetVar
                   , iLiteral = LitInt 4 }
        , IBinOp { iBinOp = BinOpAdd
                 , iDest  = offsetedArray
                 , iLeft  = arrayOffsetVar
                 , iRight = arrayVar }
        , IHeapRead { iHeapVar = offsetedArray
                    , iDest    = outVar
                    , iOperand = OperandVar indexVar (typeShift elemTy)
                    , iType    = elemTy } ]
  return outVar

-- LHS Assign
genAssign :: Annotated AssignLHS TypeA -> Var -> CodeGen ()
genAssign (ty, LHSVar ident) valueVar = do
  offset <- variableOffset ident
  tell [ IFrameWrite { iOffset = offset, iValue = valueVar, iType = ty } ]

-- LHS Pair 
genAssign (_, LHSPairElem ((elemTy, pairTy), PairElem side pairExpr)) valueVar = do
  pairVar <- genExpr pairExpr
  tell [ INullCheck { iValue = pairVar }
       , IHeapWrite { iHeapVar = pairVar, iValue = valueVar, iOperand = OperandLit (pairOffset side pairTy), iType = elemTy } ]

-- LHS Array Indexing 
genAssign (elemTy, LHSArrayElem (_, ArrayElem ident exprs)) valueVar = do
  let readIndexExprs  = init exprs
      writeIndexExpr  = last exprs

  -- Here we don't care / know what's inside the arrays
  -- The type is only used to determine the type of instruction (LDR vs LDRB)
  arrayVar <- genFrameRead (TyArray TyAny) ident
  subArrayVar <- foldM (genArrayRead (TyArray TyAny)) arrayVar readIndexExprs
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
                    , iOperand = OperandVar writeIndexVar (typeShift elemTy)
                    , iType = elemTy } ]

-- Shift depending on size of type
typeShift :: Type -> Int
typeShift TyChar = 0
typeShift TyBool = 0
typeShift _      = 2

-- Offset for a pair 
pairOffset :: PairSide -> Type -> Int
pairOffset PairFst (TyPair _ _)= 0
pairOffset PairSnd (TyPair t _)  = typeSize t


-- RHS Expression Assignment
genRHS :: Annotated AssignRHS TypeA -> CodeGen Var
genRHS (_, RHSExpr expr) = genExpr expr
genRHS (TyArray elemTy, RHSArrayLit exprs) = do
  arrayVar <- allocateVar
  arrayLen <- allocateVar
  tell [ IArrayAllocate { iDest = arrayVar, iSize = size }
       , ILiteral { iDest = arrayLen, iLiteral = LitInt (toInteger (length exprs)) }
       , IHeapWrite { iHeapVar = arrayVar
                    , iValue = arrayLen
                    , iOperand = OperandLit 0
                    , iType = TyInt }]
  forM (zip exprs [0..]) $ \(expr, index) -> do
    elemVar <- genExpr expr
    tell [ IHeapWrite  { iHeapVar = arrayVar
                       , iValue = elemVar
                       , iOperand = OperandLit (4 + index * elemSize)
                       , iType = elemTy } ]
  return arrayVar
    where
      -- For empty arrays, the element type is TyAny, so we can't get the size
      -- However, we don't need it since we know there are no elements to allocate
      size = 4 + if length exprs > 0
                 then (length exprs) * elemSize
                 else 0
      elemSize = typeSize elemTy

genRHS (t@(TyPair t1 t2), RHSNewPair fstExpr sndExpr) = do
  pairVar <- allocateVar
  fstVar <- genExpr fstExpr
  sndVar <- genExpr sndExpr
  tell [ IPairAllocate { iDest = pairVar }
       , IHeapWrite { iHeapVar = pairVar, iValue = fstVar, iOperand = OperandLit (pairOffset PairFst t), iType = t1 }
       , IHeapWrite { iHeapVar = pairVar, iValue = sndVar, iOperand = OperandLit (pairOffset PairSnd t), iType = t2 } ]
  return pairVar

genRHS (_, RHSPairElem ((elemTy, pairTy), PairElem side pairExpr)) = do
  outVar <- allocateVar
  pairVar <- genExpr pairExpr
  tell [ INullCheck { iValue = pairVar }
       , IHeapRead { iHeapVar = pairVar, iDest = outVar, iOperand = OperandLit (pairOffset side pairTy), iType = elemTy } ]
  return outVar

genRHS (_, RHSCall name exprs) = do
  outVar <- allocateVar
  argVars <- forM exprs (\e@(ty,_) -> (ty,) <$> genExpr e)
  tell [ ICall { iLabel = NamedLabel ("f_" ++ name)
               , iArgs = argVars
               , iDest = outVar } ]
  return outVar

genStmt :: Annotated Stmt TypeA -> CodeGen ()
genStmt (_, StmtSkip) = return ()
genStmt (st, StmtVar ty ident rhs) = do
  initFrame <- gets frame
  let definedVars = definedVariables initFrame
  let newFrame = initFrame { definedVariables = Set.insert ident definedVars }
  modify (\s -> s { frame = newFrame }) 
  genStmt (st, StmtAssign (ty, LHSVar ident) rhs)

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
  initFrame <- gets frame
  tell [ IFrameFree { iSize = totalAllocatedFrameSize initFrame }
       , IReturn { iValue = v }]

genStmt (_, StmtExit expr) = do
  v <- genExpr expr
  tell [IExit { iValue = v }]

genStmt (_, StmtPrint expr@(ty, _) newline) = do
  v <- genExpr expr
  tell [IPrint { iValue = v, iType = ty, iNewline = newline }]

genStmt (_, StmtIfNoElse condition thenBlock) = do
  thenLabel <- allocateLabel
  endLabel <- allocateLabel

  condVar <- genExpr condition
  tell [ICondJump { iLabel = thenLabel, iValue = condVar }]
  tell [IJump {iLabel = endLabel }
       , ILabel { iLabel = thenLabel }]
  genBlock thenBlock
  tell [ILabel { iLabel = endLabel }]

--genStmt (_, StmtSwitch condition caseArm) = do
  

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
  initialFrame <- gets frame
  modify (\s -> s { frame = setVariables locals 0 $ childFrame initialFrame } )
  updatedFrame <- gets frame
  tell [ IFrameAllocate { iSize = frameSize updatedFrame } ]
  forM_ stmts genStmt
  tell [ IFrameFree { iSize = frameSize updatedFrame } ]
  child <- gets frame
  modify (\s -> s { frame = fromJust (parent child) })



