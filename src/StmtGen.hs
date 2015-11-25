{-# LANGUAGE TypeFamilies #-}

module StmtGen where

import AST
import CodeGen
import Control.Monad.Writer

genAssign :: Annotated AssignLHS TypeA -> Var -> CodeGen ()
genAssign (_, LHSVar ident) valueVar = do
  offset <- variableOffset ident
  tell [ IFrameWrite { iOffset = offset, iValue = valueVar } ]

genAssign (_, LHSPairElem (_, PairElem side pairExpr)) valueVar = do
  pairVar <- genExpr pairExpr
  tell [ IPairWrite { iPair = pairVar, iSide = side, iValue = valueVar } ]

genAssign (_, LHSArrayElem (_, ArrayElem ident exprs)) valueVar = do
  let readIndexExprs  = init exprs
      writeIndexExpr  = last exprs

  arrayVar <- genFrameRead ident
  subArrayVar <- foldM genArrayRead arrayVar readIndexExprs

  writeIndexVar <- genExpr writeIndexExpr
  tell [ IArrayWrite { iArray = subArrayVar
                     , iIndex = writeIndexVar
                     , iValue = valueVar } ]

genRHS :: Annotated AssignRHS TypeA -> CodeGen Var
genRHS (_, RHSExpr expr) = genExpr expr
genRHS (_, RHSArrayLit exprs) = do
  arrayVar <- allocateVar
  tell [ IArrayAllocate { iDest = arrayVar, iSize = length exprs }]
  forM (zip exprs [0..]) $ \(expr, index) -> do
    indexVar <- allocateVar
    elemVar <- genExpr expr
    tell [ ILiteral { iDest = indexVar, iLiteral = LitInt index }
         , IArrayWrite { iArray = arrayVar, iIndex = indexVar, iValue = elemVar } ]
  return arrayVar

genRHS (_, RHSNewPair fstExpr sndExpr) = do
  pairVar <- allocateVar
  fstVar <- genExpr fstExpr
  sndVar <- genExpr sndExpr
  tell [ IPairAllocate { iDest = pairVar }
       , IPairWrite { iPair = pairVar, iSide = PairFst, iValue = fstVar }
       , IPairWrite { iPair = pairVar, iSide = PairSnd, iValue = sndVar } ]
  return pairVar

genRHS (_, RHSPairElem (_, PairElem side pairExpr)) = do
  outVar <- allocateVar
  pairVar <- genExpr pairExpr
  tell [ IPairRead { iPair = pairVar, iSide = side, iDest = outVar } ]
  return outVar

genRHS (_, RHSCall name exprs) = do
  argVars <- forM exprs genExpr
  tell [ ICall { iLabel = name, iArgs = argVars } ]

genStmt :: Annotated Stmt TypeA -> CodeGen ()
genStmt (_, StmtSkip) = return ()
genStmt (st, StmtVar ty ident rhs)
  = genStmt (st, StmtAssign (ty, LHSVar ident) rhs)

genStmt (_, StmtAssign lhs@(ty,_) rhs) = do
  v <- genRHS rhs
  genAssign lhs v

genStmt (_, StmtRead lhs@(ty, _)) = do
  v <- allocateVar
  tell [IRead { iDest = v, iType = ty }]
  genAssign lhs v

genStmt (_, StmtFree expr@(ty, _)) = do
  v <- genExpr expr
  tell [IFree { iValue = v, iType = ty }]

genStmt (_, StmtReturn expr) = undefined

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


