{-# LANGUAGE TypeFamilies #-}

module StmtGen where

import AST
import CodeGen
import Control.Monad.Writer

genAssign :: Annotated AssignLHS TypeA -> Var -> CodeGen ()
genAssign = undefined

genRHS :: Annotated AssignRHS TypeA -> CodeGen Var
genRHS = undefined

genExpr :: Annotated Expr TypeA -> CodeGen Var
genExpr = undefined

genBlock :: Annotated Block TypeA -> CodeGen ()
genBlock = undefined

-- TODO StmtReturn

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


