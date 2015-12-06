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
import qualified Data.Map as Map
import Data.Maybe

genProgram :: Annotated Program TypeA -> [[IR]]
genProgram (_, Program fs)
  = let generate = sequence (map genFunction fs)
    in evalState generate (map UnnamedLabel [0..])

-- Functions
genFunction :: Annotated FuncDef TypeA -> State [Label] [IR]
genFunction (_, FuncDef _ fname params body) = do
    labs <- get

    let initialState = CodeGenState {
          localNames = map Local [0..],
          tempNames = map Temp [0..],
          labels = labs,
          frame = emptyFrame
        }

        argZip = zip params argPassingRegs

        generation = do
          tell [ ILabel { iLabel = NamedLabel (show fname) }
               , IFunctionBegin { iArgs = map snd argZip }
               , IFrameAllocate { iSize = 0 } ] -- Fixed later once colouring / spilling is done
        
          savedRegs <- forM calleeSaveRegs $ \r -> do
            v <- allocateTemp
            tell [ IMove { iDest = v, iValue = r } ]
            return (r, v)

          args <- forM argZip $ \((_, name), r) -> do
            v <- allocateTemp
            tell [ IMove { iDest = v, iValue = r } ]
            return (name, v)

          case fname of
            MainFunc -> (genCall "p_initialise" [] >> return ())
            _ -> return ()

          setupFrame (Map.fromList args) savedRegs

          genBlock body

          retVal <- allocateTemp
          tell [ ILiteral { iDest = retVal, iLiteral = LitInt 0 } ]
          genReturn retVal

        (endState, irs) = (execRWS generation () initialState)

    put (labels endState)
    return irs

genReturn :: Var -> CodeGen ()
genReturn retVal = do
  savedRegs <- gets (frameSavedRegs . frame)
  tell (map (\(r,v) -> IMove { iDest = r, iValue = v }) (reverse savedRegs))

  tell [ IMove { iDest = Reg 0, iValue = retVal }
       , IFrameFree { iSize = 0 } -- Fixed later once colouring / spilling is done
       , IReturn ]

genCall :: Identifier -> [Var] -> CodeGen Var
genCall label vars = do
  outVal <- allocateTemp
  let args = zip argPassingRegs vars

  tell (map (\(a,v) -> IMove { iDest = a, iValue = v }) args)
  tell [ ICall { iLabel = NamedLabel label, iArgs = map fst args }
       , IMove { iDest = outVal, iValue = Reg 0 } ]
  return outVal

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
  litVar <- allocateTemp
  tell [ ILiteral { iDest = litVar, iLiteral = literal} ]
  return litVar

-- UnOp
genExpr (_, ExprUnOp UnOpChr expr) =
  genExpr expr
genExpr (_, ExprUnOp UnOpOrd expr) =
  genExpr expr
genExpr (_ , ExprUnOp operator expr) = do
  unOpVar <- allocateTemp
  valueVar <- genExpr expr
  tell [ IUnOp { iUnOp = operator, iDest = unOpVar, iValue = valueVar} ]
  return unOpVar

-- BinOp
genExpr (_ , ExprBinOp operator expr1 expr2) = do
  binOpVar <- allocateTemp
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
genFrameRead ty ident = gets (getFrameLocal ident . frame)

-- Read from Array
genArrayRead :: Type -> Var -> Annotated Expr TypeA -> CodeGen Var
genArrayRead elemTy arrayVar indexExpr = do
  outVar <- allocateTemp
  arrayOffsetVar <- allocateTemp 
  offsetedArray <- allocateTemp
  indexVar <- genExpr indexExpr

  genCall "p_check_array_bounds" [arrayVar, indexVar]
  tell [ ILiteral { iDest    = arrayOffsetVar
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
  localVar <- gets (getFrameLocal ident . frame)
  tell [ IMove { iDest = localVar, iValue = valueVar } ]

-- LHS Pair 
genAssign (_, LHSPairElem ((elemTy, pairTy), PairElem side pairExpr)) valueVar = do
  pairVar <- genExpr pairExpr
  genCall "p_check_null_pointer" [pairVar]
  tell [ IHeapWrite { iHeapVar = pairVar, iValue = valueVar, iOperand = OperandLit (pairOffset side pairTy), iType = elemTy } ]

-- LHS Array Indexing 
genAssign (elemTy, LHSArrayElem (_, ArrayElem ident exprs)) valueVar = do
  let readIndexExprs  = init exprs
      writeIndexExpr  = last exprs

  -- Here we don't care / know what's inside the arrays
  -- The type is only used to determine the type of instruction (LDR vs LDRB)
  arrayVar <- genFrameRead (TyArray TyAny) ident
  subArrayVar <- foldM (genArrayRead (TyArray TyAny)) arrayVar readIndexExprs
  offsetedBase <- allocateTemp

  writeIndexVar <- genExpr writeIndexExpr
  arrayOffsetVar <- allocateTemp
  genCall "p_check_array_bounds" [subArrayVar, writeIndexVar]
  tell [ILiteral { iDest = arrayOffsetVar, iLiteral = LitInt 4 }
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
  sizeVar <- allocateTemp
  tell [ ILiteral { iDest = sizeVar, iLiteral = LitInt (toInteger size) } ]
  arrayVar <- genCall "malloc" [sizeVar]

  arrayLen <- allocateTemp
  tell [ ILiteral { iDest = arrayLen, iLiteral = LitInt (toInteger (length exprs)) }
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
  sizeVar <- allocateTemp
  tell [ ILiteral { iDest = sizeVar, iLiteral = LitInt 8 }]
  pairVar <- genCall "malloc" [sizeVar]

  fstVar <- genExpr fstExpr
  tell [ IHeapWrite { iHeapVar = pairVar, iValue = fstVar, iOperand = OperandLit (pairOffset PairFst t), iType = t1 } ]

  sndVar <- genExpr sndExpr
  tell [ IHeapWrite { iHeapVar = pairVar, iValue = sndVar, iOperand = OperandLit (pairOffset PairSnd t), iType = t2 } ]

  return pairVar

genRHS (_, RHSPairElem ((elemTy, pairTy), PairElem side pairExpr)) = do
  outVar <- allocateTemp
  pairVar <- genExpr pairExpr
  genCall "p_check_null_pointer" [pairVar]
  tell [IHeapRead { iHeapVar = pairVar, iDest = outVar, iOperand = OperandLit (pairOffset side pairTy), iType = elemTy } ]
  return outVar

genRHS (_, RHSCall name exprs) = do
  argVars <- mapM genExpr exprs
  genCall ("f_" ++ name) argVars

genStmt :: Annotated Stmt TypeA -> CodeGen ()
genStmt (_, StmtSkip) = return ()

genStmt (st, StmtVar ty ident rhs) = do
  initFrame <- gets frame
  let newFrame = initFrame { definedLocals = Set.insert ident (definedLocals initFrame) }
  modify (\s -> s { frame = newFrame }) 

  genStmt (st, StmtAssign (ty, LHSVar ident) rhs)

genStmt (_, StmtAssign lhs rhs) = do
  v <- genRHS rhs
  genAssign lhs v

genStmt (_, StmtFree expr@(ty, _)) = do
  v <- genExpr expr
  genCall "free" [v]
  return ()

genStmt (_, StmtReturn expr) = do
  v <- genExpr expr
  tell [ IMove { iDest = Reg 0, iValue = v }
       , IReturn ]

genStmt (_, StmtExit expr) = do
  v <- genExpr expr
  genCall "exit" [v]
  return ()

genStmt (_, StmtPrint expr@(ty, _) newline) = do
  v <- genExpr expr
  let fname = case ty of
              TyInt  -> "p_print_int"
              TyBool -> "p_print_bool"
              TyChar -> "putchar"
              TyArray TyChar -> "p_print_string"
              _      -> "p_print_reference"

  genCall fname [v]
  when newline (genCall "p_print_ln" [] >> return ())

genStmt (_, StmtRead lhs@(ty, _)) = do
  let fname = case ty of
              TyInt  -> "p_read_int"
              TyBool -> "p_read_bool"

  v <- genCall fname []
  genAssign lhs v

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
genBlock ((_, locals), Block stmts)
  = withChildFrame (map fst locals) (forM_ stmts genStmt)

