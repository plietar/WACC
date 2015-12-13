{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module CodeGen (genProgram) where

import Common.AST
import CodeGenTypes
import Features

import Control.Monad.RWS
import Control.Monad.State
import Control.Applicative

import Data.Set (Set)
import Data.Tuple(swap)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

emit :: [IR] -> CodeGen ()
emit xs = tell (CodeGenOutput xs Set.empty)

emitFeature :: Feature -> CodeGen ()
emitFeature f = tell (CodeGenOutput [] (Set.singleton f))

genProgram :: Annotated Program TypeA -> ([[IR]], Set Feature)
genProgram (_, Program fs)
  = let generate = sequence (map genFunction fs)
        result = evalState generate (map UnnamedLabel [0..])
    in (map fst result, Set.unions (map snd result))

-- Functions
genFunction :: Annotated FuncDef TypeA -> State [Label] ([IR], Set Feature)
genFunction (_, FuncDef _ fname params body) = do
    labs <- get

    let initialState = CodeGenState {
          localNames = map Local [0..],
          tempNames = map Temp [0..],
          labels = labs,
          frame = emptyFrame
        }

        argRegCount = length argPassingRegs
        (regNames, stackNames) = splitAt argRegCount (map snd params)
        argZip = (zip regNames argPassingRegs)

        generation = do
          emit [ ILabel { iLabel = NamedLabel (show fname) }
               , IFunctionBegin { iArgs = map snd argZip, iSavedRegs = calleeSaveRegs }
               , IFrameAllocate { iSize = 0 } ] -- Fixed later once colouring / spilling is done

          regArgsMap <- forM argZip $ \(name, r) -> do
            v <- allocateTemp
            emit [ IMove { iDest = v, iValue = r } ]
            return (name, v)

          stackArgsMap <- forM (zip stackNames [0,4..]) $ \(name, offset) -> do
            v <- allocateTemp
            emit [ IFrameRead { iDest = v, iOffset = offset, iType = TyInt } ]
            return (name, v)

          case fname of
            MainFunc -> do
              genCall0 "p_initialise" []
              emitFeature Initialise
            _ -> return ()

          setupFrame (Map.fromList (regArgsMap ++ stackArgsMap))

          genBlock body

          retVal <- allocateTemp
          emit [ ILiteral { iDest = retVal, iLiteral = LitInt 0 } ]
          genReturn retVal

        (endState, result) = (execRWS generation () initialState)

    put (labels endState)
    return (instructions result, features result)

genReturn :: Var -> CodeGen ()
genReturn retVal
  = emit [ IMove { iDest = Reg 0, iValue = retVal }
         , IFrameFree { iSize = 0 } -- Fixed later once colouring / spilling is done
         , IReturn { iArgs = [ Reg 0 ], iSavedRegs = calleeSaveRegs } ]

genCall0 :: Identifier -> [Var] -> CodeGen ()
genCall0 label vars = do
  let regCount = length argPassingRegs
      (regVars, stackVars) = splitAt regCount vars

  let regArgs = zip argPassingRegs vars

  emit (map (\(a,v) -> IMove { iDest = a, iValue = v }) (zip argPassingRegs regVars))
  emit (map (\v -> IPushArg { iValue = v }) (reverse stackVars))
  emit [ ICall { iLabel = NamedLabel label, iArgs = map fst regArgs }
       , IClearArgs { iSize = 4 * length stackVars } ]

genCall1 :: Identifier -> [Var] -> CodeGen Var
genCall1 label vars = do
  outVal <- allocateTemp
  genCall0 label vars
  emit [ IMove { iDest = outVal, iValue = Reg 0 } ]
  return outVal

genCall2 :: Identifier -> [Var] -> CodeGen (Var, Var)
genCall2 label vars = do
  outVal0 <- allocateTemp
  outVal1 <- allocateTemp
  genCall0 label vars
  emit [ IMove { iDest = outVal0, iValue = Reg 0 }
       , IMove { iDest = outVal1, iValue = Reg 1 } ]
  return (outVal0, outVal1)

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
exprWeight (_, ExprIndexingElem (_, IndexingElem _ exprs))
  = 1 + maximum (map exprWeight exprs)

genExpr2 :: Annotated Expr TypeA -> Annotated Expr TypeA -> CodeGen (Var, Var)
genExpr2 expr1 expr2
  = if exprWeight expr1 > exprWeight expr2
    then liftM2 (,) (genExpr expr1) (genExpr expr2)
    else swap <$> liftM2 (,) (genExpr expr2) (genExpr expr1)

-- Literals
genExpr :: Annotated Expr TypeA -> CodeGen Var
genExpr (_ , ExprLit literal) = do
  litVar <- allocateTemp
  emit [ ILiteral { iDest = litVar, iLiteral = literal} ]
  return litVar

-- UnOp
genExpr (_, ExprUnOp UnOpChr expr) =
  genExpr expr
genExpr (_, ExprUnOp UnOpOrd expr) =
  genExpr expr
genExpr (_ , ExprUnOp operator expr) = do
  unOpVar <- allocateTemp
  valueVar <- genExpr expr
  emit [ IUnOp { iUnOp = operator, iDest = unOpVar, iValue = valueVar} ]
  return unOpVar

-- BinOp
-- Hardware division is not available
genExpr (_, ExprBinOp BinOpDiv expr1 expr2) = do
  emitFeature CheckDivideByZero

  (var1, var2) <- genExpr2 expr1 expr2
  genCall0 "p_check_divide_by_zero" [var1, var2]
  (q, _) <- genCall2 "__aeabi_idivmod" [var1, var2]
  return q

genExpr (_, ExprBinOp BinOpRem expr1 expr2) = do
  emitFeature CheckDivideByZero

  (var1, var2) <- genExpr2 expr1 expr2
  genCall0 "p_check_divide_by_zero" [var1, var2]
  (_, r) <- genCall2 "__aeabi_idivmod" [var1, var2]
  return r

-- Multiplication is special as it requires an extra register
genExpr (_, ExprBinOp BinOpMul expr1 expr2) = do
  (var1, var2) <- genExpr2 expr1 expr2
  highVar <- allocateTemp
  lowVar <- allocateTemp
  emit [ IMul { iHigh = highVar, iLow = lowVar
              , iLeft = var1, iRight = var2 } ]
  return lowVar

genExpr (_ , ExprBinOp operator expr1 expr2) = do
  outVar <- allocateTemp
  (var1, var2) <- genExpr2 expr1 expr2
  emit [ IBinOp { iBinOp = operator
                , iDest = outVar
                , iLeft = var1
                , iRight = var2 } ]
  return outVar

-- Variable
genExpr (ty, ExprVar ident) = genFrameRead ident

genExpr (_, ExprIndexingElem ((t, ts), IndexingElem ident exprs)) = do

  indexVar    <- genFrameRead ident
  outVar      <- foldM genIndexingRead indexVar (zip ts exprs)
  return outVar

-- Read from Frame
genFrameRead :: String -> CodeGen Var
genFrameRead ident = gets (getFrameLocal ident . frame)

-- Read from Tuple/Array
genIndexingRead :: Var -> (Type, Annotated Expr TypeA) -> CodeGen Var
genIndexingRead arrayVar ((TyArray elemTy), indexExpr) = do
  
  
  emitFeature CheckArrayBounds
  outVar         <- allocateTemp
  arrayOffsetVar <- allocateTemp 
  offsetedArray  <- allocateTemp
  indexVar       <- genExpr indexExpr

  genCall0 "p_check_array_bounds" [indexVar, arrayVar]
  emit [ ILiteral { iDest    = arrayOffsetVar
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

genIndexingRead tupleVar (elemTy@(TyTuple ts), (_, ExprLit (LitInt x))) = do
  outVar   <- allocateTemp
  emit [ IHeapRead { iHeapVar = tupleVar 
                   , iDest    = outVar
                   , iOperand = OperandLit (4 * fromInteger x)
                   , iType    = elemTy } ]
  return outVar

-- LHS Assign
genAssign :: Annotated AssignLHS TypeA -> Var -> CodeGen ()
genAssign (ty, LHSVar ident) valueVar = do
  localVar <- gets (getFrameLocal ident . frame)
  emit [ IMove { iDest = localVar, iValue = valueVar } ]

genAssign (_, LHSIndexingElem ((elemTy, ts), IndexingElem ident exprs)) valueVar = do

  let readIndexExprs  = init exprs
      writeIndexExpr  = last exprs

  indexVar   <- genFrameRead ident
  subIndexVar <- foldM genIndexingRead indexVar (zip ts readIndexExprs)
  genIndexingWrite (last ts) writeIndexExpr subIndexVar valueVar

genIndexingWrite :: Type -> Annotated Expr TypeA -> Var -> Var -> CodeGen ()
genIndexingWrite (TyArray elemTy) writeIndexExpr subIndexVar valueVar = do
  
  emitFeature CheckArrayBounds
  offsetedBase <- allocateTemp
  writeIndexVar <- genExpr writeIndexExpr
  arrayOffsetVar <- allocateTemp

  genCall0 "p_check_array_bounds" [writeIndexVar, subIndexVar]
  emit [ILiteral { iDest = arrayOffsetVar, iLiteral = LitInt 4 }
       , IBinOp { iBinOp = BinOpAdd, iDest = offsetedBase
                , iLeft = arrayOffsetVar, iRight = subIndexVar } 
       , IHeapWrite { iHeapVar  = offsetedBase 
                    , iValue    = valueVar
                    , iOperand  = OperandVar writeIndexVar (typeShift elemTy)
                    , iType     = elemTy } ]

genIndexingWrite (TyTuple ts) (_, ExprLit (LitInt x)) subIndexVar valueVar = 
  emitFeature CheckNullPointer
  genCall0 "p_check_null_pointer" [subIndexVar]
  emit [ IHeapWrite { iHeapVar = subIndexVar 
                    , iValue  = valueVar
                    , iOperand = OperandLit (4 * fromInteger x)
                    , iType = TyInt } ]

-- Shift depending on size of type
typeShift :: Type -> Int
typeShift TyChar = 0
typeShift TyBool = 0
typeShift _      = 2


-- RHS Expression Assignment
genRHS :: Annotated AssignRHS TypeA -> CodeGen Var
genRHS (_, RHSExpr expr) = genExpr expr
genRHS (TyArray elemTy, RHSArrayLit exprs) = do
  sizeVar <- allocateTemp
  emit [ ILiteral { iDest = sizeVar, iLiteral = LitInt (toInteger size) } ]
  arrayVar <- genCall1 "malloc" [sizeVar]

  arrayLen <- allocateTemp
  emit [ ILiteral { iDest = arrayLen, iLiteral = LitInt (toInteger (length exprs)) }
       , IHeapWrite { iHeapVar = arrayVar
                    , iValue = arrayLen
                    , iOperand = OperandLit 0
                    , iType = TyInt }]
  forM (zip exprs [0..]) $ \(expr, index) -> do
    elemVar <- genExpr expr
    emit [ IHeapWrite  { iHeapVar = arrayVar
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

genRHS (TyTuple types, RHSNewTuple exprs) = do
  sizeVar <- allocateTemp
  emit [ ILiteral { iDest = sizeVar, iLiteral = LitInt (toInteger (length exprs * 4)) }]
  tupleVar <- genCall1 "malloc" [sizeVar]
  forM (zip exprs [0..]) $ \(expr, index) -> do
    elemVar <- genExpr expr  
    emit [ IHeapWrite  { iHeapVar = tupleVar
                       , iValue = elemVar
                       , iOperand = OperandLit (index * 4)
                       , iType = types !! index } ]
  return tupleVar

genRHS (_, RHSCall name exprs) = do
  argVars <- mapM genExpr exprs
  genCall1 ("f_" ++ name) argVars

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
  case ty of
    TyTuple _  -> do
      genCall0 "p_check_null_pointer" [v]
      emitFeature CheckNullPointer
    _ -> return ()
  genCall0 "free" [v]

genStmt (_, StmtReturn expr) = do
  v <- genExpr expr
  genReturn v

genStmt (_, StmtExit expr) = do
  v <- genExpr expr
  genCall0 "exit" [v]

genStmt (_, StmtPrint expr@(ty, _) newline) = do
  v <- genExpr expr
  let (fname, feat) = case ty of
        TyInt  -> ("p_print_int", Just PrintInt)
        TyBool -> ("p_print_bool", Just PrintBool)
        TyChar -> ("putchar", Nothing)
        TyArray TyChar -> ("p_print_string", Just PrintString)
        _      -> ("p_print_reference", Just PrintReference)

  maybe (return ()) emitFeature feat
  genCall0 fname [v]

  when newline (genCall0 "p_print_ln" [] >> emitFeature PrintLine)

genStmt (_, StmtRead lhs@(ty, _)) = do
  let (fname, feat) = case ty of
        TyInt  -> ("p_read_int", ReadInt)
        TyChar -> ("p_read_char", ReadChar)

  emitFeature feat
  v <- genCall1 fname []
  genAssign lhs v

--genStmt (_, StmtIfNoElse condition thenBlock) = do
--  endLabel <- allocateLabel
--  thenLabel <- allocateLabel

--  condVar <- genExpr condition

--  emit [ICondJump { iLabel = thenLabel, iValue = condVar }
--       , ILabel { iLabel = thenLabel }
--       , IJump { iLabel = endLabel }]
--  genBlock thenBlock
--  emit [ILabel { iLabel = endLabel }]

genStmt (_, StmtIf condition thenBlock elseBlock) = do
  thenLabel <- allocateLabel
  endLabel <- allocateLabel

  condVar <- genExpr condition
  emit [ICondJump { iLabel = thenLabel, iValue = condVar }]

  case elseBlock of 
    Just b -> genBlock b
            
    Nothing -> return ()

  emit [IJump { iLabel = endLabel }
       , ILabel { iLabel = thenLabel }] 
  genBlock thenBlock
  emit [ILabel { iLabel = endLabel }]

genStmt (_, StmtWhile condition block ) = do
  startLabel <- allocateLabel
  endLabel <- allocateLabel

  emit [IJump { iLabel = endLabel }]

  emit [ILabel { iLabel = startLabel }]
  genBlock block

  emit [ILabel { iLabel = endLabel }]
  condVar <- genExpr condition
  emit [ICondJump { iLabel = startLabel, iValue = condVar }]

genStmt (_, StmtSwitch expr cs) = do
  e <- genExpr expr
  endLabel <- allocateLabel
  mapM (genCaseArm endLabel e) cs
  emit [ILabel {iLabel = endLabel }] 

genStmt (_, StmtScope block) = genBlock block

genCaseArm :: Label -> Var -> Annotated CaseArm TypeA -> CodeGen ()
genCaseArm endLabel e (_, CaseArm l b) = do
  condVar <- allocateTemp
  literalVar <- allocateTemp
  nextCase <- allocateLabel

  emit [ILiteral { iDest = literalVar, iLiteral = l}
       , IBinOp { iBinOp = BinOpNE, iDest = condVar, iLeft = e, iRight = literalVar }
       , ICondJump { iLabel = nextCase, iValue = condVar }]
  genBlock b
  emit [IJump {iLabel = endLabel}
       , ILabel {iLabel = nextCase}]

  
-- Block code generation
genBlock :: Annotated Block TypeA -> CodeGen ()
genBlock ((_, locals), Block stmts)
  = withChildFrame (map fst locals) (forM_ stmts genStmt)



