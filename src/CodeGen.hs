{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module CodeGen (genProgram) where

import Arguments
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

genProgram :: Annotated Program TypeA -> WACCArguments ([[IR]], Set Feature)
genProgram (_, Program fs)
  = snd <$> evalRWST (mapM genDecl fs) () (map UnnamedLabel [0..])

genDecl :: Annotated Decl TypeA -> RWST () ([[IR]], Set Feature) [Label] WACCArguments ()
genDecl (_, DeclFunc f) = genFunction f
genDecl (_, DeclType f) = return ()
genDecl (_, DeclFFIFunc f) = return ()

genAlloc :: Int -> Type -> CodeGen Var
genAlloc size t = do
  sizeVar <- allocateTemp
  emit [ ILiteral { iDest = sizeVar, iLiteral = LitInt (toInteger size) } ]
  typeInfoVar <- genGCTypeInfo t
  genCall1 "GCAlloc" [sizeVar, typeInfoVar]

-- Functions
genFunction :: Annotated FuncDef TypeA -> RWST () ([[IR]], Set Feature) [Label] WACCArguments ()
genFunction (_, FuncDef _ async fname params body) = do
    labs <- get

    let initialState = CodeGenState {
          localNames = map Local [0..],
          tempNames = map Temp [0..],
          labels = labs,
          frame = emptyFrame
        }

        reader = CodeGenReader {
            asyncContext = async
        }

        argRegSkip = if async then 1 else 0
        argRegCount = length argPassingRegs - argRegSkip
        (regNames, stackNames) = splitAt argRegCount (map snd params)
        argZip = (zip regNames (drop argRegSkip argPassingRegs))

        setupArguments = do
          regArgsMap <- forM argZip $ \(name, r) -> do
            v <- allocateTemp
            emit [ IMove { iDest = v, iValue = r } ]
            return (name, v)

          stackArgsMap <- forM (zip stackNames [0,4..]) $ \(name, offset) -> do
            v <- allocateTemp
            emit [ IFrameRead { iDest = v, iOffset = offset, iType = TyInt } ]
            return (name, v)

          setupFrame (Map.fromList (regArgsMap ++ stackArgsMap))


        generation :: CodeGen ()
        generation = do
          emit [ ILabel { iLabel = NamedLabel (show fname) }
               , IFunctionBegin { iArgs = map snd argZip, iSavedRegs = calleeSaveRegs }
               , IFrameAllocate { iSize = 0 } ] -- Fixed later once colouring / spilling is done

          if async
          then do
            emit [ IMove { iDest = GeneratorState, iValue = Reg 0 } ]

            startLabel <- allocateLabel
            emit [ ICompare { iValue = GeneratorState, iOperand = OperandLit 0 }
                 , ICondJump { iCondition = CondEQ, iLabel = startLabel } ]

            continuationVar <- allocateTemp
            emit [ IHeapRead { iHeapVar = GeneratorState
                             , iDest = continuationVar
                             , iOperand = OperandLit 0
                             , iType = TyInt }
                 , IJumpReg { iValue = continuationVar } ]

            emit [ ILabel { iLabel = startLabel } ]

            setupArguments

            sizeVar <- allocateTemp
            emit [ ILiteral { iDest = sizeVar, iLiteral = LitInt 64 } ]
            stateVar <- genCall1 "malloc" [sizeVar]
            emit [ IMove { iDest = GeneratorState, iValue = stateVar } ]

          else setupArguments

          genBlock body

          retVal <- allocateTemp
          emit [ ILiteral { iDest = retVal, iLiteral = LitInt 0 } ]
          genReturn retVal

    (endState, result) <- lift (execRWST generation reader initialState)

    put (labels endState)
    tell ([instructions result], features result)

genYield :: Var -> CodeGen ()
genYield value = do
  continuationLabel <- allocateLabel
  continuationVar <- allocateTemp

  emit [ ILiteral { iDest = continuationVar, iLiteral = LitLabel continuationLabel }
       , IHeapWrite { iHeapVar = GeneratorState
                    , iValue = continuationVar
                    , iOperand = OperandLit 0
                    , iType = TyInt }
       , IMove { iDest = Reg 0, iValue = GeneratorState }
       , IMove { iDest = Reg 1, iValue = value }
       , IYield { iSavedRegs = calleeSaveRegs, iValue = GeneratorState, iSavedContext = [] }
       , ILabel { iLabel = continuationLabel }
       , IRestore { iValue = GeneratorState, iSavedContext = [] } ]

genReturn :: Var -> CodeGen ()
genReturn retVal = do
  async <- asks asyncContext
  if async
  then do
    -- FIXME
    -- genCall "free" [ GeneratorState ]

    zeroVal <- allocateTemp
    emit [ ILiteral { iDest = zeroVal, iLiteral = LitInt 0 }
         , IMove { iDest = Reg 0, iValue = zeroVal }
         , IMove { iDest = Reg 1, iValue = retVal }
         , IFrameFree { iSize = 0 }
         , IReturn { iArgs = [ Reg 0, Reg 1 ], iSavedRegs = [] } ]

  else emit [ IMove { iDest = Reg 0, iValue = retVal }
            , IFrameFree { iSize = 0 }
            , IReturn { iArgs = [ Reg 0 ], iSavedRegs = [] } ]

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

genAwait :: Identifier -> [Var] -> CodeGen Var
genAwait name argVars = do
  stateVar <- allocateTemp
  resultVar <- allocateTemp

  againLabel <- allocateLabel
  endLabel <- allocateLabel

  -- Initial call
  nullVar <- allocateTemp
  emit [ ILiteral { iDest = nullVar, iLiteral = LitInt 0 } ]
  (s, r) <- genCall2 name (nullVar : argVars)
  emit [ IMove { iDest = stateVar, iValue = s } ]
  emit [ IMove { iDest = resultVar, iValue = r } ]

  emit [ ICompare { iValue = stateVar, iOperand = OperandLit 0 } ]
  emit [ ICondJump { iLabel = endLabel, iCondition = CondEQ } ]

  emit [ ILabel { iLabel = againLabel } ]
  -- Keep calling until function is ready
  genYield resultVar

  (s, r) <- genCall2 name [stateVar]
  emit [ IMove { iDest = stateVar, iValue = s } ]
  emit [ IMove { iDest = resultVar, iValue = r } ]

  emit [ ICompare { iValue = stateVar, iOperand = OperandLit 0 } ]
  emit [ ICondJump { iLabel = againLabel, iCondition = CondNE } ]

  emit [ ILabel { iLabel = endLabel } ]

  return resultVar

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
  emitFeature CheckNullPointer
  genCall0 "p_check_null_pointer" [tupleVar]
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

genIndexingWrite (TyTuple ts) (_, ExprLit (LitInt x)) subIndexVar valueVar = do

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


-- Garbage Collector Type Information Methods
genGCTypeInfo :: Type -> CodeGen Var
genGCTypeInfo t = do
  emitFeature (GCTypeInformation t)
  let label = mangleTypeInformation t
  typeInfoVar <- allocateTemp
  emit [ ILiteral { iDest = typeInfoVar, iLiteral = LitLabel (NamedLabel label) } ]
  return typeInfoVar





-- RHS Expression Assignment
genRHS :: Annotated AssignRHS TypeA -> CodeGen Var
genRHS (_, RHSExpr expr) = genExpr expr
genRHS (t@(TyArray elemTy), RHSArrayLit exprs) = do
  arrayLen <- allocateTemp
  arrayVar <- genAlloc size t
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

genRHS (baseTy@(TyTuple types), RHSNewTuple exprs) = do
  tupleVar <- genAlloc size baseTy
  forM (zip exprs [0..]) $ \(expr, index) -> do
    elemVar <- genExpr expr
    emit [ IHeapWrite  { iHeapVar = tupleVar
                       , iValue = elemVar
                       , iOperand = OperandLit (index * 4)
                       , iType = types !! index } ]
  return tupleVar
    where
      size = length exprs * 4

genRHS (_, RHSCall name exprs) = do
  argVars <- mapM genExpr exprs
  genCall1 name argVars

genRHS (_, RHSAwait name exprs) = do
  argVars <- mapM genExpr exprs
  genAwait name argVars

genRHS (_, RHSNewChan) = genCall1 "wacc_channel_create" []

genRHS (_, RHSChanRecv chanName) = do
  chanVar <- genFrameRead chanName
  genAwait "wacc_channel_receive" [chanVar]

genCaseArm :: Label -> Var -> Annotated CaseArm TypeA -> CodeGen ()
genCaseArm endLabel e (_, CaseArm l b) = do
  condVar <- allocateTemp
  literalVar <- allocateTemp
  nextCase <- allocateLabel

  emit [ ILiteral { iDest = literalVar, iLiteral = l}
       , ICompare { iValue = e, iOperand = OperandVar literalVar 0 }
       , ICondJump { iLabel = nextCase, iCondition = CondNE } ]
  genBlock b
  emit [ IJump {iLabel = endLabel}
       , ILabel {iLabel = nextCase} ]

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
--  genCall0 "free" [v]

genStmt (_, StmtReturn expr) = do
  v <- genExpr expr
  genReturn v

genStmt (_, StmtExit expr) = do
  v <- genExpr expr
  genCall0 "exit" [v]

genStmt (_, StmtPrint exprs newline) = do
  forM exprs $ \expr@(ty, _) -> do
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

genStmt (_, StmtIf condition thenBlock elseBlock) = do
  elseLabel <- allocateLabel
  endLabel <- allocateLabel

  condVar <- genExpr condition

  emit [ ICompare { iValue = condVar, iOperand = OperandLit 0 }
       , ICondJump { iLabel = elseLabel, iCondition = CondEQ } ]

  genBlock thenBlock

  case elseBlock of
    Just b -> do
      emit [ IJump { iLabel = endLabel }
           , ILabel { iLabel = elseLabel } ]
      genBlock b
      emit [ ILabel { iLabel = endLabel } ]
    Nothing -> do
      emit [ ILabel { iLabel = elseLabel } ]


genStmt (_, StmtWhile condition block ) = do
  startLabel <- allocateLabel
  endLabel <- allocateLabel

  emit [IJump { iLabel = endLabel }]

  emit [ILabel { iLabel = startLabel }]
  genBlock block

  emit [ILabel { iLabel = endLabel }]
  condVar <- genExpr condition
  emit [ ICompare { iValue = condVar, iOperand = OperandLit 0 }
       , ICondJump { iLabel = startLabel, iCondition = CondNE } ]

genStmt (_, StmtSwitch expr cs) = do
  e <- genExpr expr
  endLabel <- allocateLabel
  mapM (genCaseArm endLabel e) cs
  emit [ILabel {iLabel = endLabel }]

genStmt (_, StmtScope block) = genBlock block

genStmt (_, StmtCall name exprs) = do
  argVars <- mapM genExpr exprs
  genCall0 name argVars

genStmt (_, StmtAwait name exprs) = do
  argVars <- mapM genExpr exprs
  genAwait name argVars
  return ()

genStmt (_, StmtFire name exprs) = do
  funcVar <- allocateTemp
  nameVar <- allocateTemp
  emit [ ILiteral { iDest = funcVar, iLiteral = LitLabel (NamedLabel name) } ]
  emit [ ILiteral { iDest = nameVar, iLiteral = LitString name } ]

  argVars <- mapM genExpr exprs
  genCall0 "wacc_fire" (funcVar : nameVar : argVars)

genStmt (_, StmtChanSend chanName elemRHS) = do
  chanVar <- genFrameRead chanName
  elemVar <- genRHS elemRHS

  genAwait "wacc_channel_send" [chanVar, elemVar]
  return ()

-- Block code generation
genBlock :: Annotated Block TypeA -> CodeGen ()
genBlock ((_, locals), Block stmts)
  = withChildFrame (map fst locals) (forM_ stmts genStmt)

