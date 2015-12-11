{-# LANGUAGE TupleSections #-}

module Frontend.SemCheck where

import Common.AST
import Common.ScopedMap as ScopedMap
import Common.WACCResult

import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map

data Context = Context
  { variables  :: ScopedMap String Type
  , functions  :: Map String ([Type], Type)
  , returnType :: Type
  }
type ContextState a = StateT Context WACCResult a

addVariable :: String -> Type -> ContextState ()
addVariable name value = do
  context <- get
  case ScopedMap.insertIfNotExists name value (variables context) of
    Just table -> put context { variables = table }
    Nothing -> lift $ semanticError ("Variable " ++ show name ++ " already exists")

getVariable :: String -> Context -> WACCResult Type
getVariable name context
  = case ScopedMap.lookup name (variables context) of
      Just value -> OK value
      Nothing    -> semanticError ("Unknown variable " ++ show name)

addFunction :: String -> ([Type], Type) -> ContextState ()
addFunction name typ = do
  context <- get
  case Map.lookup name (functions context) of
    Just _  -> lift $ semanticError ("Function " ++ show name ++ " already exists")
    Nothing -> put context { functions = Map.insert name typ (functions context) }

getFunction :: String -> Context -> WACCResult ([Type], Type)
getFunction name context
  = case Map.lookup name (functions context) of
      Just value -> OK value
      Nothing    -> semanticError ("Unknown function " ++ show name)

emptyContext :: Context
emptyContext = Context ScopedMap.empty Map.empty TyVoid

newContext :: Context -> Context
newContext parent
  = parent { variables = newScope (variables parent) }


compatibleType :: Type -> Type -> Bool
compatibleType t1 t2
  = case mergeTypes t1 t2 of
    OK _      -> True
    Error _ _ -> False

mergeTypes :: Type -> Type -> WACCResult Type
mergeTypes t1 TyAny = OK t1
mergeTypes TyAny t2 = OK t2
mergeTypes (TyArray t1) (TyArray t2)
  = TyArray <$> mergeTypes t1 t2
mergeTypes (TyTuple ts1) (TyTuple ts2)
  = TyTuple <$> sequence (zipWith mergeTypes ts1 ts2)
mergeTypes TyNull (TyTuple ts1)
  = OK (TyTuple ts1)
mergeTypes (TyTuple ts1) TyNull
  = OK (TyTuple ts1)
mergeTypes t1 t2
  | t1 == t2  = OK t1
  | otherwise = semanticError ("Types " ++ show t1 ++ " and " ++ 
                                      show t2 ++ " are not compatible")

isArrayType :: Type -> Bool
isArrayType (TyArray _) = True
isArrayType TyAny       = True
isArrayType _           = False

isHeapType :: Type -> Bool
isHeapType (TyTuple _ ) = True
isHeapType (TyArray _)  = True
isHeapType TyAny        = True
isHeapType _            = False

isOrderedType :: Type -> Bool
isOrderedType TyInt  = True
isOrderedType TyChar = True
isOrderedType TyAny  = True
isOrderedType _      = False

isReadableType :: Type -> Bool
isReadableType TyInt  = True
isReadableType TyBool = False
isReadableType TyChar = True
isReadableType TyAny  = True
isReadableType _      = False

isVoidType :: Type -> Bool
isVoidType TyVoid = True
isVoidType _      = False

checkLiteral :: Literal -> Type
checkLiteral (LitInt _)    = TyInt
checkLiteral (LitBool _)   = TyBool
checkLiteral (LitChar _)   = TyChar
checkLiteral (LitString _) = TyArray TyChar
checkLiteral  LitNull      = TyNull

checkIndexingElem :: Annotated IndexingElem SpanA -> Context -> WACCResult (Annotated IndexingElem TypeA)
checkIndexingElem (_, IndexingElem varname exprs) context = do
  baseTy <- getVariable varname context
  exprs' <- mapM (\e -> checkExpr e context) exprs
  ty <- checkIndexingElem' baseTy exprs'
  return (ty, IndexingElem varname exprs')

checkIndexingElem' :: Type -> [Annotated Expr TypeA] -> WACCResult (Type, [Type])
checkIndexingElem' baseType@(TyTuple ts) exprs@((ty, e):es) 
  | compatible && literal && bounded = do
      (elemType, baseTypes) <- checkIndexingElem' (ts !! exprLitToInt e) es 
      return (elemType, baseType : baseTypes)
  | compatible && literal            = semanticError("Cannot index " 
                                         ++ show (exprLitToInt e) ++ " out of bounds in tuple.")
  | otherwise                        = semanticError("Cannot index a variable of type " ++ show ty) 
    where
      compatible = compatibleType TyInt ty
      literal    = checkIfExprIsLiteralInt e
      bounded    = -1 < exprLitToInt e && exprLitToInt e < length ts

checkIndexingElem' baseType@(TyArray t) exprs@((ty, e):es)
  | compatible   = do 
     (elemType, baseTypes) <- checkIndexingElem' t es
     return (elemType, baseType : baseTypes)
  | otherwise    = semanticError ("Cannot index a variable with variable of type " ++ show ty)
    where 
      compatible = compatibleType TyInt ty
      
checkIndexingElem' t [] = OK (t, [])
checkIndexingElem' t _  = semanticError("Type " ++ show t ++ " not indexable")
 
checkIfExprIsLiteralInt :: Expr a -> Bool
checkIfExprIsLiteralInt (ExprLit (LitInt _)) = True
checkIfExprIsLiteralInt _ = False

exprLitToInt :: Expr a -> Int
exprLitToInt (ExprLit (LitInt n)) = fromInteger n

--checkArrayIndexing :: Type -> [Type] -> WACCResult Type
--checkArrayIndexing (TyArray innerType) (indexType : tys) = do
--  if compatibleType TyInt indexType
--  then checkArrayIndexing innerType tys
--  else semanticError ("Cannot index array with type " ++ show indexType)
--checkArrayIndexing TyAny _ _ = OK TyAny
--checkArrayIndexing t [] _    = OK t
--checkArrayIndexing t _ _     = semanticError ("Cannot index variable of type " ++ show t)

checkExpr :: Annotated Expr SpanA -> Context -> WACCResult (Annotated Expr TypeA)
checkExpr (_, ExprLit lit) _ = return (checkLiteral lit, ExprLit lit)

checkExpr (_, ExprVar varname) context = do
  ty <- getVariable varname context
  return (ty, ExprVar varname)

checkExpr (_, ExprIndexingElem indexElem) context = do
  indexElem'@(ty, _) <- checkIndexingElem indexElem context
  return (ty, ExprIndexingElem indexElem')

checkExpr (_, ExprUnOp op expr) context = do
  expr'@(t1, _) <- checkExpr expr context
  ty <- checkUnOp op t1
  return (ty, ExprUnOp op expr')

checkExpr (_, ExprBinOp op e1 e2) context = do
  e1'@(t1,_) <- checkExpr e1 context
  e2'@(t2,_) <- checkExpr e2 context
  ty <- checkBinOp op t1 t2
  return (ty, ExprBinOp op e1' e2')


unOpType :: UnOp -> (Type -> Bool, Type)
unOpType UnOpNot = (compatibleType TyBool, TyBool)
unOpType UnOpNeg = (compatibleType TyInt, TyInt)
unOpType UnOpOrd = (compatibleType TyChar, TyInt)
unOpType UnOpChr = (compatibleType TyInt, TyChar)
unOpType UnOpLen = (isArrayType, TyInt)

checkUnOp :: UnOp -> Type -> WACCResult Type
checkUnOp op typ
  = let (predicate, result) = unOpType op
    in if predicate typ
       then OK result
       else semanticError ("Cannot apply unary operator " ++ show op
                        ++ " to type " ++ show typ)

binOpType :: BinOp -> (Type -> Type -> Bool, Type)
binOpType op = case op of
  BinOpAdd -> arithmeticOp
  BinOpSub -> arithmeticOp
  BinOpMul -> arithmeticOp
  BinOpDiv -> arithmeticOp
  BinOpRem -> arithmeticOp
  BinOpGT  -> orderOp
  BinOpGE  -> orderOp
  BinOpLT  -> orderOp
  BinOpLE  -> orderOp
  BinOpEQ  -> equalityOp
  BinOpNE  -> equalityOp
  BinOpAnd -> booleanOp
  BinOpOr  -> booleanOp
  where
    arithmeticOp = (\t1 t2 -> compatibleType TyInt t1 
                           && compatibleType TyInt t2
                           , TyInt)
    booleanOp    = (\t1 t2 -> compatibleType TyBool t1 
                           && compatibleType TyBool t2
                           , TyBool)
    orderOp      = (\t1 t2 -> compatibleType t1 t2
                           && isOrderedType t1 
                           && isOrderedType t2
                           , TyBool)
    equalityOp   = (compatibleType, TyBool)

checkBinOp :: BinOp -> Type -> Type -> WACCResult Type
checkBinOp op t1 t2
  = let (predicate, result) = binOpType op
    in if predicate t1 t2
       then OK result
       else semanticError ("Cannot apply binary operator " ++ show op 
                        ++ " to types " ++ show t1 ++ " and " ++ show t2)

checkPairElem :: Annotated PairElem SpanA -> Context -> WACCResult (Annotated PairElem TypeA)
checkPairElem (_, PairElem side expr) context = do
  expr'@(outerType, _) <- checkExpr expr context
  innerType <- case (side, outerType) of
    (PairFst, TyPair f _) -> return f
    (PairSnd, TyPair _ s) -> return s
    (_, TyAny           ) -> return TyAny
    (_, _               ) -> semanticError ("Type " ++ show outerType ++ " is not pair")
  return ((innerType, outerType), PairElem side expr')
  
checkAssignLHS :: Annotated AssignLHS SpanA -> Context -> WACCResult (Annotated AssignLHS TypeA)
checkAssignLHS (_, LHSVar varname) context = do
  ty <- getVariable varname context
  return (ty, LHSVar varname)

checkAssignLHS (_, LHSPairElem pairElem) context = do
  pairElem'@((ty, _), _) <- checkPairElem pairElem context
  return (ty, LHSPairElem pairElem')

checkAssignLHS (_, LHSIndexingElem indexElem) context = do
  indexElem'@(ty, _) <- checkIndexingElem indexElem context
  return (ty, LHSIndexingElem indexElem')

checkAssignRHS :: Annotated AssignRHS SpanA -> Context -> WACCResult (Annotated AssignRHS TypeA)
checkAssignRHS (_, RHSExpr expr) context = do
  expr'@(ty, _) <- checkExpr expr context
  return (ty, RHSExpr expr')

checkAssignRHS (_, RHSArrayLit exprs) context = do
  exprs' <- mapM (\e -> checkExpr e context) exprs
  innerType <- foldM mergeTypes TyAny (map fst exprs')
  return (TyArray innerType, RHSArrayLit exprs')

checkAssignRHS (_, RHSNewPair e1 e2) context = do
  e1'@(t1,_) <- checkExpr e1 context
  e2'@(t2,_) <- checkExpr e2 context
  return (TyPair t1 t2, RHSNewPair e1' e2')

checkAssignRHS (_, RHSPairElem pairElem) context = do
  pairElem'@((ty, _), _) <- checkPairElem pairElem context
  return (ty, RHSPairElem pairElem')

checkAssignRHS (_, RHSNewTuple exprs) context = do
  exprs' <- mapM (\e -> checkExpr e context) exprs
  return (TyTuple (map fst exprs'), RHSNewTuple exprs')

checkAssignRHS (_, RHSCall fname args) context = do
  (expectedArgsType, returnType) <- getFunction fname context
  args' <- mapM (\e -> checkExpr e context) args

  let checkArgs :: Int -> [Type] -> [Type] -> WACCResult ()
      checkArgs _ [] [] = OK ()
      checkArgs n (a1:as1) (a2:as2)
        | compatibleType a1 a2 = checkArgs (n+1) as1 as2
        | otherwise = semanticError ("Expected type " ++ show a2 
                                  ++ " but got type " ++ show a1 
                                  ++ " for argument " ++ show (n + 1)
                                  ++ " of call to function "
                                  ++ show fname)
      checkArgs n _ _
        = semanticError ("Wrong number of arguments in call"
                      ++ " to function \"" ++ fname ++ "\"."
                      ++ " Expected " ++ show (length expectedArgsType)
                      ++ " but got " ++ show (length args))

  checkArgs 0 expectedArgsType (map fst args')
  return (returnType, RHSCall fname args')


checkBlock :: Annotated Block SpanA -> Context -> WACCResult (Annotated Block TypeA)
checkBlock (_, Block stmts) parent = do
  let context = newContext parent
  (stmts', finalContext) <- runStateT (mapM checkPosStmt stmts) context

  let alwaysReturns = or (map fst stmts')
  let locals = Map.assocs (ScopedMap.localTable (variables finalContext))

  return ((alwaysReturns, locals), Block stmts')


checkPosStmt :: Annotated Stmt SpanA -> ContextState (Annotated Stmt TypeA)
checkPosStmt stmt@(((line,column,fname),_), _)
  = withErrorContext
      ("at " ++ show fname ++ " (line " ++ show line ++  ", column "  ++ show column ++ ")")
      (checkStmt stmt)


checkStmt :: Annotated Stmt SpanA -> ContextState (Annotated Stmt TypeA)
checkStmt (_, StmtSkip) = return (False, StmtSkip)

checkStmt (_, StmtVar varType varname rhs) = do
  context <- get
  rhs'@(rhsType, _) <- lift $ checkAssignRHS rhs context

  when (not (compatibleType varType rhsType))
       (lift (semanticError ("Cannot assign RHS of type " ++ show rhsType
                          ++ " to LHS of type " ++ show varType)))
  addVariable varname varType
  return (False, StmtVar varType varname rhs')

checkStmt (_, StmtAssign lhs rhs) = do
  context <- get
  lhs'@(lhsType, _) <- lift $ checkAssignLHS lhs context
  rhs'@(rhsType, _) <- lift $ checkAssignRHS rhs context
  when (not (compatibleType lhsType rhsType))
       (lift (semanticError ("Cannot assign RHS of type " ++ show rhsType
                          ++ " to LHS of type " ++ show lhsType)))
  return (False, StmtAssign lhs' rhs')

checkStmt (_, StmtRead lhs) = do
  context <- get
  lhs'@(lhsType, _) <- lift $ checkAssignLHS lhs context
  when (not (isReadableType lhsType))
       (lift (semanticError ("Cannot read variable of type " 
                                                             ++ show lhsType)))
  return (False, StmtRead lhs')

checkStmt (_, StmtFree expr) = do
  context <- get
  expr'@(exprType, _) <- lift $ checkExpr expr context
  when (not (isHeapType exprType))
       (lift (semanticError ("Cannot free variable of type " ++ show exprType)))
  return (False, StmtFree expr')

checkStmt (_, StmtReturn expr) = do
  context <- get
  expr'@(exprType, _) <- lift $ checkExpr expr context
  retType <- lift $ mergeTypes (returnType context) exprType
  return (True, StmtReturn expr')

checkStmt (_, StmtExit expr) = do
  context <- get
  expr'@(exprType, _) <- lift $ checkExpr expr context
  when (not (compatibleType TyInt exprType))
       (lift (semanticError ("Expected int in exit statement, got " ++ show exprType)))
  return (True, StmtExit expr')

checkStmt (_, StmtPrint expr ln) = do
  context <- get
  expr' <- lift $ checkExpr expr context
  return (False, StmtPrint expr' ln)

checkStmt (_, StmtIf predicate b1 b2) = do
  context <- get
  predicate'@(predicateType, _) <- lift $ checkExpr predicate context
  when (not (compatibleType TyBool predicateType))
       (lift (semanticError ("Condition cannot be of type " ++ show predicateType)))
  b1'@((al1,_),_) <- lift $ checkBlock b1 context
  b2'@((al2,_),_) <- lift $ checkBlock b2 context

  let al = al1 && al2
  return (al, StmtIf predicate' b1' b2')

checkStmt (_, StmtWhile predicate block) = do
  context <- get
  predicate'@(predicateType, _) <- lift $ checkExpr predicate context
  when (not (compatibleType TyBool predicateType))
       (lift (semanticError ("Condition cannot be of type " ++ show predicateType)))

  block' <- lift $ checkBlock block context
  return (False, StmtWhile predicate' block')

checkStmt (_, StmtScope block) = do
  context <- get
  block'@((al, _),_) <- lift $ checkBlock block context
  return (al, StmtScope block')


checkFunction :: Annotated FuncDef SpanA -> Context -> WACCResult (Annotated FuncDef TypeA)
checkFunction (_, FuncDef expectedReturnType name args block) globalContext
  = withErrorContext ("In function " ++ show name) $ do
      let setupContext = do
           modify (\c -> c { returnType = expectedReturnType })
           forM_ args (\(argType, argName) -> addVariable argName argType)

      context <- execStateT setupContext globalContext 
      block'@((alwaysReturns, _), _) <- checkBlock block context

      unless (isVoidType expectedReturnType || alwaysReturns)
             (syntaxError ("Function " ++ show name ++ " does not always return"))

      return ((), FuncDef expectedReturnType name args block')

checkProgram :: Annotated Program SpanA -> WACCResult (Annotated Program TypeA)
checkProgram (_, Program funcs) = do
  let defineFunc (_, FuncDef returnType (FuncName name) args _)
        = addFunction name (map fst args, returnType)
      defineFunc (_, FuncDef _ MainFunc _ _) = return ()

      defineAllFuncs = forM_ funcs defineFunc
  context <- execStateT defineAllFuncs emptyContext
  funcs' <- forM funcs (\f -> checkFunction f context)
  return ((), Program funcs')


waccSemCheck :: Annotated Program SpanA -> WACCResult (Annotated Program TypeA)
waccSemCheck = checkProgram
