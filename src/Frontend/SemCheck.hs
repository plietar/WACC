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
  { variables    :: ScopedMap String Type
  , functions    :: Map String (String, Bool, [Type], Type)
  , typeAliases  :: Map String Type
  , returnType   :: Type
  , asyncContext :: Bool
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

addFunction :: String -> (String, Bool, [Type], Type) -> ContextState ()
addFunction name typ = do
  context <- get
  case Map.lookup name (functions context) of
    Just _  -> lift $ semanticError ("Function " ++ show name ++ " already exists")
    Nothing -> put context { functions = Map.insert name typ (functions context) }

addTypeAlias :: String -> Type -> ContextState ()
addTypeAlias name typeAlias = do
  context <- get
  case Map.lookup name (typeAliases context) of
    Just _  -> lift $ semanticError ("Type " ++ show name ++ " already exists")
    Nothing -> put context { typeAliases = Map.insert name typeAlias (typeAliases context) }

getFunction :: String -> Context -> WACCResult (String, Bool, [Type], Type)
getFunction name context
  = case Map.lookup name (functions context) of
      Just value -> OK value
      Nothing    -> semanticError ("Unknown function " ++ show name)

getTypeAlias :: String -> Context -> WACCResult Type
getTypeAlias name context
  = case Map.lookup name (typeAliases context) of
      Just value -> OK value
      Nothing    -> semanticError ("Unknown type " ++ show name)

emptyContext :: Context
emptyContext = Context ScopedMap.empty Map.empty Map.empty TyVoid False

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
mergeTypes (TyPair f1 s1) (TyPair f2 s2)
  = TyPair <$> mergeTypes f1 f2 <*> mergeTypes s1 s2
mergeTypes (TyChan t1) (TyChan t2)
  = TyChan <$> mergeTypes t1 t2
mergeTypes t1 t2
  | t1 == t2  = OK t1
  | otherwise = semanticError ("Types " ++ show t1 ++ " and " ++ 
                                      show t2 ++ " are not compatible")

checkType :: Context -> Type -> WACCResult Type
checkType ctx (TyArray elemTy)
  = TyArray <$> checkType ctx elemTy
checkType ctx (TyPair fstTy sndTy)
  = TyPair <$> checkType ctx fstTy <*> checkType ctx sndTy
checkType ctx (TyChan elemTy)
  = TyChan <$> checkType ctx elemTy
checkType ctx (TyName name)
  = getTypeAlias name ctx
checkType _ ty = return ty


isArrayType :: Type -> Bool
isArrayType (TyArray _) = True
isArrayType TyAny       = True
isArrayType _           = False

isHeapType :: Type -> Bool
isHeapType (TyPair _ _) = True
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

isAbstractType :: Type -> Bool
isAbstractType TyAny          = True
isAbstractType TyVoid         = True
isAbstractType (TyPair ta tb) = isAbstractType ta || isAbstractType tb
isAbstractType (TyArray ty)   = isAbstractType ty
isAbstractType _              = False

isVoidType :: Type -> Bool
isVoidType TyVoid = True
isVoidType _      = False


checkLiteral :: Literal -> Type
checkLiteral (LitInt _)    = TyInt
checkLiteral (LitBool _)   = TyBool
checkLiteral (LitChar _)   = TyChar
checkLiteral (LitString _) = TyArray TyChar
checkLiteral  LitNull      = TyPair TyAny TyAny


checkExpr :: Annotated Expr SpanA -> Context -> WACCResult (Annotated Expr TypeA)
checkExpr (_, ExprLit lit) _ = return (checkLiteral lit, ExprLit lit)

checkExpr (_, ExprVar varname) context = do
  ty <- getVariable varname context
  return (ty, ExprVar varname)

checkExpr (_, ExprArrayElem arrayElem) context = do
  arrayElem'@(ty, _) <- checkArrayElem arrayElem context
  return (ty, ExprArrayElem arrayElem')

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


checkArrayElem :: Annotated ArrayElem SpanA -> Context -> WACCResult (Annotated ArrayElem TypeA)
checkArrayElem (_, ArrayElem varname exprs) context = do
  baseTy <- getVariable varname context
  exprs' <- mapM (\e -> checkExpr e context) exprs
  ty <- checkArrayIndexing baseTy (map fst exprs') context
  return (ty, ArrayElem varname exprs')

checkArrayIndexing :: Type -> [Type] -> Context -> WACCResult Type
checkArrayIndexing (TyArray innerType) (indexType : tys) context = do
  if compatibleType TyInt indexType
  then checkArrayIndexing innerType tys context
  else semanticError ("Cannot index array with type " ++ show indexType)
checkArrayIndexing TyAny _ _ = OK TyAny
checkArrayIndexing t [] _    = OK t
checkArrayIndexing t _ _     = semanticError ("Cannot index variable of type " ++ show t)


checkPairElem :: Annotated PairElem SpanA -> Context -> WACCResult (Annotated PairElem TypeA)
checkPairElem (_, PairElem side expr) context = do
  expr'@(outerType, _) <- checkExpr expr context
  outerType' <- checkType context outerType
  innerType <- case (side, outerType') of
    (PairFst, TyPair f _) -> return f
    (PairSnd, TyPair _ s) -> return s
    (_, TyAny           ) -> return TyAny
    (_, _               ) -> semanticError ("Type " ++ show outerType ++ " is not pair")
  innerType' <- checkType context innerType
  return ((innerType', outerType'), PairElem side expr')


checkAssignLHS :: Annotated AssignLHS SpanA -> Context -> WACCResult (Annotated AssignLHS TypeA)
checkAssignLHS (_, LHSVar varname) context = do
  ty <- getVariable varname context
  return (ty, LHSVar varname)

checkAssignLHS (_, LHSPairElem pairElem) context = do
  pairElem'@((ty, _), _) <- checkPairElem pairElem context
  return (ty, LHSPairElem pairElem')

checkAssignLHS (_, LHSArrayElem arrayElem) context = do
  arrayElem'@(ty, _) <- checkArrayElem arrayElem context
  return (ty, LHSArrayElem arrayElem')


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

checkAssignRHS (_, RHSCall fname args) context = do
  (symbolName, args', returnType) <- checkCall fname args context
  return (returnType, RHSCall symbolName args')

checkAssignRHS (_, RHSAwait fname args) context = do
  (symbolName, args', returnType) <- checkAwait fname args context
  return (returnType, RHSAwait symbolName args')

checkAssignRHS (_, RHSNewChan) context = return (TyChan TyAny, RHSNewChan)

checkAssignRHS (_, RHSChanRecv chanName) context = do
  chanTy <- getVariable chanName context
  elemTy <- case chanTy of
    TyChan ty -> return ty
    _         -> semanticError ("Cannot receive from non-channel type " ++ show chanTy)
  return (elemTy, RHSChanRecv chanName)

checkArgs :: Context -> [Type] -> [Type] -> WACCResult ()
checkArgs ctx actual expected = checkArgs' 0 actual expected 
  where
    checkArgs' :: Int -> [Type] -> [Type] -> WACCResult ()
    checkArgs' _ [] [] = OK ()
    checkArgs' n (a1:as1) (a2:as2)
      | compatibleType a1 a2 = checkArgs' (n+1) as1 as2
      | otherwise = semanticError ("Expected type " ++ show a2 
                                ++ " but got type " ++ show a1 
                                ++ " for argument " ++ show (n + 1))
    checkArgs' n _ _
      = semanticError ("Wrong number of arguments."
                    ++ " Expected " ++ show (length expected)
                    ++ " but got " ++ show (length actual))

checkCall :: Identifier -> [Annotated Expr SpanA] -> Context -> WACCResult (Identifier, [Annotated Expr TypeA], Type)
checkCall fname args context = do
  (symbolName, async, expectedArgsType, returnType) <- getFunction fname context
  when async (semanticError ("Cannot call asynchronous function " ++ fname))

  args' <- mapM (\e -> checkExpr e context) args
  checkArgs context expectedArgsType (map fst args')

  return (symbolName, args', returnType)

checkAwait :: Identifier -> [Annotated Expr SpanA] -> Context -> WACCResult (Identifier, [Annotated Expr TypeA], Type)
checkAwait fname args context = do
  (symbolName, async, expectedArgsType, returnType) <- getFunction fname context
  unless async (semanticError ("Cannot await synchronous function " ++ fname))

  args' <- mapM (\e -> checkExpr e context) args
  checkArgs context expectedArgsType (map fst args')

  return (symbolName, args', returnType)

checkFire :: Identifier -> [Annotated Expr SpanA] -> Context -> WACCResult (Identifier, [Annotated Expr TypeA], Type)
checkFire fname args context = do
  (symbolName, async, expectedArgsType, returnType) <- getFunction fname context
  unless async (semanticError ("Cannot fire synchronous function " ++ fname))
  when (length args > 1) (semanticError ("Function " ++ fname ++ " with more than one argument cannot be fired"))

  args' <- mapM (\e -> checkExpr e context) args
  checkArgs context expectedArgsType (map fst args')

  return (symbolName, args', returnType)

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
  varType' <- lift $ checkType context varType
  rhs'@(rhsType, _) <- lift $ checkAssignRHS rhs context

  ty <- lift $ mergeTypes varType' rhsType

  when (isAbstractType ty)
       (lift (semanticError ("Cannot declare variable " ++ varname
                          ++ " of incomplete type " ++ show ty)))

  addVariable varname ty
  return (False, StmtVar ty varname rhs')

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

checkStmt (_, StmtPrint exprs ln) = do
  context <- get
  exprs' <- lift $ mapM (\e -> checkExpr e context) exprs
  return (False, StmtPrint exprs' ln)

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

checkStmt (_, StmtCall fname args) = do
  context <- get
  (symbolName, args', _) <- lift $ checkCall fname args context
  return (False, StmtCall symbolName args')

checkStmt (_, StmtAwait fname args) = do
  context <- get
  (symbolName, args', _) <- lift $ checkAwait fname args context
  return (False, StmtAwait symbolName args')

checkStmt (_, StmtFire fname args) = do
  context <- get
  (symbolName, args', _) <- lift $ checkFire fname args context
  return (False, StmtFire symbolName args')

checkStmt (_, StmtChanSend chanName rhs) = do
  context <- get
  chanTy <- lift $ getVariable chanName context
  rhs'@(rhsTy, _) <- lift $ checkAssignRHS rhs context

  lift $ case chanTy of
    TyChan ty -> mergeTypes ty rhsTy
    _         -> semanticError ("Cannot send to non-channel type " ++ show chanTy)

  return (False, StmtChanSend chanName rhs')

checkFunction :: Annotated FuncDef SpanA -> Context -> WACCResult (Annotated FuncDef TypeA)
checkFunction (_, FuncDef expectedReturnType async name args block) globalContext
  = withErrorContext ("In function " ++ show name) $ do
      expectedReturnType' <- checkType globalContext expectedReturnType
      args' <- mapM (\(ty, name) -> (,name) <$> checkType globalContext ty) args

      let setupContext = do
           modify (\c -> c { returnType = expectedReturnType',
                             asyncContext = async })
           forM_ args' (\(argType, argName) -> addVariable argName argType)

      context <- execStateT setupContext globalContext 
      block'@((alwaysReturns, _), _) <- checkBlock block context

      unless (isVoidType expectedReturnType' || alwaysReturns)
             (syntaxError ("Function " ++ show name ++ " does not always return"))

      return ((), FuncDef expectedReturnType' async name args' block')

checkTypeAlias :: Annotated TypeDef SpanA -> Context -> WACCResult (Annotated TypeDef TypeA)
checkTypeAlias (_, TypeDef name typeAlias) context = do
  typeAlias' <- checkType context typeAlias
  return ((), TypeDef name typeAlias)

checkFFIFunc :: Annotated FFIFunc SpanA -> Context -> WACCResult (Annotated FFIFunc TypeA)
checkFFIFunc (_, FFIFunc returnType async name argTypes symbolName) context = do
  returnType' <- checkType context returnType
  argTypes' <- mapM (checkType context) argTypes
  return ((), FFIFunc returnType' async name argTypes symbolName)

checkDecl :: Annotated Decl SpanA -> Context -> WACCResult (Annotated Decl TypeA)
checkDecl (_, DeclFunc f) ctx = ((),) <$> DeclFunc <$> checkFunction f ctx
checkDecl (_, DeclType f) ctx = ((),) <$> DeclType <$> checkTypeAlias f ctx
checkDecl (_, DeclFFIFunc f) ctx = ((),) <$> DeclFFIFunc <$> checkFFIFunc f ctx

defineFunction :: Annotated FuncDef SpanA -> ContextState ()
defineFunction (_, FuncDef returnType async (FuncName name) args _) = do
  context <- get
  returnType' <- lift $ checkType context returnType
  argTypes <- lift $ mapM (checkType context . fst) args
  addFunction name ("f_" ++ name, async, argTypes, returnType')
defineFunction (_, FuncDef _ _ MainFunc _ _) = return ()

defineTypeAlias :: Annotated TypeDef SpanA -> ContextState ()
defineTypeAlias (_, TypeDef name typeAlias) = do
  context <- get
  typeAlias' <- lift $ checkType context typeAlias
  addTypeAlias name typeAlias'

defineFFIFunc :: Annotated FFIFunc SpanA -> ContextState ()
defineFFIFunc (_, FFIFunc returnType async name argTypes symbolName) = do
  context <- get
  returnType' <- lift $ checkType context returnType
  argTypes' <- lift $ mapM (checkType context) argTypes
  addFunction name (symbolName, async, argTypes', returnType')

defineDecl :: Annotated Decl SpanA -> ContextState ()
defineDecl (_, DeclFunc f) = defineFunction f
defineDecl (_, DeclType f) = defineTypeAlias f
defineDecl (_, DeclFFIFunc f) = defineFFIFunc f

checkProgram :: Annotated Program SpanA -> WACCResult (Annotated Program TypeA)
checkProgram (_, Program decls) = do
  context <- execStateT (forM_ decls defineDecl) emptyContext
  decls' <- forM decls (\d -> checkDecl d context)
  return ((), Program decls')

waccSemCheck :: Annotated Program SpanA -> WACCResult (Annotated Program TypeA)
waccSemCheck = checkProgram
