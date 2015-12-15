{-# LANGUAGE TupleSections #-}

module Frontend.SemCheck where

import Arguments
import Common.AST
import Common.ScopedMap as ScopedMap
import Common.WACCResult
import Common.Stuff

import Control.Monad.State
import Control.Monad.Reader
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

type SemCheck = ReaderT Context (WACCResultT WACCArguments)
type SemCheckState = StateT Context (WACCResultT WACCArguments)

liftSemCheck :: SemCheck a -> SemCheckState a
liftSemCheck m = do
  context <- get
  lift $ runReaderT m context

addVariable :: String -> Type -> SemCheckState ()
addVariable name value = do
  vars <- gets variables
  case ScopedMap.insertIfNotExists name value vars of
    Just table -> modify (\c -> c { variables = table })
    Nothing ->  semanticError ("Variable " ++ show name ++ " already exists")

addFunction :: String -> (String, Bool, [Type], Type) -> SemCheckState ()
addFunction name typ = do
  fns <- gets functions
  case Map.lookup name fns of
    Just _  -> semanticError ("Function " ++ show name ++ " already exists")
    Nothing -> modify (\c -> c { functions = Map.insert name typ fns })

addTypeAlias :: String -> Type -> SemCheckState ()
addTypeAlias name typeAlias = do
  types <- gets typeAliases
  case Map.lookup name types of
    Just _  -> semanticError ("Type " ++ show name ++ " already exists")
    Nothing -> modify (\c -> c { typeAliases = Map.insert name typeAlias types })

getVariable :: String -> SemCheck Type
getVariable name = do
  vars <- asks variables
  case ScopedMap.lookup name vars of
    Just value -> return value
    Nothing    -> semanticError ("Unknown variable " ++ show name)

getFunction :: String -> SemCheck (String, Bool, [Type], Type)
getFunction name = do
  fns <- asks functions
  case Map.lookup name fns of
    Just value -> return value
    Nothing    -> semanticError ("Unknown function " ++ show name)

getTypeAlias :: String -> SemCheck Type
getTypeAlias name = do
  types <- asks typeAliases
  case Map.lookup name types of
    Just value -> return value
    Nothing    -> semanticError ("Unknown type " ++ show name)

emptyContext :: Context
emptyContext = Context ScopedMap.empty Map.empty Map.empty TyVoid False

newContext :: Context -> Context
newContext parent
  = parent { variables = newScope (variables parent) }

compatibleType :: Type -> Type -> SemCheck Bool
compatibleType TyAny _ = return True
compatibleType _ TyAny = return True
compatibleType TyNull (TyTuple _) = return True
compatibleType (TyTuple _) TyNull = return True

compatibleType (TyArray t1) (TyArray t2)
  = compatibleType t1 t2 
compatibleType (TyTuple ts1) (TyTuple ts2)
  = (andM (compatibleType <$> ts1 <*> ts2))
    <^(&&)
    (length ts1 == length ts2)

compatibleType (TyChan t1) (TyChan t2)
  = compatibleType t1 t2 

compatibleType t1 t2 = return (t1 == t2)

mergeTypes :: Type -> Type -> SemCheck Type
mergeTypes t1 TyAny = return t1
mergeTypes TyAny t2 = return t2
mergeTypes TyNull (TyTuple tys)
  = return (TyTuple tys)
mergeTypes (TyTuple tys) TyNull
  = return (TyTuple tys)

mergeTypes (TyArray t1) (TyArray t2)
  = TyArray <$> mergeTypes t1 t2
mergeTypes t1@(TyTuple ts1) t2@(TyTuple ts2) = do
  unless (length ts1 == length ts2)
         (semanticError ("Tuples " ++ show t1 ++ " and " ++ show t2 ++ " are of different sizes"))
  TyTuple <$> sequence (zipWith mergeTypes ts1 ts2)
mergeTypes (TyChan t1) (TyChan t2)
  = TyChan <$> mergeTypes t1 t2

mergeTypes t1 t2
  | t1 == t2  = return t1
  | otherwise = semanticError ("Types " ++ show t1 ++ " and " ++ show t2 ++ " are not compatible")

checkType :: Type -> SemCheck Type
checkType (TyArray elemTy)
  = TyArray <$> checkType elemTy
checkType (TyTuple elemTys)
  = TyTuple <$> mapM checkType elemTys
checkType (TyChan elemTy)
  = TyChan <$> checkType elemTy
checkType (TyName name)
  = getTypeAlias name
checkType ty = return ty

isArrayType :: Type -> SemCheck Bool
isArrayType = compatibleType (TyArray TyAny)

isHeapType :: Type -> SemCheck Bool
isHeapType (TyTuple _ ) = return True
isHeapType (TyArray _)  = return True
isHeapType TyAny        = return True
isHeapType _            = return False

isOrderedType :: Type -> SemCheck Bool
isOrderedType TyInt  = return True
isOrderedType TyChar = return True
isOrderedType TyAny  = return True
isOrderedType _      = return False

isReadableType :: Type -> SemCheck Bool
isReadableType TyInt  = return True
isReadableType TyChar = return True
isReadableType TyAny  = return True
isReadableType _      = return False

isAbstractType :: Type -> SemCheck Bool
isAbstractType TyAny         = return True
isAbstractType TyVoid        = return True
isAbstractType (TyTuple tys) = anyM isAbstractType tys
isAbstractType (TyArray ty)  = isAbstractType ty
isAbstractType _             = return False

isVoidType :: Type -> SemCheck Bool
isVoidType TyVoid = return True
isVoidType _      = return False

checkLiteral :: Literal -> Type
checkLiteral (LitInt _)    = TyInt
checkLiteral (LitBool _)   = TyBool
checkLiteral (LitChar _)   = TyChar
checkLiteral (LitString _) = TyArray TyChar
checkLiteral  LitNull      = TyNull

checkIndexingElem :: Annotated IndexingElem SpanA -> SemCheck (Annotated IndexingElem TypeA)
checkIndexingElem (_, IndexingElem varname exprs) = do
  baseTy <- getVariable varname
  exprs' <- mapM checkExpr exprs
  ty <- checkIndexingElem' baseTy exprs'
  return (ty, IndexingElem varname exprs')

checkIndexingElem' :: Type -> [Annotated Expr TypeA] -> SemCheck (Type, [Type])
checkIndexingElem' baseType@(TyTuple ts) exprs@((ty, e):es) = do
  compatible <- compatibleType TyInt ty
  unless compatible (semanticError ("Cannot index tuple with variable of type " ++ show ty))

  index <- case e of
    ExprLit (LitInt i) -> return (fromInteger i)
    _ -> semanticError "Index must be an integer literal"

  unless (index >= 0 && index < length ts)
         (semanticError "Index out of bounds")

  (elemType, baseTypes) <- checkIndexingElem' (ts !! index) es
  return (elemType, baseType : baseTypes)

checkIndexingElem' baseType@(TyArray t) exprs@((ty, e):es) = do
  compatible <- compatibleType TyInt ty
  unless compatible (semanticError ("Cannot index array with variable of type " ++ show ty))
  (elemType, baseTypes) <- checkIndexingElem' t es
  return (elemType, baseType : baseTypes)

checkIndexingElem' t [] = return (t, [])
checkIndexingElem' t _  = semanticError ("Type " ++ show t ++ " not indexable")

checkExpr :: Annotated Expr SpanA -> SemCheck (Annotated Expr TypeA)
checkExpr (_, ExprLit lit) = return (checkLiteral lit, ExprLit lit)

checkExpr (_, ExprVar varname) = do
  ty <- getVariable varname
  return (ty, ExprVar varname)

checkExpr (_, ExprIndexingElem indexElem) = do
  indexElem'@((t, tys), ts) <- checkIndexingElem indexElem
  return (t, ExprIndexingElem indexElem')

checkExpr (_, ExprUnOp op expr) = do
  expr'@(t1, _) <- checkExpr expr
  ty <- checkUnOp op t1
  return (ty, ExprUnOp op expr')

checkExpr (_, ExprBinOp op e1 e2) = do
  e1'@(t1,_) <- checkExpr e1
  e2'@(t2,_) <- checkExpr e2
  ty <- checkBinOp op t1 t2
  return (ty, ExprBinOp op e1' e2')


unOpType :: UnOp -> (Type -> SemCheck Bool, Type)
unOpType UnOpNot = (compatibleType TyBool, TyBool)
unOpType UnOpNeg = (compatibleType TyInt, TyInt)
unOpType UnOpOrd = (compatibleType TyChar, TyInt)
unOpType UnOpChr = (compatibleType TyInt, TyChar)
unOpType UnOpLen = (isArrayType, TyInt)

checkUnOp :: UnOp -> Type -> SemCheck Type
checkUnOp op typ = do
  let (predicate, result) = unOpType op
  ok <- predicate typ
  unless ok (semanticError ("Cannot apply unary operator " ++ show op ++ " to type " ++ show typ))
  return result

binOpType :: BinOp -> (Type -> Type -> SemCheck Bool, Type)
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
                              <^(&&)^>
                              compatibleType TyInt t2
                           , TyInt)
    booleanOp    = (\t1 t2 -> compatibleType TyBool t1
                              <^(&&)^>
                              compatibleType TyBool t2
                           , TyBool)
    orderOp      = (\t1 t2 -> compatibleType t1 t2
                              <^(&&)^>
                              isOrderedType t1
                              <^(&&)^>
                              isOrderedType t2
                           , TyBool)
    equalityOp   = (compatibleType, TyBool)

checkBinOp :: BinOp -> Type -> Type -> SemCheck Type
checkBinOp op t1 t2 = do
  let (predicate, result) = binOpType op
  ok <- predicate t1 t2
  unless ok (semanticError ("Cannot apply binary operator " ++ show op ++ " to types " ++ show t1 ++ " and " ++ show t2))
  return result

checkAssignLHS :: Annotated AssignLHS SpanA -> SemCheck (Annotated AssignLHS TypeA)
checkAssignLHS (_, LHSVar varname) = do
  ty <- getVariable varname
  return (ty, LHSVar varname)

checkAssignLHS (_, LHSIndexingElem indexElem) = do
  indexElem'@((ty, tys), _) <- checkIndexingElem indexElem
  return (ty, LHSIndexingElem indexElem')

checkAssignRHS :: Annotated AssignRHS SpanA -> SemCheck (Annotated AssignRHS TypeA)
checkAssignRHS (_, RHSExpr expr) = do
  expr'@(ty, _) <- checkExpr expr
  return (ty, RHSExpr expr')

checkAssignRHS (_, RHSArrayLit exprs) = do
  exprs' <- mapM checkExpr exprs
  innerType <- foldM mergeTypes TyAny (map fst exprs')
  return (TyArray innerType, RHSArrayLit exprs')

checkAssignRHS (_, RHSNewTuple exprs) = do
  exprs' <- mapM checkExpr exprs
  return (TyTuple (map fst exprs'), RHSNewTuple exprs')

checkAssignRHS (_, RHSCall fname args) = do
  (symbolName, args', returnType) <- checkCall fname args
  return (returnType, RHSCall symbolName args')

checkAssignRHS (_, RHSAwait fname args) = do
  (symbolName, args', returnType) <- checkAwait fname args
  return (returnType, RHSAwait symbolName args')

checkAssignRHS (_, RHSNewChan) = return (TyChan TyAny, RHSNewChan)

checkAssignRHS (_, RHSChanRecv chanName) = do
  chanTy <- getVariable chanName
  (TyChan elemTy) <- mergeTypes chanTy (TyChan TyAny)
  unlessM (asks asyncContext)
          (semanticError ("Cannot receive from channel from within a synchronous function"))
  return (elemTy, RHSChanRecv chanName)

checkArgs :: [Type] -> [Type] -> SemCheck ()
checkArgs actual expected = checkArgs' 0 actual expected
  where
    checkArgs' :: Int -> [Type] -> [Type] -> SemCheck ()
    checkArgs' _ [] [] = return ()
    checkArgs' n (a1:as1) (a2:as2) = do
      unlessM (compatibleType a1 a2)
              (semanticError ("Expected type " ++ show a2
                           ++ " but got type " ++ show a1
                           ++ " for argument " ++ show (n + 1)))
      checkArgs' (n+1) as1 as2

    checkArgs' n _ _
      = semanticError ("Wrong number of arguments."
                    ++ " Expected " ++ show (length expected)
                    ++ " but got " ++ show (length actual))

checkCall :: Identifier -> [Annotated Expr SpanA] -> SemCheck (Identifier, [Annotated Expr TypeA], Type)
checkCall fname args = do
  (symbolName, async, expectedArgsType, returnType) <- getFunction fname
  when async (semanticError ("Cannot call asynchronous function " ++ fname))

  args' <- mapM checkExpr args
  checkArgs expectedArgsType (map fst args')

  return (symbolName, args', returnType)

checkAwait :: Identifier -> [Annotated Expr SpanA] -> SemCheck (Identifier, [Annotated Expr TypeA], Type)
checkAwait fname args = do
  unlessM (asks asyncContext)
          (semanticError ("Cannot await from within a synchronous function"))

  (symbolName, async, expectedArgsType, returnType) <- getFunction fname
  unless async (semanticError ("Cannot await synchronous function " ++ fname))

  args' <- mapM checkExpr args
  checkArgs expectedArgsType (map fst args')

  return (symbolName, args', returnType)

checkFire :: Identifier -> [Annotated Expr SpanA] -> SemCheck (Identifier, [Annotated Expr TypeA], Type)
checkFire fname args = do
  (symbolName, async, expectedArgsType, returnType) <- getFunction fname
  unless async (semanticError ("Cannot fire synchronous function " ++ fname))
  unlessM (lift . lift $ getArgument runtimeEnabled)
          (semanticError "Cannot use fire if runtime is disabled")

  when (length args > 1) (semanticError ("Function " ++ fname ++ " with more than one argument cannot be fired"))

  args' <- mapM checkExpr args
  checkArgs expectedArgsType (map fst args')

  return (symbolName, args', returnType)

checkBlock :: Annotated Block SpanA -> SemCheck (Annotated Block TypeA)
checkBlock (_, Block stmts) = do
  context <- ask
  let childContext = newContext context
  (stmts', finalContext) <- lift $ runStateT (mapM checkPosStmt stmts) childContext

  let alwaysReturns = or (map fst stmts')
  let locals = Map.assocs (ScopedMap.localTable (variables finalContext))

  return ((alwaysReturns, locals), Block stmts')

checkCaseArm :: Type -> Annotated CaseArm SpanA -> SemCheck (Annotated CaseArm TypeA)
checkCaseArm expectedType (_,  CaseArm l b) = do
  b' <- checkBlock b
  let litType = checkLiteral l
  ty <- mergeTypes litType expectedType
  return (ty, CaseArm l b')

checkPosStmt :: Annotated Stmt SpanA -> SemCheckState (Annotated Stmt TypeA)
checkPosStmt stmt@(((line,column,fname),_), _)
  = withErrorContext
      ("at " ++ show fname ++ " (line " ++ show line ++  ", column "  ++ show column ++ ")")
      (checkStmt stmt)


checkStmt :: Annotated Stmt SpanA -> SemCheckState (Annotated Stmt TypeA)
checkStmt (_, StmtSkip) = return (False, StmtSkip)

checkStmt (_, StmtVar varType varname rhs) = do
  varType' <- liftSemCheck $ checkType varType
  rhs'@(rhsType, _) <- liftSemCheck $ checkAssignRHS rhs

  ty <- liftSemCheck $ mergeTypes varType' rhsType

  whenM (liftSemCheck $ isAbstractType ty)
        (semanticError ("Cannot declare variable " ++ varname
                     ++ " of incomplete type " ++ show ty))

  addVariable varname ty
  return (False, StmtVar ty varname rhs')

checkStmt (_, StmtAssign lhs rhs) = do
  lhs'@(lhsType, _) <- liftSemCheck $ checkAssignLHS lhs
  rhs'@(rhsType, _) <- liftSemCheck $ checkAssignRHS rhs

  unlessM (liftSemCheck $ compatibleType lhsType rhsType)
          (semanticError ("Cannot assign RHS of type " ++ show rhsType
                       ++ " to LHS of type " ++ show lhsType))
  return (False, StmtAssign lhs' rhs')

checkStmt (_, StmtRead lhs) = do
  lhs'@(lhsType, _) <- liftSemCheck $ checkAssignLHS lhs
  unlessM (liftSemCheck $ isReadableType lhsType)
       (lift (semanticError ("Cannot read variable of type "
                                                             ++ show lhsType)))
  return (False, StmtRead lhs')

checkStmt (_, StmtFree expr) = do
  expr'@(exprType, _) <- liftSemCheck $ checkExpr expr
  unlessM (liftSemCheck $ isHeapType exprType)
       (lift (semanticError ("Cannot free variable of type " ++ show exprType)))
  return (False, StmtFree expr')

checkStmt (_, StmtReturn expr) = do
  expr'@(exprType, _) <- liftSemCheck $ checkExpr expr
  expectedReturnType <- gets returnType
  retType <- liftSemCheck $ mergeTypes expectedReturnType exprType
  return (True, StmtReturn expr')

checkStmt (_, StmtExit expr) = do
  expr'@(exprType, _) <- liftSemCheck $ checkExpr expr
  unlessM (liftSemCheck $ compatibleType TyInt exprType)
       (lift (semanticError ("Expected int in exit statement, got " ++ show exprType)))
  return (True, StmtExit expr')

checkStmt (_, StmtPrint exprs ln) = do
  exprs' <- liftSemCheck $ mapM checkExpr exprs
  return (False, StmtPrint exprs' ln)

checkStmt (_, StmtIf predicate b1 maybeB2) = do
  predicate'@(predicateType, _) <- liftSemCheck $ checkExpr predicate
  unlessM (liftSemCheck $ compatibleType TyBool predicateType)
       (lift (semanticError ("Condition cannot be of type " ++ show predicateType)))
  case maybeB2 of
    Just b2 -> do
      b1'@((al1,_),_) <- liftSemCheck $ checkBlock b1
      b2'@((al2,_),_) <- liftSemCheck $ checkBlock b2
      let al = al1 && al2
      return (al, StmtIf predicate' b1' (Just b2'))

    Nothing -> do
      b <- liftSemCheck $ checkBlock b1
      return (False, StmtIf predicate' b Nothing)

checkStmt (_, StmtSwitch value cs) = do
  value'@(valueType, _) <- liftSemCheck $ checkExpr value
  cs' <- liftSemCheck $ mapM (checkCaseArm valueType) cs
  return (False, StmtSwitch value' cs')

checkStmt (_, StmtWhile predicate block) = do
  predicate'@(predicateType, _) <- liftSemCheck $ checkExpr predicate
  unlessM (liftSemCheck $ compatibleType TyBool predicateType)
       (lift (semanticError ("Condition cannot be of type " ++ show predicateType)))

  block' <- liftSemCheck $ checkBlock block
  return (False, StmtWhile predicate' block')

checkStmt (_, StmtScope block) = do
  block'@((al, _),_) <- liftSemCheck $ checkBlock block
  return (al, StmtScope block')

checkStmt (_, StmtCall fname args) = do
  (symbolName, args', _) <- liftSemCheck $ checkCall fname args
  return (False, StmtCall symbolName args')

checkStmt (_, StmtAwait fname args) = do
  (symbolName, args', _) <- liftSemCheck $ checkAwait fname args
  return (False, StmtAwait symbolName args')

checkStmt (_, StmtFire fname args) = do
  (symbolName, args', _) <- liftSemCheck $ checkFire fname args
  return (False, StmtFire symbolName args')

checkStmt (_, StmtChanSend chanName rhs) = do
  chanTy <- liftSemCheck $ getVariable chanName
  rhs'@(rhsTy, _) <- liftSemCheck $ checkAssignRHS rhs

  liftSemCheck $ mergeTypes chanTy (TyChan rhsTy)
  unlessM (gets asyncContext)
          (semanticError ("Cannot send to channel from within a synchronous function"))

  return (False, StmtChanSend chanName rhs')

checkFunction :: Annotated FuncDef SpanA -> SemCheck (Annotated FuncDef TypeA)
checkFunction (_, FuncDef expectedReturnType async name args block)
  = withErrorContext ("In function " ++ show name) $ do
      expectedReturnType' <- checkType expectedReturnType
      args' <- mapM (\(ty, name) -> (,name) <$> checkType ty) args

      let setupContext = do
           modify (\c -> c { returnType = expectedReturnType',
                             asyncContext = async })
           forM_ args' (\(argType, argName) -> addVariable argName argType)

      globalContext <- ask
      context <- lift $ execStateT setupContext globalContext
      block'@((alwaysReturns, _), _) <- checkBlock block

      unlessM (isVoidType expectedReturnType' <^(||) alwaysReturns)
              (syntaxError ("Function " ++ show name ++ " does not always return"))

      return ((), FuncDef expectedReturnType' async name args' block')

checkTypeAlias :: Annotated TypeDef SpanA -> SemCheck (Annotated TypeDef TypeA)
checkTypeAlias (_, TypeDef name typeAlias) = do
  typeAlias' <- checkType typeAlias
  return ((), TypeDef name typeAlias)

checkFFIFunc :: Annotated FFIFunc SpanA -> SemCheck (Annotated FFIFunc TypeA)
checkFFIFunc (_, FFIFunc returnType async name argTypes symbolName) = do
  returnType' <- checkType returnType
  argTypes' <- mapM checkType argTypes
  return ((), FFIFunc returnType' async name argTypes symbolName)

checkDecl :: Annotated Decl SpanA -> SemCheck (Annotated Decl TypeA)
checkDecl (_, DeclFunc f)    = ((),) <$> DeclFunc <$> checkFunction f
checkDecl (_, DeclType f)    = ((),) <$> DeclType <$> checkTypeAlias f
checkDecl (_, DeclFFIFunc f) = ((),) <$> DeclFFIFunc <$> checkFFIFunc f

defineFunction :: Annotated FuncDef SpanA -> SemCheckState ()
defineFunction (_, FuncDef returnType async (FuncName name) args _) = do
  returnType' <- liftSemCheck $ checkType returnType
  argTypes <- liftSemCheck $ mapM (checkType . fst) args
  addFunction name ("f_" ++ name, async, argTypes, returnType')
defineFunction (_, FuncDef _ _ MainFunc _ _) = return ()

defineTypeAlias :: Annotated TypeDef SpanA -> SemCheckState ()
defineTypeAlias (_, TypeDef name typeAlias) = do
  typeAlias' <- liftSemCheck $ checkType typeAlias
  addTypeAlias name typeAlias'

defineFFIFunc :: Annotated FFIFunc SpanA -> SemCheckState ()
defineFFIFunc (_, FFIFunc returnType async name argTypes symbolName) = do
  returnType' <- liftSemCheck $ checkType returnType
  argTypes' <- liftSemCheck $ mapM checkType argTypes
  addFunction name (symbolName, async, argTypes', returnType')

defineDecl :: Annotated Decl SpanA -> SemCheckState ()
defineDecl (_, DeclFunc f) = defineFunction f
defineDecl (_, DeclType f) = defineTypeAlias f
defineDecl (_, DeclFFIFunc f) = defineFFIFunc f

checkProgram :: Annotated Program SpanA -> WACCResultT WACCArguments (Annotated Program TypeA)
checkProgram (_, Program decls) = do
  context <- execStateT (forM_ decls defineDecl) emptyContext
  decls' <- runReaderT (forM decls checkDecl) context
  return ((), Program decls')

waccSemCheck :: Annotated Program SpanA -> WACCResultT WACCArguments (Annotated Program TypeA)
waccSemCheck = checkProgram
