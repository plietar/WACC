{-# LANGUAGE TupleSections #-}

module Frontend.SemCheck where

import Arguments
import Common.AST
import Common.ScopedMap as ScopedMap
import Common.WACCResult
import Common.Stuff

import Prelude hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)
import Control.Monad.Trans
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Traversable(mapM)

data Context = Context
  { variables    :: ScopedMap String Type
  , functions    :: Map String (String, Bool, [Type], Type)
  , typeAliases  :: Map String Type
  , returnType   :: Type
  , asyncContext :: Bool
  }

type SemCheck = ReaderT Context (WriterT (Set Identifier, Set Type) (WACCResultT WACCArguments))
type SemCheckState = StateT Context (WriterT (Set Identifier, Set Type) (WACCResultT WACCArguments))

emitMemberNames names
  = tell (Set.fromList names, Set.empty)

emitTypeTag ty
  = tell (Set.empty, Set.singleton ty)

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

isSubtypeOf :: Type -> Type -> SemCheck Bool
isSubtypeOf TyAny _ = return True

isSubtypeOf (TyArray t1) (TyArray TyAny)
  = return True
isSubtypeOf (TyArray t1) (TyArray t2)
  = isSubtypeOf t1 t2

isSubtypeOf (TyChan t1) (TyChan t2)
  = isSubtypeOf t1 t2

isSubtypeOf (TyTuple _) TyNull = return True
isSubtypeOf (TyTuple [_,_]) (TyTuple [TyAny, TyAny]) = return True
isSubtypeOf (TyTuple ts1) (TyTuple ts2)
  = (&&) (length ts1 == length ts2) <$>
    andM (zipWith isSubtypeOf ts1 ts2)

isSubtypeOf (TyStruct ts1) (TyStruct ts2)
  = isStructSubtype (Map.assocs ts1) (Map.assocs ts2)

isSubtypeOf (TyUnion ts1) (TyUnion ts2)
  = return (Set.isSubsetOf ts1 ts1)

isSubtypeOf t1 t2 = return (t1 == t2)

checkIsSubtypeOf :: Type -> Type -> SemCheck ()
checkIsSubtypeOf t1 t2
  = unlessM (isSubtypeOf t1 t2) (semanticError (show t2 ++ " is not a subtype of " ++ show t1))

-- Check if the second struct is a subtype of the first
-- Make sure everyfield in the first type is present in
-- the second
isStructSubtype :: [(Identifier, Type)] -> [(Identifier, Type)] -> SemCheck Bool
isStructSubtype []    _  = return True
isStructSubtype (_:_) [] = return False
isStructSubtype ((n1, t1) : s1) ((n2, t2) : s2)
  | n1 == n2  = (&&) <$> isSubtypeOf t1 t2 <*> isStructSubtype s1 s2
  | otherwise = isStructSubtype ((n1, t1) : s1) s2

intersectTypes :: Type -> Type -> SemCheck Type
intersectTypes TyAny ty = return ty
intersectTypes ty TyAny = return ty
intersectTypes ty@(TyTuple _) TyNull = return ty
intersectTypes TyNull ty@(TyTuple _) = return ty
intersectTypes (TyTuple ts1) (TyTuple ts2)
  | length ts1 /= length ts2 = semanticError ("Tuples " ++ show ts1 ++ " and " ++ show ts2 ++ " have different lengths")
  | otherwise = TyTuple <$> zipWithM intersectTypes ts1 ts2
intersectTypes (TyChan t1) (TyChan t2) = TyChan <$> (intersectTypes t1 t2)
intersectTypes (TyStruct ts1) (TyStruct ts2)
  = (TyStruct . Map.fromList) <$> intersectStructs (Map.assocs ts1) (Map.assocs ts2)
intersectTypes t1 t2
  | t1 == t2  = return t1
  | otherwise = semanticError ("Types " ++ show t1 ++ " and " ++ show t2 ++ " are incompatible.")

intersectStructs :: [(Identifier, Type)] -> [(Identifier, Type)] -> SemCheck [(Identifier, Type)]
intersectStructs [] [] = return []
intersectStructs [] (_:_) = return []
intersectStructs (_:_) [] = return []
intersectStructs ((n1, t1) : s1) ((n2, t2) : s2)
  | n1 < n2   = intersectStructs s1 ((n2, t2) : s2)
  | n1 > n2   = intersectStructs ((n1, t1) : s1) s2
  | otherwise = (:) <$> ((n1,) <$> intersectTypes t1 t2)
                    <*> intersectStructs s1 s2

checkType :: Type -> SemCheck Type
checkType (TyArray elemTy)
  = TyArray <$> checkType elemTy
checkType (TyTuple elemTys)
  = TyTuple <$> mapM checkType elemTys
checkType (TyChan elemTy)
  = TyChan <$> checkType elemTy
checkType (TyName name)
  = getTypeAlias name
checkType (TyStruct members) = do
  emitMemberNames (Map.keys members)
  TyStruct <$> mapM checkType members
checkType (TyUnion tys)
  = (TyUnion . Set.fromList) <$> checkUnion (Set.elems tys)
checkType ty = return ty

checkUnion :: [Type] -> SemCheck [Type]
checkUnion [] = return []
checkUnion (ty:ts) = do
  ty' <- checkType ty
  case ty' of
    TyUnion ts' -> (++) <$> checkUnion (Set.elems ts')
                        <*> checkUnion ts
    _           -> (:)  <$> checkType ty' <*> checkUnion ts

isArrayType :: Type -> SemCheck Bool
isArrayType = isSubtypeOf (TyArray TyAny)

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
isAbstractType (TyName _)    = return True

-- This is allowed when using nested pairs
-- It leads to type unsafety, but is unfortunately
-- allowed by the language
isAbstractType (TyTuple [TyTuple [TyAny,TyAny], _])
  = return False
isAbstractType (TyTuple [_, TyTuple [TyAny,TyAny]])
  = return False
isAbstractType (TyTuple tys) = anyM isAbstractType tys
isAbstractType (TyArray ty)  = isAbstractType ty
isAbstractType (TyChan ty)   = isAbstractType ty
isAbstractType (TyStruct tys) = anyM isAbstractType (Map.elems tys)
isAbstractType (TyUnion tys) = anyM isAbstractType (Set.elems tys)
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

checkPromotion :: Type -> Annotated Expr TypeA -> SemCheck (Annotated Expr TypeA)
checkPromotion lhsTy@(TyUnion _) (exprTy@(TyUnion _), expr) = do
  checkIsSubtypeOf lhsTy exprTy
  return (exprTy, expr)

checkPromotion unionTy@(TyUnion memberTys) (tyExpr, expr)
  = checkPromotion' (Set.elems memberTys)
  where
    checkPromotion' [] = semanticError "Foo"
    checkPromotion' (ty:tys)
      | ty == tyExpr = do
        emitTypeTag tyExpr
        return (unionTy, ExprPromote (tyExpr, expr) tyExpr)
      | otherwise    = checkPromotion' tys 

checkPromotion lhsTy (exprTy, expr) = do
  checkIsSubtypeOf lhsTy exprTy
  return (exprTy, expr)

checkPromotionRHS :: Type -> Annotated AssignRHS TypeA -> SemCheck (Annotated AssignRHS TypeA)
checkPromotionRHS lhsType (_, RHSExpr expr) = do
  (ty, expr') <- checkPromotion lhsType expr
  return (ty, RHSExpr (ty, expr'))

checkPromotionRHS lhsType (rhsType, rhs) = do
  checkIsSubtypeOf lhsType rhsType
  return (rhsType, rhs)

checkIndexingElem :: Annotated IndexingElem SpanA -> SemCheck (Annotated IndexingElem TypeA)
checkIndexingElem (_, IndexingElem varname exprs) = do
  baseTy <- getVariable varname
  exprs' <- mapM checkExpr exprs
  ty <- checkIndexingElem' baseTy exprs'
  return (ty, IndexingElem varname exprs')

checkIndexingElem' :: Type -> [Annotated Expr TypeA] -> SemCheck (Type, [Type])
checkIndexingElem' baseType@(TyTuple ts) exprs@((ty, e):es) = do
  unlessM (isSubtypeOf TyInt ty)
          (semanticError ("Cannot index tuple with variable of type " ++ show ty))

  index <- case e of
    ExprLit (LitInt i) -> return (fromInteger i)
    _ -> semanticError "Index must be an integer literal"

  unless (index >= 0 && index < length ts)
         (semanticError "Index out of bounds")

  (elemType, baseTypes) <- checkIndexingElem' (ts !! index) es
  return (elemType, baseType : baseTypes)

checkIndexingElem' baseType@(TyArray t) exprs@((ty, e):es) = do
  unlessM (isSubtypeOf TyInt ty)
          (semanticError ("Cannot index array with variable of type " ++ show ty))
  (elemType, baseTypes) <- checkIndexingElem' t es
  return (elemType, baseType : baseTypes)

checkIndexingElem' t [] = return (t, [])
checkIndexingElem' t _  = semanticError ("Type " ++ show t ++ " not indexable")

checkStructElem :: Annotated StructElem SpanA -> SemCheck (Annotated StructElem TypeA)
checkStructElem (_, StructElem baseName members) = do
  baseTy <- getVariable baseName
  resultTy <- foldM checkStructElem' baseTy members
  return (resultTy, StructElem baseName members)
  where
    checkStructElem' :: Type -> Identifier -> SemCheck Type
    checkStructElem' ty@(TyStruct elemsTy) name
      = case Map.lookup name elemsTy of
        Just elemTy -> return elemTy
        Nothing -> semanticError ("No field named " ++ name ++ " in " ++ show ty)
    checkStructElem' ty _ = semanticError ("Type " ++ show ty ++ " is not a structure")

checkExpr :: Annotated Expr SpanA -> SemCheck (Annotated Expr TypeA)
checkExpr (_, ExprLit lit) = return (checkLiteral lit, ExprLit lit)

checkExpr (_, ExprVar varname) = do
  ty <- getVariable varname
  return (ty, ExprVar varname)

checkExpr (_, ExprIndexingElem indexElem) = do
  indexElem'@((ty, _), _) <- checkIndexingElem indexElem
  return (ty, ExprIndexingElem indexElem')

checkExpr (_, ExprStructElem structElem) = do
  structElem'@(ty, _) <- checkStructElem structElem
  return (ty, ExprStructElem structElem')

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
unOpType UnOpNot = (isSubtypeOf TyBool, TyBool)
unOpType UnOpNeg = (isSubtypeOf TyInt, TyInt)
unOpType UnOpOrd = (isSubtypeOf TyChar, TyInt)
unOpType UnOpChr = (isSubtypeOf TyInt, TyChar)
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
    arithmeticOp = (\t1 t2 -> (&&) <$> isSubtypeOf TyInt t1
                                   <*> isSubtypeOf TyInt t2
                           , TyInt)
    booleanOp    = (\t1 t2 -> (&&) <$> isSubtypeOf TyBool t1
                                   <*> isSubtypeOf TyBool t2
                           , TyBool)
    orderOp      = (\t1 t2 -> (isSubtypeOf t1 t2 <^(||)^> isSubtypeOf t2 t1)
                              <^(&&)^>
                              isOrderedType t1
                              <^(&&)^>
                              isOrderedType t2
                           , TyBool)
    equalityOp = (\t1 t2 -> (||) <$> isSubtypeOf t1 t2
                                 <*> isSubtypeOf t2 t1
                         , TyBool)

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
  indexElem'@((ty, _), _) <- checkIndexingElem indexElem
  return (ty, LHSIndexingElem indexElem')

checkAssignLHS (_, LHSStructElem structElem) = do
  structElem'@(ty, _) <- checkStructElem structElem
  return (ty, LHSStructElem structElem')

checkAssignRHS :: Annotated AssignRHS SpanA -> SemCheck (Annotated AssignRHS TypeA)
checkAssignRHS (_, RHSExpr expr) = do
  expr'@(ty, _) <- checkExpr expr
  return (ty, RHSExpr expr')

checkAssignRHS (_, RHSArrayLit exprs) = do
  exprs' <- mapM checkExpr exprs
  innerType <- foldM intersectTypes TyAny (map fst exprs')
  return (TyArray innerType, RHSArrayLit exprs')

checkAssignRHS (_, RHSStructLit values) = do
  emitMemberNames (Map.keys values)

  values' <- mapM checkExpr values
  let valuesTy = Map.map fst values'

  return (TyStruct valuesTy, RHSStructLit values')

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
  elemTy <- case chanTy of
            TyChan ty -> return ty
            _         -> semanticError ("Cannot receive from non-channel " ++ show chanTy)

  unlessM (asks asyncContext)
          (semanticError ("Cannot receive from channel from within a synchronous function"))

  return (elemTy, RHSChanRecv chanName)

checkArgs :: [Type] -> [Type] -> SemCheck ()
checkArgs expected actual = checkArgs' 0 expected actual
  where
    checkArgs' :: Int -> [Type] -> [Type] -> SemCheck ()
    checkArgs' _ [] [] = return ()
    checkArgs' n (a1:as1) (a2:as2) = do
      withErrorContext ("In argument " ++ show (n + 1))
        (checkIsSubtypeOf a1 a2)
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
  withErrorContext ("in call to function " ++ fname)
    (checkArgs expectedArgsType (map fst args'))

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
  unlessM (lift . lift . lift $ getArgument runtimeEnabled)
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
  checkIsSubtypeOf expectedType litType

  return (litType, CaseArm l b')

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

  let lhsType = if varType' == TyAny
                then rhsType
                else varType'

  whenM (liftSemCheck $ isAbstractType lhsType)
        (semanticError ("Cannot declare variable " ++ varname
                     ++ " of incomplete type " ++ show lhsType))

  promotedRHS <- liftSemCheck $ withErrorContext "in assignment"
                                (checkPromotionRHS lhsType rhs')

  addVariable varname lhsType
  return (False, StmtVar lhsType varname promotedRHS)

checkStmt (_, StmtAssign lhs rhs) = do
  lhs'@(lhsType, _) <- liftSemCheck $ checkAssignLHS lhs
  rhs' <- liftSemCheck $ checkAssignRHS rhs

  promotedRHS <- liftSemCheck $ withErrorContext "in assignment"
                                (checkPromotionRHS lhsType rhs')

  return (False, StmtAssign lhs' promotedRHS)

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
  liftSemCheck $ withErrorContext "in return statement"
                  (checkIsSubtypeOf expectedReturnType exprType)
  return (True, StmtReturn expr')

checkStmt (_, StmtExit expr) = do
  expr'@(exprType, _) <- liftSemCheck $ checkExpr expr
  liftSemCheck $ withErrorContext "in exit statement"
                  (checkIsSubtypeOf TyInt exprType)
  return (True, StmtExit expr')

checkStmt (_, StmtPrint exprs ln) = do
  exprs' <- liftSemCheck $ mapM checkExpr exprs
  return (False, StmtPrint exprs' ln)

checkStmt (_, StmtIf predicate b1 maybeB2) = do
  predicate'@(predicateType, _) <- liftSemCheck $ checkExpr predicate
  liftSemCheck $ withErrorContext "in if statement"
                  (checkIsSubtypeOf TyBool predicateType)
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
  liftSemCheck $ withErrorContext "in while statement"
                  (checkIsSubtypeOf TyBool predicateType)

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

  liftSemCheck $ checkIsSubtypeOf chanTy (TyChan rhsTy)

  unlessM (gets asyncContext)
          (semanticError ("Cannot send to channel from within a synchronous function"))

  return (False, StmtChanSend chanName rhs')

checkStmt (_, StmtTypeSwitch varName cases) = do
  varTy <- liftSemCheck $ getVariable varName
  members <- case varTy of
    TyUnion m -> return m
    _         -> semanticError ("Variable " ++ show varName ++ " of type " ++ show varTy ++ " is not a union")

  cases' <- liftSemCheck $ mapM (checkTypeCase varName members) cases
  return (False, StmtTypeSwitch varName cases')

checkTypeCase :: Identifier -> Set Type -> Annotated TypeCase SpanA -> SemCheck (Annotated TypeCase TypeA)
checkTypeCase varName members (_, TypeCase ty body) = do
  ty' <- checkType ty
  unless (Set.member ty' members)
         (semanticError (show ty' ++ " is not a member of union type " ++ show (TyUnion members)))

  emitTypeTag ty'

  context <- ask
  childContext <- lift $ execStateT (addVariable varName ty') (newContext context)

  body' <- local (const childContext) (checkBlock body)

  return ((), TypeCase ty' body')

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
      block'@((alwaysReturns, _), _) <- local (const context) (checkBlock block)

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

checkProgram :: Annotated Program SpanA -> WriterT (Set Identifier, Set Type) (WACCResultT WACCArguments) (Annotated Program TypeA)
checkProgram (_, Program decls) = do
  context <- execStateT (forM_ decls defineDecl) emptyContext
  decls' <- runReaderT (forM decls checkDecl) context
  return ((), Program decls')

waccSemCheck :: Annotated Program SpanA -> WACCResultT WACCArguments (Annotated Program TypeA, (Set Identifier, Set Type))
waccSemCheck = runWriterT . checkProgram
