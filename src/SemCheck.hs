{-# LANGUAGE LambdaCase #-}

module SemCheck where
import qualified Data.Map as Map
import Control.Monad.State

import ScopedMap
import AST
import Common
import Control.Monad.Trans
import Control.Applicative

data Context = Context
  { variables :: ScopedMap String Type
  , functions :: Map.Map String ([Type], Type)
  }
type ContextState a = StateT Context WACCResult a

addVariable :: String -> Type -> ContextState ()
addVariable name value = do
  context <- get
  case ScopedMap.insertIfNotExists name value (variables context) of
    Just table -> put context { variables = table }
    Nothing -> lift $ Error SemanticError ("Variable \"" ++ name ++ "\" already exists")

getVariable :: String -> Context -> WACCResult Type
getVariable name context
  = case ScopedMap.lookup name (variables context) of
      Just value -> OK value
      Nothing    -> Error SemanticError ("Unknown variable \"" ++ name ++ "\"")

addFunction :: String -> ([Type], Type) -> ContextState ()
addFunction name typ = do
  context <- get
  case Map.lookup name (functions context) of
    Just _  -> lift $ Error SemanticError ("Function \"" ++ name ++ "\" already exists")
    Nothing -> put context { functions = Map.insert name typ (functions context) }

getFunction :: String -> Context -> WACCResult ([Type], Type)
getFunction name context
  = case Map.lookup name (functions context) of
      Just value -> OK value
      Nothing    -> Error SemanticError ("Unknown function \"" ++ name ++ "\"")

emptyContext :: Context
emptyContext = Context ScopedMap.empty Map.empty

newContext :: Context -> Context
newContext parent
  = Context
    { variables = newScope (variables parent)
    , functions = functions parent
    }

typeCheckExpr :: Expr -> Context -> WACCResult Type
typeCheckExpr (ExprLit (LitInt _)) _    = OK TyInt
typeCheckExpr (ExprLit (LitBool _)) _   = OK TyBool
typeCheckExpr (ExprLit (LitChar _)) _   = OK TyChar
typeCheckExpr (ExprLit (LitString _)) _ = OK TyString
typeCheckExpr (ExprNull) _              = OK TyAny

typeCheckExpr (ExprVar varname) context
  = getVariable varname context
typeCheckExpr (ExprArrayElem arrayElem) context
  = checkArrayElem arrayElem context
typeCheckExpr (ExprUnOp op expr) context
  = typeCheckExpr expr context >>= checkUnOp op
typeCheckExpr (ExprBinOp op e1 e2) context = do
  t1 <- typeCheckExpr e1 context
  t2 <- typeCheckExpr e2 context
  checkBinOp op t1 t2

unOpType :: UnOp -> (Type -> Bool, Type)
unOpType UnOpNot = (compatibleType TyBool, TyBool)
unOpType UnOpNeg = (compatibleType TyInt, TyInt)
unOpType UnOpOrd = (compatibleType TyChar, TyInt)
unOpType UnOpChr = (compatibleType TyChar, TyInt)
unOpType UnOpLen = (isArrayType, TyInt)

checkUnOp :: UnOp -> Type -> WACCResult Type
checkUnOp op typ
  = let (predicate, result) = unOpType op
    in if predicate typ
       then OK result
       else Error SemanticError ("Cannot apply unary operator " ++ show op ++ " to type " ++ show typ)

binOpType :: BinOp -> (Type -> Type -> Bool, Type)
binOpType BinOpAdd = arithmeticOp
binOpType BinOpSub = arithmeticOp
binOpType BinOpMul = arithmeticOp
binOpType BinOpDiv = arithmeticOp
binOpType BinOpRem = arithmeticOp
binOpType BinOpGT  = orderOp
binOpType BinOpGE  = orderOp
binOpType BinOpLT  = orderOp
binOpType BinOpLE  = orderOp
binOpType BinOpEQ  = equalityOp
binOpType BinOpNE  = equalityOp
binOpType BinOpAnd = booleanOp
binOpType BinOpOr  = booleanOp

arithmeticOp = (\t1 t2 -> compatibleType TyInt t1 && compatibleType TyInt t2, TyInt)
booleanOp    = (\t1 t2 -> compatibleType TyBool t1 && compatibleType TyBool t2, TyBool)
orderOp      = (\t1 t2 -> compatibleType t1 t2 && isOrderedType t1 && isOrderedType t2, TyBool)
equalityOp   = (compatibleType, TyBool)

checkBinOp :: BinOp -> Type -> Type -> WACCResult Type
checkBinOp op t1 t2
  = let (predicate, result) = binOpType op
    in if predicate t1 t2
       then OK result
       else Error SemanticError ("Cannot apply binary operator " ++ show op ++ " to types " ++ show t1 ++ " and " ++ show t2)

checkArrayElem :: ArrayElem -> Context -> WACCResult Type
checkArrayElem (ArrayElem varname exprs) context
  = getVariable varname context >>= (\arrtype -> checkArrayIndexing arrtype exprs context)

checkArrayIndexing :: Type -> [Expr] -> Context -> WACCResult Type
checkArrayIndexing (TyArray innerType) (e : es) context = do
  indexType <- typeCheckExpr e context
  if compatibleType TyInt indexType
  then checkArrayIndexing innerType es context
  else Error SemanticError ("Cannot index array with type " ++ show indexType)
checkArrayIndexing TyAny _ _ = OK TyAny
checkArrayIndexing t [] _    = OK t
checkArrayIndexing t _ _     = Error SemanticError ("Cannot index variable of type " ++ show t)

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
mergeTypes t1 t2
  | t1 == t2  = OK t1
  | otherwise = Error SemanticError ("Types " ++ show t1 ++ " and " ++ show t2 ++ " are not compatible")

isArrayType :: Type -> Bool
isArrayType (TyArray _) = True
isArrayType TyAny       = True
isArrayType _           = False

isPairType :: Type -> Bool
isPairType (TyPair _ _) = True
isPairType TyAny        = True
isPairType _            = False

isOrderedType :: Type -> Bool
isOrderedType TyInt  = True
isOrderedType TyChar = True
isOrderedType TyAny  = True
isOrderedType _      = False

typeCheckPairElem :: PairElem -> Context -> WACCResult Type
typeCheckPairElem (PairElem side e) context = do
  t <- typeCheckExpr e context
  case (side, t) of
    (PairFst, TyPair fst _) -> OK fst
    (PairSnd, TyPair _ snd) -> OK snd
    (_, TyAny             ) -> OK TyAny
    (_, _                 ) -> Error SemanticError ("Type " ++ show t ++ " is not pair")

typeCheckAssignLHS :: AssignLHS -> Context -> WACCResult Type
typeCheckAssignLHS (LHSVar name) context
  = getVariable name context
typeCheckAssignLHS (LHSPair pairElem) context
  = typeCheckPairElem pairElem context
typeCheckAssignLHS (LHSArray array) context
  = checkArrayElem array context

typeCheckAssignRHS :: AssignRHS -> Context -> WACCResult Type
typeCheckAssignRHS (RHSExpr expr) context
  = typeCheckExpr expr context
typeCheckAssignRHS (RHSArrayLit exprs) context
  = foldM (\t e -> typeCheckExpr e context >>= mergeTypes t) TyAny exprs
typeCheckAssignRHS (RHSNewPair e1 e2) context = do
  t1 <- typeCheckExpr e1 context
  t2 <- typeCheckExpr e2 context
  return (TyPair t1 t2)
typeCheckAssignRHS (RHSPair pairElem) context
  = typeCheckPairElem pairElem context
typeCheckAssignRHS (RHSCall fname args) context = do
  (expectedArgsType, returnType) <- undefined
  let checkArgs _ [] [] = OK ()
      checkArgs n (a1:as1) (a2:as2)
        = if compatibleType a1 a2
          then checkArgs (n+1) as1 as2
          else Error SemanticError ("Expected type " ++ show a2 ++ " but got type " ++ show a1 ++ " for argument " ++ show (n + 1)++ " of call to function \"" ++ fname ++ "\"")
      checkArgs n _ _ = Error SemanticError ("Wrong number of arguments in call to function \"" ++ fname ++ "\". Expected " ++ show (length expectedArgsType) ++ " but got " ++ show n)
  actualArgsType <- mapM (\e -> typeCheckExpr e context) args
  checkArgs 0 expectedArgsType actualArgsType
  return returnType

typeCheckBlock :: [Stmt] -> Context -> WACCResult Type
typeCheckBlock block parent
  = let context = newContext parent
    in fmap last (evalStateT (forM block typeCheckStmt) context)


typeCheckStmt :: Stmt -> ContextState Type
typeCheckStmt StmtSkip = return TyVoid

typeCheckStmt (StmtVar varType varname rhs) = do
  context <- get
  rhsType <- lift $ typeCheckAssignRHS rhs context
  when (not (compatibleType varType rhsType))
       (lift (Error SemanticError ("Cannot assign RHS of type " ++ show rhsType ++ " to LHS of type " ++ show varType)))
  addVariable varname varType
  return TyVoid

typeCheckStmt (StmtAssign lhs rhs) = do
  context <- get
  lhsType <- lift $ typeCheckAssignLHS lhs context
  rhsType <- lift $ typeCheckAssignRHS rhs context
  when (not (compatibleType lhsType rhsType))
       (lift (Error SemanticError ("Cannot assign RHS of type " ++ show rhsType ++ " to LHS of type " ++ show lhsType)))
  return TyVoid

typeCheckStmt (StmtFree e) = do
  context <- get
  t <- lift $ typeCheckExpr e context
  when (not (isPairType t))
       (lift (Error SemanticError ("Cannot free variable of type " ++ show t)))
  return TyVoid

typeCheckStmt (StmtReturn e) = do
  context <- get
  lift $ typeCheckExpr e context

typeCheckStmt (StmtPrint _ e) = do
  context <- get
  lift $ typeCheckExpr e context
  return TyVoid

typeCheckStmt (StmtIf predicate b1 b2) = do
  context <- get
  predicateType <- lift $ typeCheckExpr predicate context
  when (not (compatibleType TyBool predicateType))
       (lift (Error SemanticError ("Condition cannot be of type " ++ show predicateType)))
  t1 <- lift $ typeCheckBlock b1 context
  t2 <- lift $ typeCheckBlock b2 context
  lift $ mergeTypes t1 t2

typeCheckStmt (StmtWhile predicate block) = do
  context <- get
  predicateType <- lift $ typeCheckExpr predicate context
  when (not (compatibleType TyBool predicateType))
       (lift (Error SemanticError ("Condition cannot be of type " ++ show predicateType)))
  lift $ typeCheckBlock block context

typeCheckStmt (StmtScope block) = do
  context <- get
  lift $ typeCheckBlock block context

typeCheckFunction (FuncDef expectedReturnType name args block) context = do
  let addArgs = forM_ args (\(argType, argName) -> addVariable argName argType)
  context <- execStateT addArgs context
  actualReturnType <- typeCheckBlock block context
  when (actualReturnType == TyVoid)
       (Error SyntaxError ("Function \"" ++ name ++ "\" does not return anything"))
  when (not (compatibleType expectedReturnType actualReturnType))
       (Error SemanticError ("Function \"" ++ name ++ "\" should return type " ++ show expectedReturnType ++ " but returns " ++ show actualReturnType ++ " instead"))

typeCheckProgram (Program funcs block) = do
  let defineFunc (FuncDef returnType name args _)
        = addFunction name (map fst args, returnType)
      defineAllFuncs = forM_ funcs defineFunc
  context <- execStateT defineAllFuncs emptyContext
  forM_ funcs (\f -> typeCheckFunction f context)
  typeCheckBlock block context

