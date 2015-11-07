{-# LANGUAGE LambdaCase #-}

module SemCheck where
import Data.Map as Map
import Control.Monad.State

import ScopedMap
import AST
import Common
import Control.Monad.Trans

data Context = Context
  { variables :: ScopedMap String Type
  , functions :: Map String ([Type], Type)
  }
type ContextState a = StateT Context WACCResult a

addVariable :: String -> Type -> ContextState ()
addVariable name value = do
  state <- get
  case ScopedMap.insertIfNotExists name value (variables state) of
    Just table -> put state { variables = table }
    Nothing -> lift $ Error SemanticError ("Variable \"" ++ name ++ "\" already exists")

getVariable :: String -> Context -> WACCResult Type
getVariable name state
  = case ScopedMap.lookup name (variables state) of
      Just value -> OK value
      Nothing    -> Error SemanticError ("Unknown variable \"" ++ name ++ "\"")

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
compatibleType _ TyAny = True
compatibleType TyAny _ = True
compatibleType (TyPair f1 s1) (TyPair f2 s2)
  = compatibleType f1 f2 && compatibleType s1 s2
compatibleType (TyArray t1) (TyArray t2)
  = compatibleType t1 t2
compatibleType t1 t2
  = t1 == t2

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
typeCheckAssignRHS (RHSArrayLit []) context
  = OK TyAny
typeCheckAssignRHS (RHSArrayLit exprs) context
  = undefined -- TODO
typeCheckAssignRHS (RHSNewPair e1 e2) context = do
  t1 <- typeCheckExpr e1 context
  t2 <- typeCheckExpr e2 context
  return (TyPair t1 t2)
typeCheckAssignRHS (RHSPair pairElem) context
  = typeCheckPairElem pairElem context
typeCheckAssignRHS (RHSCall fname args) context
  = undefined -- TODO

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
  if isPairType t
  then return TyVoid
  else lift (Error SemanticError ("Cannot free variable of type " ++ show t))

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
  when (not (compatibleType t1 t2))
       (lift (Error SemanticError ("Then and Else branches do not return the same type")))
  return t1

typeCheckStmt (StmtWhile predicate block) = do
  context <- get
  predicateType <- lift $ typeCheckExpr predicate context
  when (not (compatibleType TyBool predicateType))
       (lift (Error SemanticError ("Condition cannot be of type " ++ show predicateType)))
  lift $ typeCheckBlock block context

typeCheckStmt (StmtScope block) = do
  context <- get
  lift $ typeCheckBlock block context

typeCheckProgram (Program func block) = typeCheckBlock block


