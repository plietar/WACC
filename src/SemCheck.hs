module SemCheck where
import Data.Map as Map
import AST
import Common

data SymbolTable = Scope (Map String Type) SymbolTable | Root
addSymbol :: String -> Type -> SymbolTable -> WACCResult SymbolTable
addSymbol varname typ (Scope table nextScope)
  = if Map.notMember varname table
    then OK (Scope (Map.insert varname typ table) nextScope)
    else Error SemanticError ("Symbol \"" ++ varname ++ "\" already defined")
addSymbol _ _ Root
  = Error SemanticError "Cannot add symbol to root scope" 

symbolLookUp :: String -> SymbolTable -> WACCResult Type
symbolLookUp varname (Scope table nextScope)
  = case Map.lookup varname table of
    Nothing -> symbolLookUp varname nextScope
    Just ty -> OK ty
symbolLookUp varname Root
  = Error SemanticError ("Unknown symbol \"" ++ varname ++ "\"")

typeCheckExpr :: Expr -> SymbolTable -> WACCResult Type
typeCheckExpr (ExprLit (LitInt _)) _    = OK TyInt
typeCheckExpr (ExprLit (LitBool _)) _   = OK TyBool
typeCheckExpr (ExprLit (LitChar _)) _   = OK TyChar
typeCheckExpr (ExprLit (LitString _)) _ = OK TyString
typeCheckExpr (ExprNull) _              = OK TyNull

typeCheckExpr (ExprVar varname) symboltable
  = symbolLookUp varname symboltable

typeCheckExpr (ExprArrayElem (ArrayElem varname exprs)) table
  = symbolLookUp varname table >>= (\arrtype -> checkArrayIndexing arrtype exprs table)

typeCheckExpr (ExprUnOp op expr) table
  = typeCheckExpr expr table >>= checkUnOp op
typeCheckExpr (ExprBinOp op e1 e2) table = do
  t1 <- typeCheckExpr e1 table
  t2 <- typeCheckExpr e2 table
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
       else Error SemanticError ("Cannot apply unary operator to type " ++ show typ)

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
       else Error SemanticError ("Cannot apply binary operator to types " ++ show t1 ++ " and " ++ show t2)

checkArrayIndexing :: Type -> [Expr] -> SymbolTable -> WACCResult Type
checkArrayIndexing (TyArray innerType) (e : es) table = do
  indexType <- typeCheckExpr e table
  if compatibleType TyInt indexType
  then checkArrayIndexing innerType es table
  else Error SemanticError ("Cannot index array with type " ++ show indexType)
checkArrayIndexing t [] _ = OK t
checkArrayIndexing t _ _  = Error SemanticError ("Cannot index variable of type " ++ show t)


compatibleType :: Type -> Type -> Bool
compatibleType (TyPair _ _) (TyNull)     = True
compatibleType (TyNull) (TyPair _ _)     = True
compatibleType (TyPair _ _) TyNestedPair = True
compatibleType TyNestedPair (TyPair _ _) = True
compatibleType (TyPair f1 s1) (TyPair f2 s2)
  = compatibleType f1 f2 && compatibleType s1 s2
compatibleType (TyArray t1) (TyArray t2)
  = compatibleType t1 t2
compatibleType t1 t2
  = t1 == t2


isArrayType :: Type -> Bool
isArrayType (TyArray _) = True
isArrayType _           = False


isOrderedType :: Type -> Bool
isOrderedType TyInt  = True
isOrderedType TyChar = True
isOrderedType _      = False


