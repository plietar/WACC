module SemCheck where
import Data.Map as Map
import AST

data SymbolTable = Scope (Map String Type) SymbolTable | Root
addSymbol :: String -> Type -> SymbolTable -> Maybe SymbolTable
addSymbol varname typ (Scope table nextScope)
 = if Map.notMember varname table
   then Just (Scope (Map.insert varname typ table) nextScope)
   else Nothing
addSymbol _ _ Root = error "Cannot add symbols to root"

symbolLookUp :: String -> SymbolTable -> Maybe Type
symbolLookUp varname (Scope table nextScope)
  = case Map.lookup varname table of
    Nothing -> symbolLookUp varname nextScope
    Just ty -> Just ty
symbolLookUp varname Root
  = Nothing


typeCheckExpr :: Expr -> SymbolTable -> Maybe Type
typeCheckExpr (ExprLit (LitInt _)) _ = Just TyInt
typeCheckExpr (ExprLit (LitBool _)) _ = Just TyBool
typeCheckExpr (ExprLit (LitChar _)) _ = Just TyChar
typeCheckExpr (ExprLit (LitString _)) _ = Just TyString
typeCheckExpr (ExprNull) _ = Just TyNull

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

unOpType UnOpNot = (compatibleType TyBool, TyBool)
unOpType UnOpNeg = (compatibleType TyInt, TyInt)
unOpType UnOpOrd = (compatibleType TyChar, TyInt)
unOpType UnOpChr = (compatibleType TyChar, TyInt)
unOpType UnOpLen = (isArrayType, TyInt)

checkUnOp op typ
  = let (predicate, result) = unOpType op
    in if predicate typ then Just result else Nothing

binOpType BinOpAdd = arithmeticOp
binOpType BinOpSub = arithmeticOp
binOpType BinOpMul = arithmeticOp
binOpType BinOpDiv = arithmeticOp
binOpType BinOpRem = arithmeticOp
binOpType BinOpGT = orderOp
binOpType BinOpGE = orderOp
binOpType BinOpLT = orderOp
binOpType BinOpLE = orderOp
binOpType BinOpEQ = equalityOp
binOpType BinOpNE = equalityOp
binOpType BinOpAnd = booleanOp
binOpType BinOpOr = booleanOp

arithmeticOp = (\t1 t2 -> compatibleType TyInt t1 && compatibleType TyInt t2, TyInt)
booleanOp    = (\t1 t2 -> compatibleType TyBool t1 && compatibleType TyBool t2, TyBool)
orderOp      = (\t1 t2 -> compatibleType t1 t2 && isOrderedType t1 && isOrderedType t2, TyBool)
equalityOp   = (compatibleType, TyBool)

checkBinOp op t1 t2
  = let (predicate, result) = binOpType op
    in if predicate t1 t2 then Just result else Nothing


checkArrayIndexing :: Type -> [Expr] -> SymbolTable -> Maybe Type
checkArrayIndexing (TyArray t) (e : es) table
  | Just ty <- (typeCheckExpr e table), ty == TyInt = checkArrayIndexing t es table
  | otherwise = Nothing
checkArrayIndexing t [] _ = Just t
checkArrayIndexing _ _ _ = Nothing


compatibleType :: Type -> Type -> Bool
compatibleType (TyPair _ _) (TyNull) = True
compatibleType (TyNull) (TyPair _ _) = True
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


