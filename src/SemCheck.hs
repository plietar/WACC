module SemCheck where
import Data.Map as Map
import AST



data SymbolTable = Scope (Map String Type) SymbolTable | Root



typeCheckExpr :: Expr -> SymbolTable -> Maybe Type
typeCheckExpr (ExprLit (LitInt _)) _ = Just TyInt

typeCheckExpr (ExprLit (LitBool _)) _ = Just TyBool
typeCheckExpr (ExprLit (LitChar _)) _ = Just TyChar
typeCheckExpr (ExprLit (LitString _)) _ = Just TyString

typeCheckExpr (ExprNull) _ = Just TyNull
typeCheckExpr (ExprVar varname) symboltable
  = symbolLookUp varname symboltable
typeCheckExpr (ExprArrayElem (ArrayElem str exprs)) table
  = symbolLookUp str table >>= (\arrtype -> checkArrayIndexing arrtype exprs table)
typeCheckExpr (ExprUnOp op expr) table
  = typeCheckExpr expr table >>= checkUnOp op

checkUnOp UnOpNot t
  = typesToMaybe t TyBool
checkUnOp UnOpNeg t
  = typesToMaybe t TyInt
checkUnOp UnOpOrd t
  = typesToMaybe t TyChar
checkUnOp UnOpChr t
  = typesToMaybe t TyInt
-- len is valid for any array. Is this okay?
checkUnOp UnOpLen (TyArray t)
  = Just (TyArray t)
checkUnOp UnOpLen _
  = Nothing
--checkUnOp UnOpLen t
--  = typesToMaybe t (TyArray arrayt)


typesToMaybe typeTest typeResult
  = case compatibleType typeTest typeResult of
    True -> Just typeResult
    False -> Nothing



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


symbolLookUp varname (Scope table nextScope)
  = case Map.lookup varname table of
    Nothing -> symbolLookUp varname nextScope
    Just ty -> Just ty
symbolLookUp varname Root
  = Nothing


checkArrayIndexing :: Type -> [Expr] -> SymbolTable -> Maybe Type
checkArrayIndexing (TyArray t) (e : es) table
  | Just ty <- (typeCheckExpr e table), ty == TyInt = checkArrayIndexing t es table
  | otherwise = Nothing
checkArrayIndexing t [] _ = Just t
checkArrayIndexing _ _ _ = Nothing



