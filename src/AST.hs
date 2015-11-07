module AST where

data Program = Program [FuncDef] [Stmt]
    deriving (Show)
data FuncDef = FuncDef Type String [(Type, String)] [Stmt]
    deriving (Show)
data Type = TyInt
          | TyBool
          | TyChar
          | TyPair Type Type
          | TyArray Type
          | TyNull
          | TyAny
          | TyVoid
    deriving (Eq,Show)

data ArrayElem = ArrayElem String [Expr]
    deriving (Show)
data PairSide  = PairFst | PairSnd
    deriving (Show)
data PairElem  = PairElem PairSide Expr
    deriving (Show)

data AssignLHS = LHSVar String
               | LHSPair PairElem
               | LHSArray ArrayElem
    deriving (Show)

data AssignRHS = RHSExpr Expr
               | RHSArrayLit [Expr]
               | RHSNewPair Expr Expr
               | RHSPair PairElem
               | RHSCall String [Expr]
    deriving (Show)

data Stmt = StmtSkip
          | StmtVar Type String AssignRHS
          | StmtAssign AssignLHS AssignRHS
          | StmtRead AssignLHS 
          | StmtFree Expr
          | StmtReturn Expr
          | StmtExit Expr
          | StmtPrint Bool Expr
          | StmtIf Expr [Stmt] [Stmt]
          | StmtWhile Expr [Stmt]
          | StmtScope [Stmt]
    deriving (Show)

data Literal = LitInt Integer
             | LitBool Bool
             | LitChar Char
             | LitString String
    deriving (Show)

data UnOp = UnOpNot
          | UnOpNeg
          | UnOpLen
          | UnOpOrd
          | UnOpChr
    deriving (Show)

data BinOp = BinOpAdd
           | BinOpSub
           | BinOpMul
           | BinOpDiv
           | BinOpRem
           | BinOpGT
           | BinOpGE
           | BinOpLT
           | BinOpLE
           | BinOpEQ
           | BinOpNE
           | BinOpAnd
           | BinOpOr
    deriving (Show)

data Expr = ExprLit Literal
          | ExprNull
          | ExprVar String
          | ExprArrayElem ArrayElem
          | ExprUnOp UnOp Expr
          | ExprBinOp BinOp Expr Expr
    deriving (Show)
