module AST where

import Common

data Program = Program [FuncDef] Block
    deriving (Show)
data FuncDef = FuncDef Type String [(Type, String)] Block
    deriving (Show)
data Type = TyInt
          | TyBool
          | TyChar
          | TyPair Type Type
          | TyArray Type
          | TyNull
          | TyAny
          | TyVoid
    deriving (Eq)

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
          | StmtIf Expr Block Block
          | StmtWhile Expr Block
          | StmtScope Block
    deriving (Show)

type Block = [(Pos, Stmt)]

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

data Expr = ExprLit Literal
          | ExprNull
          | ExprVar String
          | ExprArrayElem ArrayElem
          | ExprUnOp UnOp Expr
          | ExprBinOp BinOp Expr Expr
    deriving (Show)

instance Show Type where
  show TyInt        = "int"
  show TyBool       = "bool"
  show TyChar       = "char"
  show (TyPair f s) = "pair(" ++ show f ++ "," ++ show s ++ ")"
  show (TyArray t)  = show t ++ "[]"
  show TyNull       = "null"
  show TyAny        = "any"
  show TyVoid       = "void"

instance Show BinOp where
  show BinOpAdd = "+"
  show BinOpSub = "-"
  show BinOpMul = "*"
  show BinOpDiv = "/"
  show BinOpRem = "%"
  show BinOpGT  = ">"
  show BinOpGE  = ">="
  show BinOpLT  = "<"
  show BinOpLE  = "<="
  show BinOpEQ  = "=="
  show BinOpNE  = "!="
  show BinOpAnd = "&&"
  show BinOpOr  = "||"

instance Show UnOp where
  show UnOpNot = "!"
  show UnOpNeg = "neg"
  show UnOpLen = "len"
  show UnOpOrd = "ord"
  show UnOpChr = "chr"
