{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AST where

import Common

class ( Show (Ann a Program)
      , Show (Ann a FuncDef)
      , Show (Ann a Block)
      , Show (Ann a Stmt)
      , Show (Ann a Expr)
      , Show (Ann a AssignLHS)
      , Show (Ann a AssignRHS)
      , Show (Ann a ArrayElem)
      , Show (Ann a PairElem)
      ) => Annotation a where
  type Ann a (t :: * -> *)

type Annotated (t :: * -> *) (a :: *)
  = (Ann a t, t a)

data FuncName = FuncName Identifier | MainFunc

data Program a = Program [Annotated FuncDef a]
data FuncDef a = FuncDef Type FuncName [(Type, Identifier)] (Annotated Block a)
data Block a = Block [Annotated Stmt a]

data Stmt a
  = StmtSkip
  | StmtVar    Type Identifier (Annotated AssignRHS a)
  | StmtAssign (Annotated AssignLHS a) (Annotated AssignRHS a)
  | StmtRead   (Annotated AssignLHS a)
  | StmtFree   (Annotated Expr a)
  | StmtReturn (Annotated Expr a)
  | StmtExit   (Annotated Expr a)
  | StmtPrint  (Annotated Expr a) Bool
  | StmtIf     (Annotated Expr a) (Annotated Block a) (Annotated Block a)
  | StmtWhile  (Annotated Expr a) (Annotated Block a)
  | StmtScope  (Annotated Block a)

data AssignLHS a
  = LHSVar       Identifier
  | LHSPairElem  (Annotated PairElem a)
  | LHSArrayElem (Annotated ArrayElem a)

data AssignRHS a
  = RHSExpr     (Annotated Expr a)
  | RHSArrayLit [Annotated Expr a]
  | RHSNewPair  (Annotated Expr a) (Annotated Expr a)
  | RHSPairElem (Annotated PairElem a)
  | RHSCall     Identifier [Annotated Expr a]

data Expr a
  = ExprLit       Literal
  | ExprVar       Identifier
  | ExprArrayElem (Annotated ArrayElem a)
  | ExprUnOp      UnOp  (Annotated Expr a)
  | ExprBinOp     BinOp (Annotated Expr a) (Annotated Expr a)

type Identifier = String

data ArrayElem a
  = ArrayElem Identifier [(Annotated Expr a)]

data PairElem a
  = PairElem PairSide (Annotated Expr a)

data PairSide = PairFst | PairSnd

data Literal = LitInt Integer
             | LitBool Bool
             | LitChar Char
             | LitString String
             | LitNull
  deriving Show

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

data Type = TyInt
          | TyBool
          | TyChar
          | TyPair Type Type
          | TyArray Type
          | TyAny
          | TyVoid
    deriving (Eq)

deriving instance Annotation a => Show (Program a)
deriving instance Annotation a => Show (FuncDef a)
deriving instance Annotation a => Show (Block a)
deriving instance Annotation a => Show (Stmt a)
deriving instance Annotation a => Show (Expr a)
deriving instance Annotation a => Show (AssignLHS a)
deriving instance Annotation a => Show (AssignRHS a)
deriving instance Annotation a => Show (ArrayElem a)
deriving instance Annotation a => Show (PairElem a)

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

instance Show Type where
  show TyInt        = "int"
  show TyBool       = "bool"
  show TyChar       = "char"
  show (TyPair f s) = "pair(" ++ show f ++ "," ++ show s ++ ")"
  show (TyArray t)  = show t ++ "[]"
  show TyAny        = "any"
  show TyVoid       = "void"

instance Show PairSide where
  show PairFst = "fst"
  show PairSnd = "snd"

instance Show FuncName where
  show MainFunc = "main"
  show (FuncName name) = "f_" ++ name

data SpanA
instance Annotation SpanA where
  type Ann SpanA t = Span

data TypeA
instance Annotation TypeA where
  type Ann TypeA Program = ()
  type Ann TypeA FuncDef = ()
  type Ann TypeA Block = (Bool, [String])
  type Ann TypeA Stmt = Bool
  type Ann TypeA Expr = Type
  type Ann TypeA AssignLHS = Type
  type Ann TypeA AssignRHS = Type
  type Ann TypeA ArrayElem = Type
  type Ann TypeA PairElem = Type
