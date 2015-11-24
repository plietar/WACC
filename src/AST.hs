{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AST where

import Common

class ( Show (Ann a Identifier)
      , Show (Ann a Program)
      , Show (Ann a FuncDef)
      , Show (Ann a Parameter)
      , Show (Ann a Block)
      , Show (Ann a Stmt)
      , Show (Ann a Expr)
      , Show (Ann a AssignLHS)
      , Show (Ann a AssignRHS)
      , Show (Ann a ArrayElem)
      , Show (Ann a PairElem)
      , Show (Ann a PairSide)
      , Show (Ann a Literal)
      , Show (Ann a UnOp)
      , Show (Ann a BinOp)
      ) => Annotation a where
  type Ann a (t :: * -> *)

type Annotated (t :: * -> *) (a :: *)
  = (Ann a t, t a)

data Program a = Program [Annotated FuncDef a] (Annotated Block a)
data Parameter a = Parameter Type (Annotated Identifier a)
data FuncDef a = FuncDef Type (Annotated Identifier a) [Annotated Parameter a] (Annotated Block a)
data Block a = Block [Annotated Stmt a]

data Stmt a
  = StmtSkip
  | StmtVar    Type (Annotated Identifier a) (Annotated AssignRHS a)
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
  = LHSVar       (Annotated Identifier a)
  | LHSPairElem  (Annotated PairElem a)
  | LHSArrayElem (Annotated ArrayElem a)

data AssignRHS a
  = RHSExpr     (Annotated Expr a)
  | RHSArrayLit [Annotated Expr a]
  | RHSNewPair  (Annotated Expr a) (Annotated Expr a)
  | RHSPairElem (Annotated PairElem a)
  | RHSCall     (Annotated Identifier a) [Annotated Expr a]

data Expr a
  = ExprLit       (Annotated Literal a)
  | ExprVar       (Annotated Identifier a)
  | ExprArrayElem (Annotated ArrayElem a)
  | ExprUnOp      (Annotated UnOp a) (Annotated Expr a)
  | ExprBinOp     (Annotated BinOp a) (Annotated Expr a) (Annotated Expr a)

data Identifier a = Identifier String

data ArrayElem a
  = ArrayElem (Annotated Identifier a) [(Annotated Expr a)]

data PairElem a
  = PairElem (Annotated PairSide a) (Annotated Expr a)

data PairSide a = PairFst | PairSnd

data Literal a = LitInt Integer
               | LitBool Bool
               | LitChar Char
               | LitString String
               | LitNull

data UnOp a = UnOpNot
            | UnOpNeg
            | UnOpLen
            | UnOpOrd
            | UnOpChr

data BinOp a = BinOpAdd
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
          | TyNull
          | TyAny
          | TyVoid
    deriving (Eq)

deriving instance Annotation a => Show (Identifier a)
deriving instance Annotation a => Show (Program a)
deriving instance Annotation a => Show (FuncDef a)
deriving instance Annotation a => Show (Parameter a)
deriving instance Annotation a => Show (Block a)
deriving instance Annotation a => Show (Stmt a)
deriving instance Annotation a => Show (Expr a)
deriving instance Annotation a => Show (AssignLHS a)
deriving instance Annotation a => Show (AssignRHS a)
deriving instance Annotation a => Show (ArrayElem a)
deriving instance Annotation a => Show (PairElem a)
deriving instance Annotation a => Show (PairSide a)
deriving instance Annotation a => Show (Literal a)

instance Show (BinOp a) where
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

instance Show (UnOp a) where
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
  show TyNull       = "null"
  show TyAny        = "any"
  show TyVoid       = "void"

data SpanA
instance Annotation SpanA where
  type Ann SpanA t = Span

data TypeA
instance Annotation TypeA where
  type Ann TypeA Identifier = ()
  type Ann TypeA Program = ()
  type Ann TypeA FuncDef = ()
  type Ann TypeA Parameter = ()
  type Ann TypeA Block = (Bool, Type)
  type Ann TypeA Stmt = (Bool, Type)
  type Ann TypeA Expr = Type
  type Ann TypeA AssignLHS = Type
  type Ann TypeA AssignRHS = Type
  type Ann TypeA ArrayElem = Type
  type Ann TypeA PairElem = Type
  type Ann TypeA PairSide = () --((Type, Type) -> Type)
  type Ann TypeA Literal = Type
  type Ann TypeA UnOp = () --(Type -> Maybe Type)
  type Ann TypeA BinOp = () --(Type -> Type -> Maybe Type)

