{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Common.AST where

import Common.Span
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data FuncName = FuncName Identifier | MainFunc
data Program a = Program [Annotated Decl a]
data Decl a = DeclFunc (Annotated FuncDef a)
            | DeclType (Annotated TypeDef a)
            | DeclFFIFunc (Annotated FFIFunc a)

data FFIFunc a = FFIFunc Type Bool Identifier [Type] Identifier
data FuncDef a = FuncDef Type Bool FuncName [(Type, Identifier)] (Annotated Block a)
data TypeDef a = TypeDef Identifier Type

data Block a = Block [Annotated Stmt a]
data CaseArm a = CaseArm Literal (Annotated Block a)

data Stmt a
  = StmtSkip
  | StmtVar    Type Identifier (Annotated AssignRHS a)
  | StmtAssign (Annotated AssignLHS a) (Annotated AssignRHS a)
  | StmtRead   (Annotated AssignLHS a)
  | StmtFree   (Annotated Expr a)
  | StmtReturn (Annotated Expr a)
  | StmtExit   (Annotated Expr a)
  | StmtPrint  [(Annotated Expr a)] Bool
  | StmtIf     (Annotated Expr a) (Annotated Block a) (Maybe (Annotated Block a))
  | StmtWhile  (Annotated Expr a) (Annotated Block a)
  | StmtScope  (Annotated Block a)
  | StmtSwitch (Annotated Expr a) [Annotated CaseArm a]
  | StmtFor    (Annotated Expr a) (Annotated Expr a) (Annotated Expr a) (Annotated Block a)
  | StmtCall   Identifier [Annotated Expr a]
  | StmtAwait  Identifier [Annotated Expr a]
  | StmtFire   Identifier [Annotated Expr a]
  | StmtChanSend Identifier (Annotated AssignRHS a)

data AssignLHS a
  = LHSVar       Identifier
  | LHSIndexingElem (Annotated IndexingElem a)
  | LHSStructElem (Annotated StructElem a)

data AssignRHS a
  = RHSExpr     (Annotated Expr a)
  | RHSArrayLit [Annotated Expr a]
  | RHSStructLit (Map String (Annotated AssignRHS a))
  | RHSNewTuple [Annotated Expr a]
  | RHSCall     Identifier [Annotated Expr a]
  | RHSAwait    Identifier [Annotated Expr a]
  | RHSNewChan
  | RHSChanRecv Identifier

data Expr a
  = ExprLit       Literal
  | ExprVar       Identifier
  | ExprIndexingElem (Annotated IndexingElem a)
  | ExprStructElem (Annotated StructElem a)
  | ExprUnOp      UnOp  (Annotated Expr a)
  | ExprBinOp     BinOp (Annotated Expr a) (Annotated Expr a)

type Identifier = String

data IndexingElem a
  = IndexingElem Identifier [(Annotated Expr a)]

data StructElem a
  = StructElem Identifier [Identifier]

data Label = NamedLabel String | UnnamedLabel Int
  deriving (Ord, Eq)

instance Show Label where
  show (UnnamedLabel i) = ".L" ++ show i
  show (NamedLabel   n) = n

data Literal = LitInt Integer
             | LitBool Bool
             | LitChar Char
             | LitString String
             | LitNull
             | LitLabel Label
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
          | TyTuple [Type]
          | TyArray Type
          | TyChan Type
          | TyStruct (Map String Type)
          | TyName Identifier
          | TyFFI Identifier
          | TyAny
          | TyVoid
          | TyNull
    deriving (Eq)

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
  show (TyTuple ts) = "tuple(" ++ intercalate ", " (map show ts) ++ ")"
  show (TyArray t)  = show t ++ "[]"
  show (TyChan t)   = "chan(" ++ show t ++ ")"
  show (TyStruct ts) = "struct{ " ++ concatMap (\(name, ty) -> show ty ++ " " ++ name ++ "; ") (Map.assocs ts) ++ "}"
  show (TyName n)   = show n
  show (TyFFI n)    = n
  show TyAny        = "any"
  show TyVoid       = "void"
  show TyNull       = "null"

instance Show FuncName where
  show MainFunc = "wacc_main"
  show (FuncName name) = "f_" ++ name


class ( Show (Ann a Program)
      , Show (Ann a Decl)
      , Show (Ann a FuncDef)
      , Show (Ann a TypeDef)
      , Show (Ann a FFIFunc)
      , Show (Ann a Block)
      , Show (Ann a CaseArm)
      , Show (Ann a Stmt)
      , Show (Ann a Expr)
      , Show (Ann a AssignLHS)
      , Show (Ann a AssignRHS)
      , Show (Ann a IndexingElem)
      , Show (Ann a StructElem)
      ) => Annotation a where
  type Ann a (t :: * -> *)

deriving instance Annotation a => Show (Program a)
deriving instance Annotation a => Show (Decl a)
deriving instance Annotation a => Show (FuncDef a)
deriving instance Annotation a => Show (TypeDef a)
deriving instance Annotation a => Show (FFIFunc a)
deriving instance Annotation a => Show (Block a)
deriving instance Annotation a => Show (CaseArm a)
deriving instance Annotation a => Show (Stmt a)
deriving instance Annotation a => Show (Expr a)
deriving instance Annotation a => Show (AssignLHS a)
deriving instance Annotation a => Show (AssignRHS a)
deriving instance Annotation a => Show (IndexingElem a)
deriving instance Annotation a => Show (StructElem a)

type Annotated (t :: * -> *) (a :: *)
  = (Ann a t, t a)

data SpanA
instance Annotation SpanA where
  type Ann SpanA t = Span

data TypeA
instance Annotation TypeA where
  type Ann TypeA Program = ()
  type Ann TypeA Decl = ()
  type Ann TypeA FuncDef = ()
  type Ann TypeA TypeDef = ()
  type Ann TypeA FFIFunc = ()
  type Ann TypeA Block = (Bool, [(String, Type)])
  type Ann TypeA CaseArm = Type
  type Ann TypeA Stmt = Bool
  type Ann TypeA Expr = Type
  type Ann TypeA AssignLHS = Type
  type Ann TypeA AssignRHS = Type
  type Ann TypeA IndexingElem = (Type, [Type])
  type Ann TypeA StructElem = Type
