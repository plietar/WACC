{
module Parser where
import Lexer
}

%name waccParser
%tokentype { Token }
%error { parseError }

%token
    begin   { TokKeyword "begin" }
    end     { TokKeyword "end" }
    skip    { TokKeyword "skip" }
    read    { TokKeyword "read" }
    free    { TokKeyword "free" }
    return  { TokKeyword "return" }
    exit    { TokKeyword "exit" }
    print   { TokKeyword "print" }
    println { TokKeyword "println" }
    is      { TokKeyword "is" }
    if      { TokKeyword "if" }
    then    { TokKeyword "then" }
    else    { TokKeyword "else" }
    fi      { TokKeyword "fi" }
    while   { TokKeyword "while" }
    do      { TokKeyword "do" }
    done    { TokKeyword "done" }
    newpair { TokKeyword "newpair" }
    call    { TokKeyword "call" }
    fst     { TokKeyword "fst" }
    snd     { TokKeyword "snd" }
    int     { TokKeyword "int" }
    bool    { TokKeyword "bool" }
    char    { TokKeyword "char" }
    string  { TokKeyword "string" }
    pair    { TokKeyword "pair" }
    null    { TokKeyword "null" }

    len     { TokKeyword "len" }
    ord     { TokKeyword "ord" }
    chr     { TokKeyword "chr" }

    '!'     { TokOp "!" }
    '*'     { TokOp "*" }
    '/'     { TokOp "/" }
    '%'     { TokOp "%" }
    '+'     { TokOp "+" }
    '-'     { TokOp "-" }
    '>'     { TokOp ">" }
    '>='    { TokOp ">=" }
    '<'     { TokOp "<" }
    '<='    { TokOp "<=" }
    '=='    { TokOp "==" }
    '!='    { TokOp "!=" }
    '&&'    { TokOp "&&" }
    '||'    { TokOp "||" }

    '('     { TokLParen }
    ')'     { TokRParen }
    ','     { TokComma }
    '='     { TokEqual }
    '['     { TokLBracket }
    ']'     { TokRBracket }
    ';'     { TokSemiColon }

    IDENT   { TokIdent $$ }
    BOOLLIT { TokBoolLit $$ }
    STRLIT  { TokStrLit $$ }
    CHARLIT { TokCharLit $$ }
    INTLIT  { TokIntLit $$ }

%left '||'
%left '&&'
%nonassoc '==' '!='
%nonassoc '<='
%nonassoc '<'
%nonassoc '>='
%nonassoc '>'
%left '+' '-'
%left '*' '/' '%'
%nonassoc '!' NEG len ord chr

%%

Program :: { Program }
  : begin FuncList_rev Block end { Program (reverse $2) $3 }

-- For some reason many(Func) causes a shift-reduce conflict when used in Program
FuncList_rev :: { [FuncDef] }
  : FuncList_rev Func { $2 : $1 }
  | { [] }

Func :: { FuncDef }
  : Type IDENT '(' sepBy(Param, ',') ')' is Block end { FuncDef $1 $2 $4 $7 }

BaseType :: { Type }
  : int { TyInt }
  | bool { TyBool }
  | char { TyChar }
  | string { TyString }

PairElemType :: { Type }
  : BaseType { $1 }
  | ArrayType { $1 }
  | pair { TyNestedPair }

ArrayType :: { Type }
  : Type '[' ']' { TyArray $1 }

PairType :: { Type }
  : pair '(' PairElemType ',' PairElemType ')' { TyPair $3 $5 }

Type :: { Type }
  : BaseType  { $1 }
  | ArrayType { $1 }
  | PairType  { $1 }

Param :: { (Type, String) }
  : Type IDENT { ($1, $2) }

Block :: { [Stmt] }
  : sepBy1(Stmt, ';') { $1 }

Stmt :: { Stmt }
  : skip { StmtSkip }
  | Type IDENT '=' AssignRHS { StmtVar $1 $2 $4 }
  | AssignLHS '=' AssignRHS { StmtAssign $1 $3 }
  | read AssignLHS { StmtRead $2 }
  | free Expr { StmtFree $2 }
  | return Expr { StmtReturn $2 }
  | exit Expr { StmtExit $2 }
  | print Expr { StmtPrint False $2 }
  | println Expr { StmtPrint True $2 }
  | if Expr then Block else Block fi { StmtIf $2 $4 $6 }
  | while Expr do Block done { StmtWhile $2 $4 }

Expr :: { Expr }
  : INTLIT    { ExprLit (LitInt $1) }
  | BOOLLIT   { ExprLit (LitBool $1) }
  | CHARLIT   { ExprLit (LitChar $1) }
  | STRLIT    { ExprLit (LitString $1) }
  | IDENT     { ExprVar $1 }
  | null      { ExprNull }
  | ArrayElem { ExprArrayElem $1 }

  | '!' Expr  { ExprUnOp UnOpNot $2 }
  | '-' Expr %prec NEG { ExprUnOp UnOpNeg $2 }
  | len Expr  { ExprUnOp UnOpLen $2 }
  | ord Expr  { ExprUnOp UnOpOrd $2 }
  | chr Expr  { ExprUnOp UnOpChr $2 }

  | Expr '+' Expr  { ExprBinOp BinOpAdd $1 $3 }
  | Expr '-' Expr  { ExprBinOp BinOpSub $1 $3 }
  | Expr '*' Expr  { ExprBinOp BinOpMul $1 $3 }
  | Expr '/' Expr  { ExprBinOp BinOpDiv $1 $3 }
  | Expr '%' Expr  { ExprBinOp BinOpRem $1 $3 }
  | Expr '>' Expr  { ExprBinOp BinOpGT  $1 $3 }
  | Expr '>=' Expr { ExprBinOp BinOpGE  $1 $3 }
  | Expr '<' Expr  { ExprBinOp BinOpLT  $1 $3 }
  | Expr '<=' Expr { ExprBinOp BinOpLE  $1 $3 }
  | Expr '==' Expr { ExprBinOp BinOpEQ  $1 $3 }
  | Expr '!=' Expr { ExprBinOp BinOpNE  $1 $3 }
  | Expr '&&' Expr { ExprBinOp BinOpAnd $1 $3 }
  | Expr '||' Expr { ExprBinOp BinOpOr  $1 $3 }

AssignLHS :: { AssignLHS }
  : IDENT     { LHSVar $1 }
  | PairElem  { LHSPair $1 }
  | ArrayElem { LHSArray $1 }

AssignRHS :: { AssignRHS }
  : Expr     { RHSExpr $1 }
  | ArrayLit { RHSArrayLit $1 }
  | newpair '(' Expr ',' Expr ')' { RHSNewPair $3 $5 }
  | PairElem { RHSPair $1 }
  | call IDENT '(' sepBy(Expr, ',') ')' { RHSCall $2 $4 }

ArrayIndex :: { Expr }
  : '[' Expr ']' { $2 }

ArrayElem :: { ArrayElem }
  : IDENT many1(ArrayIndex) { ArrayElem $1 $2 }

PairElem :: { PairElem }
  : fst Expr { PairFst $2 }
  | snd Expr { PairSnd $2 }

ArrayLit :: { [Expr] }
  : '[' sepBy(Expr, ',') ']' { $2 }

many_rev1(p)
  : p               { [$1] }
  | many_rev1(p) p  { $2 : $1 }

many1(p)
  : many_rev1(p)    { reverse $1 }

many(p)
  : many1(p)        { $1 }
  |                 { [] }

sepR(p,q)
  : p q             { $2 }

sepL(p,q)
  : p q             { $1 }

sepBy1(p,q)
  : p many(sepR(q,p)) { $1 : $2 }

sepBy(p,q)
  : sepBy1(p,q)       { $1 }
  |                   { [] }

endBy(p,q)
  : many (sepL(p,q))  { $1 }

endBy1(p,q)
  : many1 (sepL(p,q)) { $1 }

{
parseError :: [Token] -> a
parseError ts = error ("Parse Error " ++ show ts)

data Program = Program [FuncDef] [Stmt]
    deriving (Show)
data FuncDef = FuncDef Type String [(Type, String)] [Stmt]
    deriving (Show)
data Type = TyInt
          | TyBool
          | TyChar
          | TyString
          | TyPair Type Type
          | TyNestedPair
          | TyArray Type
    deriving (Show)

data ArrayElem = ArrayElem String [Expr]
    deriving (Show)
data PairElem  = PairFst Expr | PairSnd Expr
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
}

