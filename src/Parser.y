{
module Parser where
}

%name wacc
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

    len     { TokKeyword "len" }
    ord     { TokKeyword "ord" }
    chr     { TokKeyword "chr" }

    true    { TokKeyword "true" }
    false   { TokKeyword "false" }

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

%%

Program: begin  StmtList end { Program [] $2 }

StmtList: Stmt { [$1] }
        | Stmt ';' StmtList { $1 : $3 }

Stmt: skip { StmtSkip }
    | Type IDENT '=' AssignRHS { StmtVar $1 $2 $4 }
    | AssignLHS '=' AssignRHS { StmtAssign $1 $3 }
    | read AssignLHS { StmtRead $2 }
    | free Expr { StmtFree $2 }
    | return Expr { StmtReturn $2 }
    | exit Expr { StmtExit $2 }
    | print Expr { StmtPrint False $2 }
    | println Expr { StmtPrint True $2 }


Func: Type IDENT '(' ParamList ')' is StmtList end
      { FuncDef $1 $2 $4 $7 }
FuncList : Func FuncList { $1 : $2 }
         | { [] }

Type: int { TyInt }

Param: Type IDENT { ($1, $2) }
ParamList: { [] }
         | Param ',' ParamList { $1 : $3 }

Expr: IDENT { ExprVar $1 }
AssignLHS: IDENT { LHSVar $1 }
AssignRHS: Expr { RHSExpr $1 }

-- 
-- Stat : skip {}
--      | Type IDENT '=' AssignRHS {}
--      | AssignLHS '=' AssignRHS {}
--      | read AssignLHS {}
--      | free Expr {}
--      | return Expr {}
--      | exit Expr {}
--      | print Expr {}
--      | println Expr {}
--      | if Expr then Stat else Stat fi {}
--      | while Expr do Stat done {}
--      | begin Stat end {}
--      | Stat ';' Stat {}
-- 
-- AssignLHS : IDENT {}
--           | ArrayElem {}
--           | PairElem {}
-- 
-- AssignRHS : Expr {}
--           | ArrayLiter {}
--           | newpair '(' Expr ',' Expr ')' {}
--           | PairElem {}
--           | call IDENT '(' ArgList ')' {}
-- 
-- ExprList : Expr ExprList {}
--          | {}
-- 
-- ArgList : Expr ',' ExprList {}
-- 
-- PairElem : fst Expr {}
--          | snd Expr {}
-- 
-- Type : BaseType {}
--      | ArrayType {}
--      | pair {}
-- 
-- BaseType : int    {Int $1}
--          | bool   {Bool $1}
--          | char   {Char $1}
--          | string {String $1}
-- 
-- ArrayType : Type '[' ']' {}
-- 
-- PairType : pair '(' PairElemType ',' PairElemType ')' {}
-- 
-- PairElemType : BaseType {}
--              | ArrayType {}
--              | pair {}
-- 
-- Expr : IntLiter  {IntLiter $1}
--      | BoolLiter {BoolLiter $1}
--      | CharLiter {CharLiter $1}
--      | StrLiter  {StrLiter $1} 
--      | PairLiter {PairLiter $1}
--      | IDENT     {Ident $1}
--      | ArrayElem {ArrayElem $1}
--      | UnaryOper Expr {UnaryOper $2}
--      | Expr BinaryOper Expr {BinaryOper $1 $3}
--      | '(' Expr ')' {}
-- 
-- UnaryOper : '!' {}
--           | '-' {}
--           | len {}
--           | ord {}
--           | chr {}
-- 
-- BinaryOper : '*' {}
--            | '/' {}
--            | '%' {}
--           |+|-|>|>=|<|<=|==|!=|&&|||

-- Ident : (_|a-z|A-Z)(_|a-z|A-Z|0-9)*

-- ArrayElem : Ident ([ Expr ])+ 
-- 
-- IntLiter : IntSign? Digit+
-- 
-- Digit : (0-9)
-- 
-- IntSign : +|-
-- 
-- BoolLiter : true|false
-- 
-- CharLiter : ' Character '
-- 
-- StrLiter : " Characters "
-- 
-- Character : ???
-- 
-- EscapedChar : 0|b|t|n|f|r|"|'|\
-- 
-- ArrayLiter : [ (Expr (, Expr)*)? ]
-- 
-- PairLiter : null
-- 
-- Comment : #()*

{
parseError :: [Token] -> a
parseError _ = error "Parse Error"

data Program = Program [FuncDef] [Stmt]
data FuncDef = FuncDef Type String [(Type, String)] [Stmt]
data Type = TyInt
          | TyString
          | TyPair Type Type
          | TyNestedPair

data AssignLHS = LHSVar String
data AssignRHS = RHSExpr Expr
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

data Literal = LitInt Integer
             | LitBool Bool
             | LitChar Char
             | LitString String

data UnOp = UnOpNot
          | UnOpNeg
          | UnOpLen
          | UnOpOrd
          | UnOpChr

data BinOp = BinOpAdd
           | BinOpSub
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
          | ExprArrayElem Expr Expr
          | ExprUnOp UnOp Expr
          | ExprBinOp BinOp Expr Expr

data Token = TokKeyword String
           | TokIdent String
           | TokOp String
           | TokLBracket | TokRBracket | TokEqual | TokSemiColon
           | TokComma | TokLParen | TokRParen
}

