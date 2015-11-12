{
module Parser where
import Data.Int

import Tokens
import AST
import Common
}

%name waccParser Program
%name waccParseStmt Stmt
%name waccParseExpr Expr
%monad { WACCResult } { (>>=) } { return }
%tokentype { (Pos, Token) }
%error { parseError }

%token
    begin   { (_, TokKeyword "begin" ) }
    end     { (_, TokKeyword "end" ) }
    skip    { (_, TokKeyword "skip" ) }
    read    { (_, TokKeyword "read" ) }
    free    { (_, TokKeyword "free" ) }
    return  { (_, TokKeyword "return" ) }
    exit    { (_, TokKeyword "exit" ) }
    print   { (_, TokKeyword "print" ) }
    println { (_, TokKeyword "println" ) }
    is      { (_, TokKeyword "is" ) }
    if      { (_, TokKeyword "if" ) }
    then    { (_, TokKeyword "then" ) }
    else    { (_, TokKeyword "else" ) }
    fi      { (_, TokKeyword "fi" ) }
    while   { (_, TokKeyword "while" ) }
    do      { (_, TokKeyword "do" ) }
    done    { (_, TokKeyword "done" ) }
    newpair { (_, TokKeyword "newpair" ) }
    call    { (_, TokKeyword "call" ) }
    fst     { (_, TokKeyword "fst" ) }
    snd     { (_, TokKeyword "snd" ) }
    int     { (_, TokKeyword "int" ) }
    bool    { (_, TokKeyword "bool" ) }
    char    { (_, TokKeyword "char" ) }
    string  { (_, TokKeyword "string" ) }
    pair    { (_, TokKeyword "pair" ) }
    null    { (_, TokKeyword "null" ) }

    len     { (_, TokKeyword "len" ) }
    ord     { (_, TokKeyword "ord" ) }
    chr     { (_, TokKeyword "chr" ) }

    '!'     { (_, TokOp "!" ) }
    '*'     { (_, TokOp "*" ) }
    '/'     { (_, TokOp "/" ) }
    '%'     { (_, TokOp "%" ) }
    '+'     { (_, TokOp "+" ) }
    '-'     { (_, TokOp "-" ) }
    '>'     { (_, TokOp ">" ) }
    '>='    { (_, TokOp ">=" ) }
    '<'     { (_, TokOp "<" ) }
    '<='    { (_, TokOp "<=" ) }
    '=='    { (_, TokOp "==" ) }
    '!='    { (_, TokOp "!=" ) }
    '&&'    { (_, TokOp "&&" ) }
    '||'    { (_, TokOp "||" ) }

    '('     { (_, TokLParen ) }
    ')'     { (_, TokRParen ) }
    ','     { (_, TokComma ) }
    '='     { (_, TokEqual ) }
    '['     { (_, TokLBracket ) }
    ']'     { (_, TokRBracket ) }
    ';'     { (_, TokSemiColon ) }

    IDENT   { (_, TokIdent $$ ) }
    BOOLLIT { (_, TokBoolLit $$ ) }
    STRLIT  { (_, TokStrLit $$ ) }
    CHARLIT { (_, TokCharLit $$ ) }
    INTLIT  { (_, TokIntLit $$ ) }

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
  | string { TyArray TyChar }

PairElemType :: { Type }
  : BaseType { $1 }
  | ArrayType { $1 }
  | pair { TyAny }

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
  | begin Block end { StmtScope $2 }

Expr :: { Expr }
  : INTLIT    {% if $1 > toInteger (maxBound :: Int32)
                 then Error SyntaxError "Integer litteral too large"
                 else OK (ExprLit (LitInt $1)) }
  | BOOLLIT   { ExprLit (LitBool $1) }
  | CHARLIT   { ExprLit (LitChar $1) }
  | STRLIT    { ExprLit (LitString $1) }
  | IDENT     { ExprVar $1 }
  | null      { ExprNull }
  | ArrayElem { ExprArrayElem $1 }

  | '(' Expr ')'   { $2 }

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
  : fst Expr { PairElem PairFst $2 }
  | snd Expr { PairElem PairSnd $2 }

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
parseError :: [(Pos, Token)] -> WACCResult a
parseError ts = Error SyntaxError "Syntax Error"
}

