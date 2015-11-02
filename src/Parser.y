{
module Main where
}

%name wacc
%tokentype { Token }
%error { parseError }

%token
	begin	{TokBegin}
	is 	{TokIs}
	end	{TokEnd}
	skip	{TokSkip}
	read	{TokRead}
	free	{TokFree}
	return	{TokReturn}
	exit	{TokExit}
	print	{TokPrint}
	println	{TokPrintln}
	is	{TokIs}
	if	{TokIf}
	then	{TokThen}
	else	{TokElse}
	fi	{TokFinish}		
	while	{TokWhile}	
	do	{TokDo}
	done	{TokDone}	
	newpair	{TokNewpair}
	call	{TokCall}
	fst	{TokFst}
	snd	{TokSnd}
	int	{TokInt}
	bool	{TokBool}
	char	{TokChar}
	string	{TokString}
	pair	{TokPair}
	
	!	{TokNot}
	-	{TokNegative}
	len	{TokLen}
	ord	{TokOrd}
	chr	{TokChr}
	
	*	{TokMultiply}
	/	{TokDiv}
	%	{TokMod}
	+	{TokAdd}
	-	{TokMinus}
	>	{TokGT}
	>=	{TokGTE}
	<	{TokLT}
	<=	{TokLTE}
	==	{TokDoubleEqual}
	!=	{TokNotEqual}
	&&	{TokAnd}
	||	{TokOr}

	'('	{TokLParen}
	')' 	{TokRParen}
	,	{TokComma}
	= 	{TokEqual}
	[	{TokLBracket}
	] 	{TokRBracket}

	#	{TokComment}

	true 	{TokTrue}
	false 	{TokFalse}

	
%%

FuncList : Func FuncList | nil 

Program : begin FuncList Stat end				{ begin $2 $3 end } 

Func : Type Ident '(' ParamList ')' is Stat end 

ParamList : Param , ParamList | 

Param : Type Ident

Stat : skip
	| Type Ident = AssignRHS
	| AssignLHS = AssignRHS
	| read AssignLHS
	| free Expr
	| return Expr	
	| exit Expr
	| print Expr
	| println Expr
	| if Expr then Stat else Stat fi
	| while Expr do Stat done
	| begin Stat end
	| Stat ; Stat

AssignLHS : Ident
	| ArrayElem
	| PairElem

AssignRHS : Expr
	| ArrayLiter
	| newpair '(' Expr , Expr ')'
	| Pair-Elem
	| call Ident '(' ArgList ')'
	
ExprList : Expr ExprList | 

ArgList : Expr , ExprList

PairElem : fst Expr
	| snd Expr

Type : BaseType
	| ArrayType
	| pair

BaseType : int						{Int $1}
	| bool						{Bool $1}
	| char 						{Char $1}
	| string					{String $1}

ArrayType : Type [ ] 				

PairType : pair '(' PairElemType , PairElemType ')'

PairElemType : BaseType
	| ArrayType
	| pair

Expr : IntLiter						{IntLiter $1}
	| BoolLiter					{BoolLiter $1}
	| CharLiter					{CharLiter $1}
	| StrLiter					{StrLiter $1}	
	| PairLiter					{PairLiter $1}
	| Ident						{Ident $1}
	| ArrayElem					{ArrayElem $1}
	| UnaryOper Expr				{UnaryOper $2}
	| Expr BinaryOper Expr				{BinaryOper $1 $3}
	| ( Expr )					{}

UnaryOper : !|-|len|ord|chr

BinaryOper : *|/|%|+|-|>|>=|<|<=|==|!=|&&|||

Ident : (_|a-z|A-Z)(_|a-z|A-Z|0-9)*

ArrayElem : Ident ([ Expr ])+ 

IntLiter : IntSign? Digit+

Digit : (0-9)

IntSign : +|-

BoolLiter : true|false

CharLiter : ' Character '

StrLiter : " Characters "

Character : ???

EscapedChar : 0|b|t|n|f|r|"|'|\

ArrayLiter : [ (Expr (, Expr)*)? ]

PairLiter : null

Comment : #()*

parseError :: [Token] -> a
parseError _ = error "Parse Error"

data Expr 
  = LitExpr | VarExpr String | ArrayExpr [Expr] | UnExpr String | BinExpr String

data Statement
  = Skip | VarState | AssignState | ReadState | MemFreeState | Return | Exit | Print | CondBranch | While | ScopingState | SequentialComp

    
