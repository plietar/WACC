{
module Main where
}

%name wacc
%tokentype { Token }
%error { parseError }

%token
	begin	{BEGIN}
	is 	{IS}
	end	{end}
	skip	{SKIP}
	read	{READ}
	free	{FREE}
	return	{RETURN}
	exit	{EXIT}
	print	{PRINT}
	println	{PRINTLN}
	is	{IS}
	if	{IF}
	then	{THEN}
	else	{ELSE}
	fi	{FINISH}		
	while	{WHILE}	
	do	{DO}
	done	{DONE}	
	newpair	{NEWPAIR}
	call	{CALL}
	fst	{FST}
	snd	{SND}
	int	{INT}
	bool	{BOOL}
	char	{CHAR}
	string	{STRING}
	pair	{PAIR}
	
	!	{NOT}
	-	{NEGATIVE}
	len	{LEN}
	ord	{ORD}
	chr	{CHR}
	
	*	{MULTIPLY}
	/	{DIV}
	%	{MOD}
	+	{ADD}
	-	{MINUS}
	>	{GREATERTHAN}
	>=	{GREATEROREQUAL}
	<	{LESSTHAN}
	<=	{LESSTHANOREQUAL}
	==	{EQUAL}
	!=	{NOTEQUAL}
	&&	{AND}
	||	{OR}

	'('	{LEFTPARENTHESIS}
	')' 	{RIGHTPARENTHESIS}
	,	{PARAMSEPARATOR}
	= 	{ASSIGN}
	[	{LEFTBRACKET}
	] 	{RIGHTBRACKET}

	#	{COMMENT}

	true 	{TRUE}
	false 	{FALSE}

	
%%

Program : begin Func* Stat end				{ begin $2 $3 end } 

Func : Type Ident '(' ParamList? ')' is Stat end 	{ 

Param-List : Param '(', Param')'*

Param : Type Ident

Stat : skip
	| Type Ident = Assign-RHS
	| Assign-LHS = Assign-RHS
	| read Assign-LHS
	| free Expr
	| return Expr	
	| exit Expr
	| print Expr
	| println Expr
	| if Expr then Stat else Stat fi
	| while Expr do Stat done
	| begin Stat end
	| Stat ; Stat

Assign-LHS : Ident
	| Array-Elem
	| Pair-Elem

Assign-RHS : Expr
	| Array-Liter
	| newpair '(' Expr , Expr ')'
	| Pair-Elem
	| call Ident '(' Arg-List? ')'
	
Arg-List : Expr (, Expr)*

Pair-Elem : fst Expr
	| snd Expr

Type : Base-Type
	| Array-Type
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

CharLiter : 'Character'

StrLiter : " Character* "

Character : ???

EscapedChar : 0|b|t|n|f|r|"|'|\

ArrayLiter : [ (Expr (, Expr)*)? ]

PairLiter : null

Comment : #()*

parseError :: [Token] -> a
parseError _ = error "Parse Error"

d
