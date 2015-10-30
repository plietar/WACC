{
module Main where
}

%name wacc
%tokentype { Token }
%error { parseError }

%token
	begin	{BEGIN}
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

	(	{LEFTPARENTHESIS}
	) 	{RIGHTPARENTHESIS}
	,	{PARAMSEPARATOR}
	= 	{ASSIGN}
	[	{LEFTBRACKET}
	] 	{RIGHTBRACKET}

	#	{COMMENT}

	true 	{TRUE}
	false 	{FALSE}

	
%%

Program : begin Func* Stat end

Func : Type Ident (, Param)*

Param-List : Param (, Param)*

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
	| newpair ( Expr , Expr )
	| Pair-Elem
	| call Ident ( Arg-List? )
	
Arg-List : Expr (, Expr)*

Pair-Elem : fst Expr
	| snd Expr

Type : Base-Type
	| Array-Type
	| pair

Expr : Int-Liter
	| Bool-Liter
	| Char-Liter
	| Str-Liter
	| Pair-Liter
	| Ident
	| Array-Elem
	| Unary-Oper Expr
	| Expr Binary-Oper Expr
	| ( Expr )

Unary-Oper : !|-|len|ord|chr

Binary-Oper : *|/|%|+|-|>|>=|<|<=|==|!=|&&|||

Ident : (_|a-z|A-Z)(_|a-z|A-Z|0-9)*

Array-Elem : Ident [ Expr ] + 

Int-Liter : Int-Sign? Digit+

Digit : 0-9

Int-Sign : +|-

Bool-Liter : true|false

Char-Liter : 'Character'

Str-Liter : " Character* "

Character : ???

Escaped-Char : 0|b|t|n|f|r|"|'|\

Array-Liter : [ Expr (, Expr)*)? ]

Pair-Liter : null

Comment : #()*
