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


