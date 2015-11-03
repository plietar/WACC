{
module Main (main) where
}

%wrapper "basic"

$digit 			= 0-9			-- digits
$alpha 			= [a-zA-Z]		-- alphabetic characters
$underscore 	= \_
$graphic    	= $printable # $white
$non_character 	= [ \\ \' \" ]
$escaped_char 	= [ '0' 'b' 't' 'n' 'f' 'r' \" \' \\]
$int_sign 		= [\+ \-]

@character 			= ($graphic # $non_character) | (\\ $escaped_char)
@string_literal 	= \" (@character | $white)* \"
@character_literal 	= \' @character  \'
@integer_literal 	= ($int_sign)? ($digit)+
@whitespace 		= ($white | (\\ $escaped_char))+

tokens :-

-- Whitespace
  @whitespace				;

-- Comments
  "#".*(\n)				;	
 
-- Symbols
  "," 					{ \s -> TokComma }
  ";"					{ \s -> TokSemiColon }
  "["					{ \s -> TokLBracket }
  "]"					{ \s -> TokRBracket }
  "("					{ \s -> TokLParen }
  ")"					{ \s -> TokRParen } 

-- Program Keywords
  begin | end | is | skip | read | free | return | exit | print | println | if 
	| then | else | fi | while | do | done | newpair | call | fst | snd | int 
	| bool | char | string | pair | len | ord | chr | null | true | false
						{ \s -> TokKeyword s}
 
-- Operators
  "!" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<=" 
	  | "==" | "!=" | "&&" | "||"			
						{ \s -> TokOp s}

-- Assign Operator
  "="					{ \s -> TokEqual }

-- Identifier
  ($underscore | $alpha) ($underscore | $alpha | $digit)* 	{ \s -> TokIdent s}
 
-- Integer Literal
  @integer_literal		{ \s -> TokIntLit s }
 
-- Boolean Literals
  "true" | "false"		{ \s -> TokBoolLit s }

-- Character Literal
  @character_literal	{ \s -> removeQuotes (TokCharLit s) } 

-- String Literals
  @string_literal	 	{ \s -> removeQuotes (TokStrLit s) } 


{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	TokKeyword String 	|
	TokOp String		|
	TokLParen  			|
	TokRParen  			|
	TokComma   			| 
	TokEqual   			|
	TokLBracket	  		|
	TokRBracket	 		|
	TokSemiColon 		|
	TokIdent String 	|
	TokBoolLit String 	|
	TokStrLit String 	|
	TokCharLit String 	| 
	TokIntLit String 
	deriving (Eq,Show)

removeQuotes :: Token -> Token
removeQuotes (TokStrLit s) = TokStrLit $ tail $ take (length s - 1) s
removeQuotes (TokCharLit s) = TokCharLit $ tail $ take (length s - 1) s



main = do
  s <- getContents
  print (alexScanTokens s)

}
