{
module Main (main) where
}

%wrapper "basic"

$digit 		= 0-9			-- digits
$alpha 		= [a-zA-Z]		-- alphabetic characters
$underscore = \_
$graphic    = $printable # $white
$non_character 	= [ \\ \' \" ]
$escaped_char 	= [ '0' 'b' 't' 'n' 'f' 'r' \" \' \\]
$int_sign = [\+ \-]
--$binary_op = '*'  '/'  '%'  '+'  '-'  '>'  '>='  '<'  '<='  '=='  '!='  '&&'  '||'

@character 	= ($graphic # $non_character) | (\\ $escaped_char)
@string_literal 	= \" (@character)* \"
@character_literal 	= \' @character  \'
@integer_literal 	= ($int_sign)? ($digit)+

tokens :-

-- Whitespace
  $white+				;
 -- $binary_op {\s -> TokOp s}

-- Symbols
  "," 			{ \s -> TokComma }
  ";"			{ \s -> TokSemicol }
  "["			{ \s -> TokLBracket }
  "]"			{ \s -> TokRBracket }
  "("			{ \s -> TokLParen }
  ")"			{ \s -> TokRParen } 
-- Operators
  "!" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||"
						{ \s -> TokOp s}

-- Assign Operator
  "="					{\s -> TokEqual }

-- Boolean Literals
  "true" | "false" 			{ \s -> TokBool s }

-- String Literals
  @string_literal	 	{ \s -> removeQuotes (TokStrLit s) } 

-- Character Literal
  @character_literal	{ \s -> removeQuotes (TokCharLit s) } 

-- Program Keywords
  begin | end | is | skip | read | free | return | exit | print | println 
	| if | then | else | fi | while | do | done | newpair | call | fst | snd | int 
	| bool | char | string | pair | len | ord | chr | null | true | false
						{ \s -> TokKeyword s}

-- Identifier
  ($underscore | $alpha) ($underscore | $alpha | $digit)* 	{ \s -> TokIdent s}
  
-- Comments
  "#".*(\n)				;	

-- Integer Literal
  
-- Integer
  @integer_literal				{ \s -> TokIntLit s }
  

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	TokOp String	|
	TokIdent String |
	TokKeyword String |
	TokInt String | 
	TokComma   | 
	TokSemicol |
	TokLBracket  |
	TokRBracket  |
	TokLParen  |
	TokRParen  |
	TokEqual   |
	TokBool String |
	TokStrLit String |
	TokCharLit String | 
	TokIntLit String 
	deriving (Eq,Show)

removeQuotes :: Token -> Token
removeQuotes (TokStrLit s) = TokStrLit $ tail $ take (length s - 1) s
removeQuotes (TokCharLit s) = TokCharLit $ tail $ take (length s - 1) s
main = do
  s <- getContents
  print (alexScanTokens s)

}
