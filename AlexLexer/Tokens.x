{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$underscore = \_
$graphic    = $printable # $white
@string     = \" ($graphic # \")* \"

tokens :-

-- Whitespace
  $white+				;

-- Symbols
  "," 			{ \s -> TokComma }
  ";"			{ \s -> TokSemicol }
  "["			{ \s -> TokLBrack }
  "]"			{ \s -> TokRBrack }
  "("			{ \s -> TokLParen }
  ")"			{ \s -> TokRParen }

-- Operators
  \! | \* | \/ | \% | \+ | \- | \> | \>= | \< | \<= | \== | \!= | \&& 
	| \|\|													{ \s -> TokOp s}

-- Assign Operator
  "="			{\s -> TokEqual }

-- Boolean Literals
  true | false 		{ \s -> TokBool s }

-- Character Literals
  @string 	{ \"'":s:"'":rest -> TokString s } 

-- Program Keywords
  begin | end | is | skip | read | free | return | exit | print | println 
	| if | then | else | fi | while | do | done | newpair | call | fst | snd | int 
	| bool | char | string | pair | len | ord | chr | null | true | false
		{ \s -> TokKeyword s}

-- Identifier
  ($underscore | $alpha) ($underscore | $alpha | $digit)* 	{ \s -> TokIdent s}
  
-- Comments
  "#".*(\n)				;	

-- Integer
  $digit+				{ \s -> TokInt s }
  

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
	TokLBrack  |
	TokRBrack  |
	TokLParen  |
	TokRParen  |
	TokEqual   |
	TokBool String |
	TokString String
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)

}
