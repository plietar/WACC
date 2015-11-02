{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$underscore = \_
-- $binOp = [ \* \/ \% \+ \- \> \>= \< \<= \== \!= \&& \|| ]
-- $binOp = \* | \/ | \% | \+ | \- | \> | \>= | \< | \<= | \== | \!= | \&& | \|| 
tokens :-
  $white+				;
  \! | \- | len | ord | chr		{ \s -> UnaryOp s }
--   $binOp						{ \s -> BinOp s}
  "--".*				;
  \* | \/ | \% | \+ | \- | \> | \>= | \< | \<= | \== | \!= | \&& | \|\|	{ \s -> BinOp s}
  let					{ \s -> Let }
  in					{ \s -> In }
  $digit+				{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]			{ \s -> Sym (head s) }
  j{5}					{ \s -> MUCHASJOTAS}
  [\! \-]	{ \s -> UnaryOp s }
  
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	UnaryOp String |
	BinOp String |
	Let 		|
	In  		|
	Sym Char	|
	Var String	|
	Int Int		|
	Underscore	|
	MUCHASJOTAS
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)

}
