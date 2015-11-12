{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Lexer where
import Tokens
import Common
}

%wrapper "posn"

$digit             = 0-9
$alpha             = [a-zA-Z]
$underscore        = \_
$non_character     = [ \\ \' \" ]
$escaped_char      = [ '0' 'b' 't' 'n' 'f' 'r' \" \' \\]
$int_sign          = [\+ \-]

@character         = (. # $non_character) | (\\ $escaped_char)
@string_literal    = \" (@character)* \"
@character_literal = \' @character  \'
@integer_literal   = ($int_sign)? ($digit)+
@whitespace        = ($white | (\\ $escaped_char))+

tokens :-

-- Whitespace
  @whitespace          ;

  -- Comments
  "#".*(\n)            ;
 
  -- Symbols
  ","                  { \p s -> TokComma }
  ";"                  { \p s -> TokSemiColon }
  "["                  { \p s -> TokLBracket }
  "]"                  { \p s -> TokRBracket }
  "("                  { \p s -> TokLParen }
  ")"                  { \p s -> TokRParen }

  -- Program Keywords
  begin | end | is | skip | read | free | return | exit | print | println | if
    | then | else | fi | while | do | done | newpair | call | fst | snd | int
    | bool | char | string | pair | len | ord | chr | null
                       { \p s-> TokKeyword s}
 
  -- Operators
  "!" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<="
    | "==" | "!=" | "&&" | "||"
                       { \p s-> TokOp s}

  -- Assign Operator
  "="                  { \p s-> TokEqual }

  -- Boolean Literals
  "true"               { \p s-> TokBoolLit True }
  "false"              { \p s-> TokBoolLit False }


  -- Identifier
  ($underscore | $alpha) ($underscore | $alpha | $digit)*
                       { \p s-> TokIdent s}
 
  -- Integer Literal
  @integer_literal     { \p s-> TokIntLit (read s) }
  -- Character Literal
  @character_literal   { \p s-> TokCharLit (read s) }

  -- String Literals
  @string_literal      { \p s-> TokStrLit (read s) }


{
--alexScanTokens :: String -> [token]
waccLexer str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> OK []
                AlexError ((AlexPn _ line column),_,_,_) 
					-> Error LexicalError $ "at " ++ (show line) ++  ":"  ++ (show column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap (\tokens -> act pos (take len str) : tokens) (go inp')
}
