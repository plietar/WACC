{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Lexer where
import Tokens
import Common
}

%wrapper "basic"

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
  ","                  { \s -> TokComma }
  ";"                  { \s -> TokSemiColon }
  "["                  { \s -> TokLBracket }
  "]"                  { \s -> TokRBracket }
  "("                  { \s -> TokLParen }
  ")"                  { \s -> TokRParen }

  -- Program Keywords
  begin | end | is | skip | read | free | return | exit | print | println | if
    | then | else | fi | while | do | done | newpair | call | fst | snd | int
    | bool | char | string | pair | len | ord | chr | null
                       { \s -> TokKeyword s}
 
  -- Operators
  "!" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<="
    | "==" | "!=" | "&&" | "||"
                       { \s -> TokOp s}

  -- Assign Operator
  "="                  { \s -> TokEqual }

  -- Boolean Literals
  "true"               { \s -> TokBoolLit True }
  "false"              { \s -> TokBoolLit False }


  -- Identifier
  ($underscore | $alpha) ($underscore | $alpha | $digit)*
                       { \s -> TokIdent s}
 
  -- Integer Literal
  @integer_literal     { \s -> TokIntLit (read s) }
  -- Character Literal
  @character_literal   { \s -> TokCharLit (read s) }

  -- String Literals
  @string_literal      { \s -> TokStrLit (read s) }


{
-- Each action has type :: String -> Token

--waccLexer = alexScanTokens
waccLexer str = go ('\n',[],str)
  where go inp@(_,_bs,s) =
          case alexScan inp 0 of
                AlexEOF -> OK []
                AlexError _ -> Error LexicalError "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap (\tokens -> act (take len s) : tokens) (go inp')

}

