{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Lexer where
import Tokens
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
    | bool | char | string | pair | len | ord | chr | null | true | false
                       { \s -> TokKeyword s}
 
  -- Operators
  "!" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<="
    | "==" | "!=" | "&&" | "||"
                       { \s -> TokOp s}

  -- Assign Operator
  "="                  { \s -> TokEqual }

  -- Identifier
  ($underscore | $alpha) ($underscore | $alpha | $digit)*
                       { \s -> TokIdent s}
 
  -- Integer Literal
  @integer_literal     { \s -> TokIntLit (read s) }
 
  -- Boolean Literals
  "true"               { \s -> TokBoolLit True }
  "false"              { \s -> TokBoolLit False }

  -- Character Literal
  @character_literal   { \s -> TokCharLit (read s) }

  -- String Literals
  @string_literal      { \s -> TokStrLit (read s) }


{
-- Each action has type :: String -> Token

waccLexer = alexScanTokens

}

