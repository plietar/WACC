{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Frontend.Lexer where

import Common.Span
import Common.WACCResult
import Frontend.Tokens
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
  "."                  { \(AlexPn _ line column) s -> ((line, column), TokDot) }
  ","                  { \(AlexPn _ line column) s -> ((line, column), TokComma) }
  ";"                  { \(AlexPn _ line column) s -> ((line, column), TokSemiColon) }
  ":"                  { \(AlexPn _ line column) s -> ((line, column), TokColon) }
  "["                  { \(AlexPn _ line column) s -> ((line, column), TokLBracket) }
  "]"                  { \(AlexPn _ line column) s -> ((line, column), TokRBracket) }
  "("                  { \(AlexPn _ line column) s -> ((line, column), TokLParen) }
  ")"                  { \(AlexPn _ line column) s -> ((line, column), TokRParen) }
  "{"                  { \(AlexPn _ line column) s -> ((line, column), TokLBrace) }
  "}"                  { \(AlexPn _ line column) s -> ((line, column), TokRBrace) }

  -- Program Keywords
  begin | end | is | skip | read | free | return | exit | print | println | if
    | then | else | fi | while | do | done | newpair | call | fst | snd | int
    | bool | char | switch | case | string | pair | len | ord | chr | null | tuple | newtuple
    | let | ffi | void | async | await | fire | type | chan
                       { \(AlexPn _ line column) s-> ((line, column), TokKeyword s) }

  -- Operators
  "!" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<="
    | "==" | "!=" | "&&" | "||" | "<-" | "|"
                       { \(AlexPn _ line column) s-> ((line, column), TokOp s) }

  -- Assign Operator
  "="                  { \(AlexPn _ line column) s-> ((line, column), TokEqual) }

  -- Boolean Literals
  "true"               { \(AlexPn _ line column) s-> ((line, column), TokBoolLit True) }
  "false"              { \(AlexPn _ line column) s-> ((line, column), TokBoolLit False) }


  -- Identifier
  ($underscore | $alpha) ($underscore | $alpha | $digit)*
                       { \(AlexPn _ line column) s-> ((line, column), TokIdent s) }

  -- Integer Literal
  @integer_literal     { \(AlexPn _ line column) s-> ((line, column), TokIntLit (read s)) }
  -- Character Literal
  @character_literal   { \(AlexPn _ line column) s-> ((line, column), TokCharLit (read s)) }

  -- String Literals
  @string_literal      { \(AlexPn _ line column) s-> ((line, column), TokStrLit (read s)) }


{
waccLexer :: String -> String -> WACCResult [(Pos, Token)]
waccLexer fname str = fmap (addfname fname) $ go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> OK []
                AlexError ((AlexPn _ line column),chr,_,str)
                  -> lexicalError $ show fname ++ " (line " ++ show line ++  ", column "  ++ show column
                                               ++ "):\n" ++ "Token recognition error at: "
                                               ++ (chr : firstWord str)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap (\tokens -> act pos (take len str) : tokens) (go inp')


firstWord str = takeWhile (/= ' ') str

addfname :: String -> [((Int, Int), Token)] -> [((Int, Int, String), Token)]
addfname fname xs = map (\((line, col), tok) -> ((line, col, fname), tok)) xs

}

