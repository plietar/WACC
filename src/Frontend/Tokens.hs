module Frontend.Tokens where

data Token =
    TokKeyword String |
    TokOp String      |
    TokLParen         |
    TokRParen         |
    TokLBracket       |
    TokRBracket       |
    TokLBrace         |
    TokRBrace         |
    TokDot            |
    TokComma          |
    TokEqual          |
    TokSemiColon      |
    TokColon          |
    TokIdent String   |
    TokBoolLit Bool   |
    TokStrLit String  |
    TokCharLit Char   |
    TokIntLit Integer
    deriving (Eq)

instance Show Token where
  show (TokKeyword k)   = show k
  show (TokOp op)       = "operator " ++ show op
  show TokLParen        = "left parenthesis"
  show TokRParen        = "right parenthesis"
  show TokDot           = "dot"
  show TokComma         = "comma"
  show TokEqual         = "equal"
  show TokLBracket      = "left bracket"
  show TokRBracket      = "right bracket"
  show TokLBrace        = "left brace"
  show TokRBrace        = "right brace"
  show TokSemiColon     = "semicolon"
  show TokColon         = "colon"
  show (TokIdent ident) = "identifier " ++ show ident
  show (TokBoolLit _)   = "boolean literal"
  show (TokStrLit _)    = "string literal"
  show (TokCharLit _)   = "character literal"
  show (TokIntLit _)    = "integer literal"
