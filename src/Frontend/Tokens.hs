module Frontend.Tokens where

data Token =
    TokKeyword String | 
    TokOp String      | 
    TokLParen         | 
    TokRParen         | 
    TokComma          | 
    TokEqual          | 
    TokLBracket       | 
    TokRBracket       | 
    TokSemiColon      | 
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
  show TokComma         = "comma"
  show TokEqual         = "equal"
  show TokLBracket      = "left bracket"
  show TokRBracket      = "right bracket"
  show TokSemiColon     = "semicolon"
  show (TokIdent ident) = "identifier " ++ show ident
  show (TokBoolLit _)   = "boolean literal"
  show (TokStrLit _)    = "string literal"
  show (TokCharLit _)   = "character literal"
  show (TokIntLit _)    = "integer literal"
