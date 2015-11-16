module Tokens where

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
    deriving (Eq,Show)

type Pos = (Int, Int, String)
