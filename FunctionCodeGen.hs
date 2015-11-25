module FuntionCodeGen where

import AST
import BlockGen
import ARMTypes

transFunction :: (Annotated FuncDef TypeA)-> WACCResult [Instruction]
transFunction returnType fname params body
  = [Define fname] ++ [PUSH lr] ++ genBlock p body ++ [POP pc]
  where 
    p = saveParam params 1 Map.empty

saveParam :: [(Type, String)] -> Int -> Map String Int -> Map String Int
saveParam [] _ t = t
saveParam [(_, s) : rest] index table 
  = saveParam rest (index + 1) (Map.insert s index table)


