module GenARM where

import AST
import CodeGen


genARMInstruction :: IR -> String
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = Var lit } ) 
  = "LDR " ++ "r" ++ (show dest) ++ ", =" ++ (show lit)

--genARMInstruction (IBinOp { iBinOp = op, iDest = dest, 
--                            iLeft  = left, iRight = right } )
--  = 
