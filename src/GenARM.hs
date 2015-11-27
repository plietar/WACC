module GenARM where

import AST
import CodeGen


genARMInstruction :: IR -> String
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitInt n } ) 
  = "LDR " ++ "r" ++ (show dest) ++ ", =" ++ (show n)
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool True } ) 
  = "MOV " ++ "r" ++ (show dest) ++ ", #" ++ (show 1)
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool False } ) 
  = "MOV " ++ "r" ++ (show dest) ++ ", #" ++ (show 0)
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitChar chr } ) 
  = "MOV " ++ "r" ++ (show dest) ++ ", #" ++ (show chr)
-- Need a reference of the number of strings in the scope.
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitString str  } ) 
  = "LDR " ++ "r" ++ (show dest) ++ ", =msg_" ++ (show 0)




genARMInstruction (IBinOp { iBinOp = op, iDest = dest, 
        iLeft  = left, iRight = right } )
  	= case op of  
    BinOpAdd -> "" 
    BinOpSub -> ""
    BinOpMul -> ""
