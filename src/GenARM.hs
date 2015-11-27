module GenARM where

import AST
import CodeGen


genARMInstruction :: IR -> [String]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitInt n } ) 
  = ["LDR r" ++ (show dest) ++ ", =" ++ (show n)]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool True } ) 
  = ["MOV r" ++ (show dest) ++ ", #" ++ (show 1)]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool False } ) 
  = ["MOV r" ++ (show dest) ++ ", #" ++ (show 0)]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitChar chr } ) 
  = ["MOV r" ++ (show dest) ++ ", #" ++ (show chr)]
-- Need a reference of the number of strings in the scope.
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitString str  } ) 
  = ["LDR r" ++ (show dest) ++ ", =msg_" ++ (show 0)]

--BinOp
--Missing divide and mod binops, ARM calls a label "__aeabi_idiv" 
--but doesn't show the label.
genARMInstruction (IBinOp { iBinOp = op, iDest = Var dest, 
        iLeft  = Var left, iRight = Var right } )
  = case op of  
    BinOpAdd -> ["ADD r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ] 
    BinOpSub -> ["SUB r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ]
    BinOpMul -> ["MUL r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ]
    BinOpDiv -> [""]
    BinOpRem -> [""]
    BinOpGT  -> ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVGT r" ++ (show left) ++ ", #" ++ (show 1),
                 "MOVLE r" ++ (show left) ++ ", #" ++ (show 0)]




