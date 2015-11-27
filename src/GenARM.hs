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
                 "MOVGT r" ++ (show dest) ++ ", #" ++ (show 1),
                 "MOVLE r" ++ (show dest) ++ ", #" ++ (show 0)]
    BinOpGE  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVGE r" ++ (show dest) ++ ", #" ++ (show 1),
                 "MOVLT r" ++ (show dest) ++ ", #" ++ (show 0)]
    BinOpLT  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVLT r" ++ (show dest) ++ ", #" ++ (show 1),
                 "MOVGE r" ++ (show dest) ++ ", #" ++ (show 0)]
    BinOpLE  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVLE r" ++ (show dest) ++ ", #" ++ (show 1),
                 "MOVGT r" ++ (show dest) ++ ", #" ++ (show 0)]
    BinOpEQ  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVEQ r" ++ (show dest) ++ ", #" ++ (show 1),
                 "MOVNE r" ++ (show dest) ++ ", #" ++ (show 0)]
    BinOpNE  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVNE r" ++ (show dest) ++ ", #" ++ (show 1),
                 "MOVEQ r" ++ (show dest) ++ ", #" ++ (show 0)]
    BinOpAnd ->  ["AND r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ]
    BinOpOr  ->  ["OR r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ] 












