module GenARM where

import AST
import CodeGen


genARMInstruction :: IR -> [String]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitInt n } ) 
  = ["LDR r" ++ (show dest) ++ ", =" ++ (show n)]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool True } ) 
  = ["MOV r" ++ (show dest) ++ ", #1"]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool False } ) 
  = ["MOV r" ++ (show dest) ++ ", #0"]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitChar chr } ) 
  = ["MOV r" ++ (show dest) ++ ", #" ++ (show chr)]
-- Not right, need a reference of the number of strings in the scope.
-- For the moment I'll just put msg_0.
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitString str  } ) 
  = ["LDR r" ++ (show dest) ++ ", =msg_0"]

--BinOp
--Missing divide and mod binops, ARM calls a label "__aeabi_idiv" 
--but doesn't show the label.
genARMInstruction (IBinOp { iBinOp = op, iDest = Var dest, 
        iLeft  = Var left, iRight = Var right } )
  = case op of  
      BinOpAdd -> ["ADD r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ] 
      BinOpSub -> ["SUB r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ]
      BinOpMul -> ["MUL r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ]
      BinOpDiv -> ["MOV r0, r" ++ (show left),
                   "MOV r1, r" ++ (show right),
                   "BL p_check_divide_by_zero",
                   "BL __aeabi_idiv"]
      BinOpRem -> ["MOV r0, r" ++ (show left),
                   "MOV r1, r" ++ (show right),
                   "BL p_check_divide_by_zero",
                   "BL __aeabi_idivmod"]
      BinOpGT  -> ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVGT r" ++ (show dest) ++ ", #1",
                 "MOVLE r" ++ (show dest) ++ ", #0"]
      BinOpGE  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVGE r" ++ (show dest) ++ ", #1",
                 "MOVLT r" ++ (show dest) ++ ", #0"]
      BinOpLT  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVLT r" ++ (show dest) ++ ", #1",
                 "MOVGE r" ++ (show dest) ++ ", #0"]
      BinOpLE  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVLE r" ++ (show dest) ++ ", #1",
                 "MOVGT r" ++ (show dest) ++ ", #0"]
      BinOpEQ  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVEQ r" ++ (show dest) ++ ", #1",
                 "MOVNE r" ++ (show dest) ++ ", #0"]
      BinOpNE  ->  ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                 "MOVNE r" ++ (show dest) ++ ", #1",
                 "MOVEQ r" ++ (show dest) ++ ", #0"]
      BinOpAnd ->  ["AND r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ]
      BinOpOr  ->  ["OR r" ++ (show dest) ++ ", r" ++ (show left) ++ ", r" ++ (show right) ] 

genARMInstruction (IUnOp { iUnOp = op, iDest = Var dest, 
        iValue = Var value } )
  = case op of
      UnOpNot -> ["EOR r" ++ (show dest) ++ ", r" ++ (show value) ++ ", #1"]
      UnOpNeg -> ["RSBS r" ++ (show dest) ++ ", r" ++ (show value) ++ ", #0"]
      UnOpLen -> ["LDR r" ++ (show dest) ++ ", =msg_0",
                  "LDR r" ++ (show value) ++ ", =msg_0"]
      UnOpOrd -> [""]
      UnOpChr -> [""]

genARMInstruction (ICondJump { iLabel = label, iValue = value}) 
  = ["CMP r" ++ (show value) ++ ", #0",
     "BNE" ++ (show label)]

genARMInstruction (IJump {iLabel = label} ) 
  = ["B " ++ (show label)]

