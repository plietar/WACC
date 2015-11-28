module GenARM where

import AST
import CodeGen
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.State


data ARMState = ARMState
  {
    literals :: [Int]
  }

data ARMWriter = ARMWriter
  { assembly :: [String] 
  , allocated :: [(String, String)]
  }

instance Monoid ARMWriter where
  mempty = ARMWriter [] []
  mappend (ARMWriter a b) (ARMWriter a' b') = ARMWriter (a ++ a') (b ++ b')

emit :: [String] -> WriterT ARMWriter (State ARMState) ()
emit xs = tell (ARMWriter xs [])

emitLiteral :: String -> WriterT ARMWriter (State ARMState) String 
emitLiteral s = do
  (l:ls) <- gets literals
  modify (\s -> s { literals = ls })
  return ("msg_ " ++ show l)

genARMInstruction :: IR -> WriterT ARMWriter (State ARMState) ()
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitInt n }) 
  = emit ["LDR r" ++ (show dest) ++ ", =" ++ (show n)]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool True } ) 
  = emit ["MOV r" ++ (show dest) ++ ", #1"]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool False } ) 
  = emit ["MOV r" ++ (show dest) ++ ", #0"]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitChar chr } ) 
  = emit ["MOV r" ++ (show dest) ++ ", #" ++ (show chr)]
-- Not right, need a reference of the number of strings in the scope.
-- For the moment I'll just put msg_0.
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitString str  } ) = do
  message <- emitLiteral str 
  emit ["LDR r" ++ (show dest) ++ ", =" ++ message]

--BinOp
genARMInstruction (IBinOp { iBinOp = op, iDest = Var dest, 
        iLeft  = Var left, iRight = Var right } )
  = case op of  
      BinOpAdd -> emit ["ADD r" ++ (show dest) ++ ", r" ++ 
                        (show left) ++ ", r" ++ (show right) ] 
      BinOpSub -> emit ["SUB r" ++ (show dest) ++ ", r" ++ 
                        (show left) ++ ", r" ++ (show right) ]
      BinOpMul -> emit ["MUL r" ++ (show dest) ++ ", r" ++ 
                        (show left) ++ ", r" ++ (show right) ]
      BinOpDiv -> emit ["MOV r0, r" ++ (show left),
                        "MOV r1, r" ++ (show right),
                        "BL p_check_divide_by_zero",
                        "BL __aeabi_idiv"]
      BinOpRem -> emit ["MOV r0, r" ++ (show left),
                        "MOV r1, r" ++ (show right),
                        "BL p_check_divide_by_zero",
                        "BL __aeabi_idivmod"]
      BinOpGT  -> emit ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                        "MOVGT r" ++ (show dest) ++ ", #1",
                        "MOVLE r" ++ (show dest) ++ ", #0"]
      BinOpGE  -> emit ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                       "MOVGE r" ++ (show dest) ++ ", #1",
                       "MOVLT r" ++ (show dest) ++ ", #0"]
      BinOpLT  -> emit ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                        "MOVLT r" ++ (show dest) ++ ", #1",
                        "MOVGE r" ++ (show dest) ++ ", #0"]
      BinOpLE  -> emit ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                        "MOVLE r" ++ (show dest) ++ ", #1",
                        "MOVGT r" ++ (show dest) ++ ", #0"]
      BinOpEQ  -> emit ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                        "MOVEQ r" ++ (show dest) ++ ", #1",
                        "MOVNE r" ++ (show dest) ++ ", #0"]
      BinOpNE  -> emit ["CMP r" ++ (show left) ++ ", r" ++ (show right),
                        "MOVNE r" ++ (show dest) ++ ", #1",
                        "MOVEQ r" ++ (show dest) ++ ", #0"]
      BinOpAnd -> emit ["AND r" ++ (show dest) ++ ", r" ++ 
                        (show left) ++ ", r" ++ (show right) ]
      BinOpOr  -> emit ["OR r" ++ (show dest) ++ ", r" ++ 
                        (show left) ++ ", r" ++ (show right) ] 

--UnOp
genARMInstruction IUnOp { iUnOp = op, iDest = Var dest, 
        iValue = Var value } 
  = case op of
      UnOpNot -> emit ["EOR r" ++ (show dest) ++ ", r" ++ (show value) ++ ", #1"]
      UnOpNeg -> emit ["RSBS r" ++ (show dest) ++ ", r" ++ (show value) ++ ", #0"]
      UnOpLen -> emit ["LDR r" ++ (show dest) ++ ", =msg_0",
                       "LDR r" ++ (show value) ++ ", =msg_0"]
      UnOpOrd -> emit [""]
      UnOpChr -> emit [""]

--Jumps
genARMInstruction (ICondJump { iLabel = label, iValue = value}) 
  = emit ["CMP r" ++ (show value) ++ ", #0",
          "BNE" ++ (show label)]
genARMInstruction (IJump {iLabel = label} ) 
  = emit ["B " ++ (show label)]

--Call
genARMInstruction (ICall { iLabel = label, iArgs = vars, iDest = dest })
  = undefined

--Labels
genARMInstruction (ILabel { iLabel = NamedLabel label} )
  = emit [label ++ ":"] 
genARMInstruction (ILabel { iLabel = UnnamedLabel n }) 
  = emit ["L" ++ (show n) ++ ":"]

--Frame
genARMInstruction (IFrameAllocate { iSize = size })
  = emit ["SUB sp, sp, #" ++ (show size)]
genARMInstruction (IFrameFree { iSize = size } )
  = emit ["ADD sp, sp, #" ++ (show size)]
genARMInstruction (IFrameRead {iOffset = offset, iDest = Var dest} ) 
  = emit ["LDR r" ++ (show dest) ++ ", [sp, #" ++ (show offset) ++ "]"]
genARMInstruction (IFrameWrite {iOffset = offset, iValue = Var value} )
  = emit ["STR r" ++ (show value) ++ ", [sp, #" ++ (show offset) ++ "]"]

-- Array
genARMInstruction (IArrayAllocate { iDest = Var dest, iSize = size })
  = emit ["LDR r" ++ (show dest) ++ ", =" ++ show (size)]
genARMInstruction (IArrayRead { iArray = Var array, iIndex = Var index, iDest = Var dest })
  = emit ["LDR r" ++ (show arr) ++ ", [" ++ (show index) ++ "]\n MOV r" ++ (show dest) ++ " r" ++ (show array)]
genARMInstruction (IArrayWrite { iArray = Var array, iIndex = Var index, iValue = Var value })
  = emit ["STR r" ++ (show value) ++ ", [r" ++ (show array) ++ ", #" == (show index) ++ "]"]
genARMInstruction (IArrayLength { iArray = Var array, iDest = Var dest })
  = emit [""]







