module ARMGen where

import Common.AST
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
  return ("msg_" ++ show l)

genARM :: [IR] -> ARMWriter
genARM irs = evalState (execWriterT (mapM genARMInstruction irs)) (ARMState [0..])

genARMInstruction :: IR -> WriterT ARMWriter (State ARMState) ()
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitNull }) 
  = emit ["MOV r" ++ (show dest) ++ ", #0"]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitInt n }) 
  = emit ["LDR r" ++ (show dest) ++ ", =" ++ (show n)]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool True } ) 
  = emit ["MOV r" ++ (show dest) ++ ", #1"]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitBool False } ) 
  = emit ["MOV r" ++ (show dest) ++ ", #0"]
genARMInstruction (ILiteral { iDest = Var dest, iLiteral = LitChar chr } ) 
  = emit ["MOV r" ++ (show dest) ++ ", #" ++ (show chr)]
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
      UnOpLen -> emit ["LDR r" ++ (show dest) ++ ", [r" ++ show value]

--Jumps
genARMInstruction (ICondJump { iLabel = label, iValue = value}) 
  = emit ["CMP r" ++ (show value) ++ ", #0",
          "BNE " ++ (show label)]
genARMInstruction (IJump {iLabel = label} ) 
  = emit ["B " ++ (show label)]

--Call
genARMInstruction (ICall { iLabel = label, iArgs = vars, iDest = dest })
  = undefined

--Labels
genARMInstruction (ILabel { iLabel = label} )
  = emit [show label ++ ":"] 

--Frame
genARMInstruction (IFrameAllocate { iSize = 0 }) = return ()
genARMInstruction (IFrameAllocate { iSize = size })
  = emit ["SUB sp, sp, #" ++ show size]

genARMInstruction (IFrameFree { iSize = 0 }) = return ()
genARMInstruction (IFrameFree { iSize = size } )
  = emit ["ADD sp, sp, #" ++ show size]
genARMInstruction (IFrameRead {iOffset = offset, iDest = Var dest} ) 
  = emit ["LDR r" ++ (show dest) ++ ", [sp, #" ++ (show offset) ++ "]"]
genARMInstruction (IFrameWrite {iOffset = offset, iValue = Var value} )
  = emit ["STR r" ++ (show value) ++ ", [sp, #" ++ (show offset) ++ "]"]

-- Array
genARMInstruction (IArrayAllocate { iDest = Var dest, iSize = size })
  = emit [ "LDR r0, =" ++ show size
         , "BL malloc"
         , "MOV r" ++ show dest ++ ", r0"]
genARMInstruction (IArrayRead { iArray = Var array, iIndex = Var index, iDest = Var dest })
  = emit ["LDR r" ++ show dest ++ ", [r" ++ show array ++ ", r" ++ show index ++ "]"]
genARMInstruction (IArrayWrite { iArray = Var array, iIndex = Var index, iValue = Var value })
  = emit ["STR r" ++ show value ++ ", [r" ++ show array ++ ", r" ++ show index ++ "]"]

--Pair
genARMInstruction (IPairAllocate { iDest = Var dest })
  = emit [ "LDR r0, =8"
         , "BL malloc"
         , "MOV r" ++ show dest ++ ", r0"]
genARMInstruction (IPairRead { iPair = Var pair, iDest = Var dest, iSide = PairFst })
  = emit [ "LDR r" ++ show dest ++ ", [r" ++ show pair ++ "]" ]
genARMInstruction (IPairRead { iPair = Var pair, iDest = Var dest, iSide = PairSnd })
  = emit [ "LDR r" ++ show dest ++ ", [r" ++ show pair ++ ", #4]" ]
genARMInstruction (IPairWrite { iPair = Var pair, iValue = Var value, iSide = PairFst })
  = emit [ "STR r" ++ show value ++ ", [r" ++ show pair ++ "]" ]
genARMInstruction (IPairWrite { iPair = Var pair, iValue = Var value, iSide = PairSnd })
  = emit [ "STR r" ++ show value ++ ", [r" ++ show pair ++ ", #4]" ]


genARMInstruction (INullCheck { iValue = Var value })
  = emit [ "MOV r0, r" ++ show value
         , "BL p_check_null_pointer" ]
genARMInstruction (IBoundsCheck { iArray = Var array, iIndex = Var index })
  = emit [ "MOV r0, r" ++ show array
         , "MOV r1, r" ++ show index
         , "BL p_check_array_bounds" ]

-- Print
genARMInstruction (IPrint { iValue = Var value, iType = t, iNewline = newline }) = do
  emit [ "MOV r0, r" ++ show value]
  case t of
    TyInt -> emit ["BL p_print_int"]
    TyBool -> emit ["BL p_print_bool"]
    TyChar -> emit ["BL p_print_char"]
    TyArray TyChar -> emit ["BL p_print_string"]
    _ -> emit ["BL p_print_reference"]
  when newline (emit ["BL p_print_ln"])
 
-- Read
genARMInstruction (IRead { iDest = Var dest, iType = t})
  = case t of
      TyInt -> emit ["BL p_read_int"]
      TyBool -> emit ["BL p_read_bool"]
      TyChar -> emit ["BL p_read_char"]

-- Free
genARMInstruction (IFree { iValue = Var value, iType = t})
  = emit [ "MOV r0, r" ++ show value
         , "BL free" ]
  
-- Exit
genARMInstruction (IExit { iValue = Var value })
  = emit [ "MOV r0, r" ++ show value
         , "BL exit" ]

-- Function
genARMInstruction (IFunctionBegin { })
  = emit [ "PUSH {lr}" ]
genARMInstruction (IReturn { iValue = Var value })
  = emit [ "MOV r0, r" ++ show value
         , "POP {pc}" ]

