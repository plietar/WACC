{-# LANGUAGE RecordWildCards #-}
module ARMGen where

import Common.AST
import CodeGenTypes
import Data.Monoid
import Control.Monad.RWS
import Data.Set (Set)
import Data.Char
import Data.List (intercalate)
import qualified Data.Set as Set
import Features

data ARMState = ARMState
  {
    stringLiteralLabels :: [String]
  }

data ARMWriter = ARMWriter
  { assembly :: [String]
  , stringLiterals :: [(String, String)]
  , features :: Set Feature
  }

type ARMGen = RWS () ARMWriter ARMState

instance Monoid ARMWriter where
  mempty = ARMWriter [] [] Set.empty
  mappend (ARMWriter a b c) (ARMWriter a' b' c') = ARMWriter (a ++ a') (b ++ b') (Set.union c c')

offsetLimitARM :: Int
offsetLimitARM = 1024

emit :: [String] -> ARMGen ()
emit xs = tell (ARMWriter xs [] Set.empty)

emitLiteral :: String -> ARMGen String
emitLiteral s = do
  (l:ls) <- gets stringLiteralLabels
  modify (\s -> s { stringLiteralLabels = ls })
  tell (ARMWriter [] [(l, s)] Set.empty)
  return l

textSegment :: ARMWriter -> [String]
textSegment w = ".text" : ".global main" : (assembly w)

dataSegment :: ARMWriter -> [String]
dataSegment w = ".data" : concatMap genLit (stringLiterals w)
  where genLit (label, value) = [ label ++ ":"
                                , ".word " ++ show (length value)
                                , ".ascii " ++ show value ]


emitFeature :: Feature -> ARMGen ()
emitFeature ft = tell (ARMWriter [] [] (Set.singleton ft))

genARM :: [IR] -> ARMWriter
genARM irs = snd $ execRWS (mapM genARMInstruction irs) () (ARMState (map (("msg_" ++) . show) [0..]))

genARMInstruction :: IR -> ARMGen ()
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitNull })
  = emit ["MOV " ++ show dest ++ ", #0"]
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitInt n })
  = emit ["LDR " ++ show dest ++ ", =" ++ (show n)]
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitBool True } )
  = emit ["MOV " ++ show dest ++ ", #1"]
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitBool False } )
  = emit ["MOV " ++ show dest ++ ", #0"]
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitChar chr } ) 
  = emit ["MOV " ++ show dest ++ ", #" ++ (show (ord chr))]
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitString str  } ) = do
  message <- emitLiteral str
  emit ["LDR " ++ show dest ++ ", =" ++ message]

--BinOp
genARMInstruction (IBinOp { iBinOp = op, iDest = dest,
        iLeft  = left, iRight = right } )
  = case op of  
      BinOpAdd -> do
                  emit [ "ADDS " ++ show dest ++
                         ", " ++ show left ++
                         ", " ++ show right
                       , "BLVS p_throw_overflow_error"]
                  emitFeature ThrowOverflowError 
      BinOpSub -> do
                  emit [ "SUBS " ++ show dest ++
                         ", " ++ show left ++
                         ", " ++ show right
                       , "BLVS p_throw_overflow_error"]
                  emitFeature ThrowOverflowError 
      BinOpGT  -> emit ["CMP " ++ show left ++ ", " ++ (show right),
                        "MOVGT " ++ show dest ++ ", #1",
                        "MOVLE " ++ show dest ++ ", #0"]
      BinOpGE  -> emit ["CMP " ++ show left ++ ", " ++ (show right),
                       "MOVGE " ++ show dest ++ ", #1",
                       "MOVLT " ++ show dest ++ ", #0"]
      BinOpLT  -> emit ["CMP " ++ show left ++ ", " ++ (show right),
                        "MOVLT " ++ show dest ++ ", #1",
                        "MOVGE " ++ show dest ++ ", #0"]
      BinOpLE  -> emit ["CMP " ++ show left ++ ", " ++ (show right),
                        "MOVLE " ++ show dest ++ ", #1",
                        "MOVGT " ++ show dest ++ ", #0"]
      BinOpEQ  -> emit ["CMP " ++ show left ++ ", " ++ (show right),
                        "MOVEQ " ++ show dest ++ ", #1",
                        "MOVNE " ++ show dest ++ ", #0"]
      BinOpNE  -> emit ["CMP " ++ show left ++ ", " ++ (show right),
                        "MOVNE " ++ show dest ++ ", #1",
                        "MOVEQ " ++ show dest ++ ", #0"]
      BinOpAnd -> emit ["AND " ++ show dest ++ ", " ++
                        (show left) ++ ", " ++ (show right) ]
      BinOpOr  -> emit ["ORR " ++ show dest ++ ", " ++
                        (show left) ++ ", " ++ (show right) ]

      -- Division and remainder are handled by CodeGen

genARMInstruction IMul{..} = do
  emit [ "SMULL " ++ show iLow ++
         ", " ++ show iHigh ++
         ", " ++ show iLeft ++
         ", " ++ show iRight
       , "CMP " ++ show iHigh ++ ", " ++ show iLow ++ ", ASR #31"
       , "BLNE p_throw_overflow_error" ]
  emitFeature ThrowOverflowError

--UnOp
genARMInstruction IUnOp { iUnOp = op, iDest = dest,
        iValue = value }
  = case op of
      UnOpNot -> emit ["EOR " ++ show dest ++ ", " ++ show value ++ ", #1"]
      UnOpNeg -> do
        emit ["RSB " ++ show dest ++ ", " ++ show value ++ ", #0"]
        emitFeature ThrowOverflowError 
      UnOpLen -> emit ["LDR " ++ show dest ++ ", [" ++ show value ++ "]"]

genARMInstruction IMove { iDest = dest, iValue = value }
  = emit ["MOV " ++ show dest ++ ", " ++ show value]

--Jumps
genARMInstruction (ICondJump { iLabel = label, iValue = value})
  = emit ["CMP " ++ show value ++ ", #0",
          "BNE " ++ (show label)]
genARMInstruction (IJump {iLabel = label} )
  = emit ["B " ++ (show label)]

--Call
genARMInstruction IPushArg {..}
  = emit ["PUSH " ++ show iValue ]

genARMInstruction IClearArgs {..}
  = genARMInstruction IFrameFree { iSize = iSize }

genARMInstruction (ICall { iLabel = label })
  = emit ["BL " ++ show label ]

--Labels
genARMInstruction (ILabel { iLabel = label} )
  = emit [show label ++ ":"]

--Frame
genARMInstruction (IFrameAllocate { iSize = 0 }) = return ()
genARMInstruction (IFrameAllocate { iSize = size }) = do
  if size <= offsetLimitARM 
  then 
    emit ["SUB sp, sp, #" ++ show size]
  else do
    emit ["SUB sp, sp, #" ++ show offsetLimitARM ]
    genARMInstruction (IFrameAllocate { iSize = size - offsetLimitARM })


genARMInstruction (IFrameFree { iSize = 0 }) = return ()
genARMInstruction (IFrameFree { iSize = size }) = do
  if size <= offsetLimitARM 
  then 
    emit ["ADD sp, sp, #" ++ show size]
  else do
    emit ["ADD sp, sp, #" ++ show offsetLimitARM ]
    genARMInstruction (IFrameFree { iSize = size - offsetLimitARM })

genARMInstruction (IFrameRead {iOffset = offset, iDest = dest, iType = ty} )
  = emit [ldrInstr ty ++ " " ++ show dest ++ ", [sp, #" ++ show offset ++ "]"]
genARMInstruction (IFrameWrite {iOffset = offset, iValue = value, iType = ty} )
  = emit [strInstr ty ++ " " ++ show value ++ ", [sp, #" ++ show offset ++ "]"]

-- Heap Read (i.e Pairs and Arrays)
genARMInstruction (IHeapRead { iHeapVar = heapVar, iDest = dest, iOperand = OperandVar offset shift, iType = ty })
  = emit [ldrInstr ty ++ " " ++ show dest ++ ", [" ++ show heapVar ++ ", " ++ show offset ++ scaling ++ "]"]
    where scaling = if shift /= 0 then ", lsl #" ++ show shift else ""
genARMInstruction (IHeapRead { iHeapVar = heapVar, iDest = dest, iOperand = OperandLit offset, iType = ty })
  = emit [ldrInstr ty ++ " " ++ show dest ++ ", [" ++ show heapVar ++ ", #" ++ show offset ++ "]"]


-- Heap Write (i.e Pairs and Arrays)
genARMInstruction (IHeapWrite { iHeapVar = heapVar, iValue = value, iOperand = OperandVar offset shift, iType = ty })
  = emit [strInstr ty ++ " " ++ show value ++ ", [" ++ show heapVar ++ ", " ++ show offset ++ scaling ++ "]"] 
    where scaling = if shift /= 0 then ", lsl #" ++ show shift else ""
genARMInstruction (IHeapWrite { iHeapVar = heapVar, iValue = value, iOperand = OperandLit offset, iType = ty })
  = emit [strInstr ty ++ " " ++ show value ++ ", [" ++ show heapVar ++ ", #" ++ show offset ++ "]"] 
 
-- Function
genARMInstruction IFunctionBegin {..}
  = unless (null iSavedRegs)
           (emit [ "PUSH {" ++ intercalate "," (map show iSavedRegs) ++ "}" ])

genARMInstruction IReturn{..} = do
  let popRegs = map (\x -> if x == lrReg then pcReg else x) iSavedRegs
  unless (null popRegs)
         (emit [ "POP {" ++ intercalate "," (map show popRegs) ++ "}" ])
  unless (elem lrReg iSavedRegs)
         (emit ["BX lr"])
  emit [ ".ltorg" ]

strInstr :: Type -> String
strInstr TyBool = "STRB"
strInstr TyChar = "STRB"
strInstr _      = "STR"

ldrInstr :: Type -> String
ldrInstr TyBool = "LDRB"
ldrInstr TyChar = "LDRSB"
ldrInstr _      = "LDR"

