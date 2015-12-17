{-# LANGUAGE RecordWildCards #-}
module ARMGen where

import Arguments
import Common.Stuff
import Common.AST
import CodeGenTypes
import Data.Monoid
import Control.Monad.RWS
import Control.Applicative
import Data.Set (Set)
import Data.Char
import Data.List (intercalate, sort)
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

type ARMGen = (RWST () ARMWriter ARMState WACCArguments)

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

emitFeature :: Feature -> ARMGen ()
emitFeature ft = tell (ARMWriter [] [] (Set.singleton ft))

textSegment :: ARMWriter -> [String]
textSegment w = ".text" : (assembly w)

dataSegment :: ARMWriter -> [String]
dataSegment w = ".data" : concatMap genLit (stringLiterals w)
  where genLit (label, value) = [ label ++ ":"
                                , ".word " ++ show (length value)
                                , ".ascii " ++ show value ]

genARM :: [IR] -> WACCArguments ARMWriter
genARM irs = snd <$> execRWST generation () (ARMState (map (("msg_" ++) . show) [0..]))
  where
    generation :: ARMGen ()
    generation = do
      mapM genARMInstruction irs
      unlessM (lift $ getArgument runtimeEnabled) 
        (ifM (lift $ getArgument gcEnabled) 
                (emitFeature NoRuntimeGC)
                (emitFeature NoRuntime))

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
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitString str } ) = do
  message <- emitLiteral str
  emit ["LDR " ++ show dest ++ ", =" ++ message]
genARMInstruction (ILiteral { iDest = dest, iLiteral = LitLabel lab } ) = do
  emit ["LDR " ++ show dest ++ ", =" ++ show lab]
--BinOp
genARMInstruction IBinOp {..}
  = case iBinOp of
      BinOpAdd -> do
                  emit [ "ADDS " ++ show iDest ++
                         ", " ++ show iLeft ++
                         ", " ++ show iOperand
                       , "BLVS p_throw_overflow_error"]
                  emitFeature ThrowOverflowError
      BinOpSub -> do
                  emit [ "SUBS " ++ show iDest ++
                         ", " ++ show iLeft ++
                         ", " ++ show iOperand
                       , "BLVS p_throw_overflow_error"]
                  emitFeature ThrowOverflowError
      BinOpGT  -> emit ["CMP " ++ show iLeft ++ ", " ++ show iOperand,
                        "MOVGT " ++ show iDest ++ ", #1",
                        "MOVLE " ++ show iDest ++ ", #0"]
      BinOpGE  -> emit ["CMP " ++ show iLeft ++ ", " ++ show iOperand,
                       "MOVGE " ++ show iDest ++ ", #1",
                       "MOVLT " ++ show iDest ++ ", #0"]
      BinOpLT  -> emit ["CMP " ++ show iLeft ++ ", " ++ show iOperand,
                        "MOVLT " ++ show iDest ++ ", #1",
                        "MOVGE " ++ show iDest ++ ", #0"]
      BinOpLE  -> emit ["CMP " ++ show iLeft ++ ", " ++ show iOperand,
                        "MOVLE " ++ show iDest ++ ", #1",
                        "MOVGT " ++ show iDest ++ ", #0"]
      BinOpEQ  -> emit ["CMP " ++ show iLeft ++ ", " ++ show iOperand,
                        "MOVEQ " ++ show iDest ++ ", #1",
                        "MOVNE " ++ show iDest ++ ", #0"]
      BinOpNE  -> emit ["CMP " ++ show iLeft ++ ", " ++ show iOperand,
                        "MOVNE " ++ show iDest ++ ", #1",
                        "MOVEQ " ++ show iDest ++ ", #0"]
      BinOpAnd -> emit ["AND " ++ show iDest ++ ", " ++
                        (show iLeft) ++ ", " ++ show iOperand ]
      BinOpOr  -> emit ["ORR " ++ show iDest ++ ", " ++
                        (show iLeft) ++ ", " ++ show iOperand ]
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
        emit ["RSBS " ++ show dest ++ ", " ++ show value ++ ", #0"
             , "BLVS p_throw_overflow_error"]
        emitFeature ThrowOverflowError
      UnOpLen -> emit ["LDR " ++ show dest ++ ", [" ++ show value ++ "]"]

genARMInstruction IMove { iDest = dest, iValue = value }
  = emit ["MOV " ++ show dest ++ ", " ++ show value]

--Jumps
genARMInstruction ICompare{..}
  = emit ["CMP " ++ show iValue ++ ", " ++ show iOperand]

genARMInstruction ICondJump{..}
  = emit [ "B" ++ show iCondition ++ " " ++ show iLabel ]

genARMInstruction IJump{..}
  = emit [ "B " ++ show iLabel ]

--Call
genARMInstruction IPushArg {..}
  = emit ["PUSH {" ++ show iValue ++ "}"]

genARMInstruction IClearArgs {..}
  = genARMInstruction IFrameFree { iSize = iSize }

genARMInstruction (ICall { iLabel = label })
  = emit ["BL " ++ show label ]

--Labels
genARMInstruction (ILabel { iLabel = NamedLabel name} )
  = emit [".global " ++ name
         , name ++ ":"]
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
genARMInstruction IHeapRead {..}
  = emit [ldrInstr iType ++ " " ++ show iDest ++ ", [" ++ show iHeapVar ++ ", " ++ show iOperand ++ "]"]

-- Heap Write (i.e Pairs and Arrays)
genARMInstruction IHeapWrite {..}
  = emit [strInstr iType ++ " " ++ show iValue ++ ", [" ++ show iHeapVar ++ ", " ++ show iOperand ++ "]"]

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

genARMInstruction IJumpReg{..}
  = emit [ "BX " ++ show iValue ]

genARMInstruction IYield{..} = do
  let popRegs = map (\x -> if x == lrReg then pcReg else x) iSavedRegs

  unless (null iSavedContext) $
      emit [ "ADD " ++ show iValue ++ ", " ++ show iValue ++ ", #4"
           , "STMIA " ++ show iValue ++ ", {" ++ intercalate "," (map show (sort iSavedContext)) ++ "}"
           , "SUB " ++ show iValue ++ ", " ++ show iValue ++ ", #4" ]

  unless (null popRegs)
         (emit [ "POP {" ++ intercalate "," (map show popRegs) ++ "}" ])
  unless (elem lrReg iSavedRegs)
         (emit ["BX lr"])

genARMInstruction IRestore{..} = do
  unless (null iSavedContext) $
      emit [ "ADD " ++ show iValue ++ ", " ++ show iValue ++ ", #4"
           , "LDMIA " ++ show iValue ++ ", {" ++ intercalate "," (map show (sort iSavedContext)) ++ "}"
           , "SUB " ++ show iValue ++ ", " ++ show iValue ++ ", #4" ]

strInstr :: Type -> String
strInstr TyBool = "STRB"
strInstr TyChar = "STRB"
strInstr _      = "STR"

ldrInstr :: Type -> String
ldrInstr TyBool = "LDRB"
ldrInstr TyChar = "LDRSB"
ldrInstr _      = "LDR"

