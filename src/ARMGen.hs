module ARMGen where

import Common.AST
import CodeGenTypes
import Data.Monoid
import Control.Monad.RWS
import Data.Set (Set)
import Data.Char
import qualified Data.Set as Set

data Feature = CheckDivideByZero
              | CheckNullPointer
              | CheckArrayBounds
              | PrintInt
              | PrintBool
              | PrintString
              | PrintReference
              | PrintLine
              | ReadInt
              | ReadChar
              | Initialise
              | ThrowDoubleFreeError
              | ThrowRuntimeError
              | ThrowOverflowError
              deriving (Show, Eq, Ord, Enum)

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
      BinOpMul -> do
                  emit [ "SMULL " ++ show dest ++
                         ", r0" ++
                         ", " ++ show left ++ ", " ++ show right 
                       , "CMP r0, " ++ show dest ++ ", ASR #31"
                       , "BLNE p_throw_overflow_error" ]
                  emitFeature ThrowOverflowError
      BinOpDiv -> do emit ["MOV r0, " ++ show left,
                        "MOV r1, " ++ show right,
                        "BL p_check_divide_by_zero",
                        "BL __aeabi_idivmod",
                        "MOV " ++ show dest ++ ", r0"]
                     emitFeature CheckDivideByZero
      BinOpRem -> do emit ["MOV r0, " ++ (show left),
                        "MOV r1, " ++ (show right),
                        "BL p_check_divide_by_zero",
                        "BL __aeabi_idivmod",
                        "MOV " ++ show dest ++ ", r1"]
                     emitFeature CheckDivideByZero
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
genARMInstruction (IFunctionBegin { })
  = emit [ "PUSH {lr}" ]

genARMInstruction (IReturn)
  = emit [ "POP {pc}"
         , ".ltorg" ]

mergeFeatures :: Set Feature -> ([String], [String])
mergeFeatures features
  = foldl mappend ([],[]) (map genFeature $ Set.elems $ mergeDependencies features)

mergeDependencies:: Set Feature -> Set Feature
mergeDependencies features = Set.unions $ Set.elems (Set.map dependantOn features)

dependantOn :: Feature -> Set Feature
dependantOn CheckArrayBounds   = Set.fromList [CheckArrayBounds,  ThrowRuntimeError, PrintString]
dependantOn CheckDivideByZero  = Set.fromList [CheckDivideByZero, ThrowRuntimeError, PrintString]
dependantOn CheckNullPointer   = Set.fromList [CheckNullPointer,  ThrowRuntimeError, PrintString]
dependantOn ThrowOverflowError = Set.fromList [ThrowOverflowError,ThrowRuntimeError, PrintString]
dependantOn ThrowRuntimeError  = Set.fromList [ThrowRuntimeError, PrintString]
dependantOn ThrowDoubleFreeError = Set.fromList [ThrowDoubleFreeError, PrintString]
dependantOn Initialise         = Set.fromList [Initialise, ThrowDoubleFreeError]
dependantOn feature            = Set.fromList [feature]

genFeature :: Feature -> ([String], [String])
genFeature CheckDivideByZero = (["msg_p_check_divide_by_zero:", 
                                 ".word 45",
                                 ".ascii \"DivideByZeroError: divide or modulo by zero\\n\\0\""] 
                               ,["p_check_divide_by_zero:",
                                 "PUSH {lr}",
                                 "CMP r1, #0",
                                 "LDREQ r0, =msg_p_check_divide_by_zero",
                                 "BLEQ p_throw_runtime_error",
                                 "POP {pc}"])

genFeature CheckNullPointer = (["msg_p_check_null_pointer:", 
                                 ".word 50",
                                 ".ascii \"NullReferenceError: derefence a null reference \\n\\0\""] 
                               ,["p_check_null_pointer:",
                                 "PUSH {lr}",
                                 "CMP r0, #0",
                                 "LDREQ r0, =msg_p_check_null_pointer",
                                 "BLEQ p_throw_runtime_error",
                                 "POP {pc}"])

--Calls another feature: p_throw_runtime_error.
genFeature CheckArrayBounds =  (["msg_p_check_array_bounds_1:", 
                                 ".word 44",
                                 ".ascii \"ArrayIndexOutOfBoundsError: negative index\\0\"",
                                 "msg_p_check_array_bounds_2:", 
                                 ".word 45",
                                 ".ascii \"ArrayIndexOutOfBoundsError: index too large\\0\""]
                               ,["p_check_array_bounds:",
                                 "PUSH {lr}",
                                 "CMP r0, #0",
                                 "LDRLT r0, =msg_p_check_array_bounds_1",
                                 "BLLT p_throw_runtime_error",
                                 "LDR r1, [r1]",
                                 "CMP r0, r1",
                                 "LDRCS r0, =msg_p_check_array_bounds_2",
                                 "BLCS p_throw_runtime_error",
                                 "POP {pc}"])

genFeature PrintInt =  (["msg_p_print_int:", 
                         ".word 3",
                         ".ascii \"%d\\0\""] 
                       ,["p_print_int:",
                         "PUSH {lr}",
                         "MOV r1, r0",
                         "LDR r0, =msg_p_print_int",
                         "ADD r0, r0, #4",
                         "BL printf",
                         "MOV r0, #0",
                         "BL fflush",
                         "POP {pc}"])

genFeature PrintBool = (["msg_p_print_bool_1:", 
                         ".word 5",
                         ".ascii \"true\\0\"",
                         "msg_p_print_bool_2:",
                         ".word 6",
                         ".ascii \"false\\0\""] 
                       ,["p_print_bool:",
                         "PUSH {lr}",
                         "CMP r0, #0",
                         "LDRNE r0, =msg_p_print_bool_1",
                         "LDREQ r0, =msg_p_print_bool_2",
                         "ADD r0, r0, #4",
                         "BL printf",
                         "MOV r0, #0",
                         "BL fflush",
                         "POP {pc}"])

genFeature PrintString = (["msg_p_print_string:", 
                           ".word 5",
                           ".ascii \"%.*s\\0\""] 
                         ,["p_print_string:",
                           "PUSH {lr}",
                           "LDR r1, [r0]",
                           "ADD r2, r0, #4",
                           "LDR r0, =msg_p_print_string",
                           "ADD r0, r0, #4",
                           "BL printf",
                           "MOV r0, #0",
                           "BL fflush",
                           "POP {pc}"])

genFeature PrintReference = (["msg_p_print_reference:", 
                              ".word 3",
                              ".ascii \"%p\\0\""] 
                            ,["p_print_reference:",
                              "PUSH {lr}",
                              "MOV r1, r0",
                              "LDR r0, =msg_p_print_reference",
                              "ADD r0, r0, #4",
                              "BL printf",
                              "MOV r0, #0",
                              "BL fflush",
                              "POP {pc}"])

genFeature PrintLine = (["msg_p_print_ln:", 
                         ".word 1",
                         ".ascii \"\\0\""] 
                       ,["p_print_ln:",
                         "PUSH {lr}",
                         "LDR r0, =msg_p_print_ln",
                         "ADD r0, r0, #4",
                         "BL puts",
                         "MOV r0, #0",
                         "BL fflush",
                         "POP {pc}"])

genFeature ReadInt = (["msg_p_read_int:", 
                         ".word 3",
                         ".ascii \"%d\\0\""] 
                       ,["p_read_int:",
                         "PUSH {lr}",
                         "SUB sp, sp, #4",
                         "MOV r1, sp",
                         "LDR r0, =msg_p_read_int",
                         "ADD r0, r0, #4",
                         "BL scanf",
                         "LDR r0, [sp]",
                         "ADD sp, sp, #4",
                         "POP {pc}"])

genFeature ReadChar = (["msg_p_read_char:", 
                         ".word 4",
                         ".ascii \" %c\\0\""] 
                       ,["p_read_char:",
                         "PUSH {lr}",
                         "SUB sp, sp, #1",
                         "MOV r1, sp",
                         "LDR r0, =msg_p_read_char",
                         "ADD r0, r0, #4",
                         "BL scanf",
                         "LDRSB r0, [sp]",
                         "ADD sp, sp, #1",
                         "POP {pc}"])

--Calls another feature: p_print_string.
genFeature ThrowRuntimeError = ([""] 
                               ,["p_throw_runtime_error:",
                                 "BL p_print_string",
                                 "MOV r0, #-1",
                                 "BL exit"])

genFeature ThrowOverflowError =  (["msg_p_throw_overflow_error:",
                                   ".word 82",
                                   ".ascii \"OverflowError: the result is too small/large to store \
                                     \in a 4-byte signed-integer. \\n\""] 
                                 ,["p_throw_overflow_error:",
                                   "LDR r0, =msg_p_throw_overflow_error",
                                   "BL p_throw_runtime_error"])

genFeature ThrowDoubleFreeError = (["msg_p_throw_double_free:",
                                    ".word 34",
                                    ".ascii \"DoubleFreeError: pair freed twice\\n\""]
                                  ,["p_throw_double_free:",
                                    "PUSH {lr}",
                                    "LDR r0, =msg_p_throw_double_free",
                                    "BL p_print_string",
                                    "POP {pc}"])
genFeature Initialise = ([],
                         [ "PUSH {lr}"
                         , "MOV r0, #-5"
                         , "MOV r1, #2"
                         , "BL mallopt"
                         , "MOV r0, #6"
                         , "LDR r1, =p_throw_double_free"
                         , "BL signal"
                         , "POP {pc}"])


strInstr :: Type -> String
strInstr TyBool = "STRB"
strInstr TyChar = "STRB"
strInstr _      = "STR"

ldrInstr :: Type -> String
ldrInstr TyBool = "LDRB"
ldrInstr TyChar = "LDRSB"
ldrInstr _      = "LDR"

