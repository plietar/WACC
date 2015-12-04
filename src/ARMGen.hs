module ARMGen where

import Common.AST
import CodeGenTypes
import Data.Monoid
import Control.Monad.RWS
import Data.Set (Set)
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
              | ThrowRuntimeError
              | ThrowOverflowError
              | FreePair
              deriving (Show, Eq, Ord, Enum)

allFeatures = [CheckDivideByZero .. FreePair]

f = mconcat (map genFeature allFeatures)
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
      BinOpDiv -> do emit ["MOV r0, r" ++ (show left),
                        "MOV r1, r" ++ (show right),
                        "BL p_check_divide_by_zero",
                        "BL __aeabi_idiv"]
                     emitFeature CheckDivideByZero
      BinOpRem -> do emit ["MOV r0, r" ++ (show left),
                        "MOV r1, r" ++ (show right),
                        "BL p_check_divide_by_zero",
                        "BL __aeabi_idivmod"]
                     emitFeature CheckDivideByZero
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
      UnOpNeg -> emit ["RSB r" ++ (show dest) ++ ", r" ++ (show value) ++ ", #0"]
      UnOpLen -> emit ["LDR r" ++ (show dest) ++ ", [r" ++ show value ++ "]"]

genARMInstruction IMove { iDest = Var dest, iValue = Var value }
  = emit ["MOV r" ++ show dest ++ ", r" ++ show value]

--Jumps
genARMInstruction (ICondJump { iLabel = label, iValue = Var value})
  = emit ["CMP r" ++ (show value) ++ ", #0",
          "BNE " ++ (show label)]
genARMInstruction (IJump {iLabel = label} )
  = emit ["B " ++ (show label)]

--Call
genARMInstruction (ICall { iLabel = label, iArgs = args, iDest = dest }) = do
  forM args $ \(ty, Var arg) -> do
    emit [strInstr ty ++ " r" ++ show arg ++ ", [sp, #" ++ show (typeSize ty) ++ "]!"]
  emit ["BL " ++ show label ]
  unless (null args) (emit ["ADD sp, sp, #" ++ show (sum (map (typeSize . fst) args))])
  emit [ "MOV r" ++ show dest ++ ", r0" ]

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
genARMInstruction (IFrameRead {iOffset = offset, iDest = Var dest, iType = ty} )
  = emit [ldrInstr ty ++ " r" ++ (show dest) ++ ", [sp, #" ++ (show offset) ++ "]"]
genARMInstruction (IFrameWrite {iOffset = offset, iValue = Var value, iType = ty} )
  = emit [strInstr ty ++ " r" ++ (show value) ++ ", [sp, #" ++ (show offset) ++ "]"]

-- Array
genARMInstruction (IArrayAllocate { iDest = Var dest, iSize = size })
  = emit [ "LDR r0, =" ++ show size
         , "BL malloc"
         , "MOV r" ++ show dest ++ ", r0"]

-- Heap Read (i.e Pairs and Arrays)
genARMInstruction (IHeapRead { iHeapVar = Var heapVar, iDest = Var dest, iOperand = OperandVar (Var offset) shift, iType = ty })
  = emit [ldrInstr ty ++ " r" ++ show dest ++ ", [r" ++ show heapVar ++ ", r" ++ show offset ++ scaling ++ "]"]
    where scaling = if shift /= 0 then ", lsl #" ++ show shift else ""
genARMInstruction (IHeapRead { iHeapVar = Var heapVar, iDest = Var dest, iOperand = OperandLit offset, iType = ty })
  = emit [ldrInstr ty ++ " r" ++ show dest ++ ", [r" ++ show heapVar ++ ", #" ++ show offset ++ "]"]


-- Heap Write (i.e Pairs and Arrays)
genARMInstruction (IHeapWrite { iHeapVar = Var heapVar, iValue = Var value, iOperand = OperandVar (Var offset) shift, iType = ty })
  = emit [strInstr ty ++ " r" ++ show value ++ ", [r" ++ show heapVar ++ ", r" ++ show offset ++ scaling ++ "]"] 
    where scaling = if shift /= 0 then ", lsl #" ++ show shift else ""
genARMInstruction (IHeapWrite { iHeapVar = Var heapVar, iValue = Var value, iOperand = OperandLit offset, iType = ty })
  = emit [strInstr ty ++ " r" ++ show value ++ ", [r" ++ show heapVar ++ ", #" ++ show offset ++ "]"] 
 

--Pair
genARMInstruction (IPairAllocate { iDest = Var dest })
  = emit [ "MOV r0, #8"
         , "BL malloc"
         , "MOV r" ++ show dest ++ ", r0"]

genARMInstruction (INullCheck { iValue = Var value })
  = do emit [ "MOV r0, r" ++ show value
         , "BL p_check_null_pointer" ]
       emitFeature CheckNullPointer
genARMInstruction (IBoundsCheck { iArray = Var array, iIndex = Var index })
  = do emit [ "MOV r0, r" ++ show array
         , "MOV r1, r" ++ show index
         , "BL p_check_array_bounds" ]
       emitFeature CheckArrayBounds

-- Print
genARMInstruction (IPrint { iValue = Var value, iType = t, iNewline = newline }) = do
  emit [ "MOV r0, r" ++ show value]
  case t of
    TyInt -> do emit ["BL p_print_int"]
                emitFeature PrintInt
    TyBool -> do emit ["BL p_print_bool"]
                 emitFeature PrintBool
    TyChar -> do emit ["BL putchar"]
    TyArray TyChar -> do emit ["BL p_print_string"]
                         emitFeature PrintString
    _ -> do emit ["BL p_print_reference"]
            emitFeature PrintReference
  when newline (do emit ["BL p_print_ln"]
                   emitFeature PrintLine)
 
-- Read
genARMInstruction (IRead { iDest = Var dest, iType = t})
  = case t of
      TyInt -> emit ["BL p_read_int"]
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

mergeFeatures :: Set Feature -> ([String], [String])
mergeFeatures features
  = foldl f ([],[]) (Set.elems (mergeDependencies features)) 
  where
    f (a, b) feature = (a ++ (fst res), b ++ (snd res))
      where
        res = genFeature feature

mergeDependencies:: Set Feature -> Set Feature
mergeDependencies features = Set.unions $ Set.elems (Set.map dependantOn features)

dependantOn :: Feature -> Set Feature
dependantOn CheckArrayBounds   = Set.fromList [CheckArrayBounds,  ThrowRuntimeError, PrintString]
dependantOn CheckDivideByZero  = Set.fromList [CheckDivideByZero, ThrowRuntimeError, PrintString]
dependantOn CheckNullPointer   = Set.fromList [CheckNullPointer,  ThrowRuntimeError, PrintString]
dependantOn ThrowOverflowError = Set.fromList [ThrowOverflowError,ThrowRuntimeError, PrintString]
dependantOn FreePair           = Set.fromList [FreePair, ThrowRuntimeError, PrintString]
dependantOn ThrowRuntimeError  = Set.fromList [ThrowRuntimeError, PrintString]
dependantOn feature            = Set.fromList [feature]

genFeature :: Feature -> ([String], [String])
genFeature CheckDivideByZero = (["msg_p_check_divide_by_zero:", 
                                 ".word 45",
                                 ".ascii \"DivideByZeroError: divide or modulo by zero\\n\\0\""] 
                               ,["p_check_divide_by_zero:",
                                 "PUSH {lr}",
                                 "CMP r1, #0",
                                 "LDREQ r0, =msg_check_p_divide_by_zero",
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
                         ".ascii \"%.d\\0\""] 
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
                           ".ascii \"%.*s\0"] 
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
                              ".ascii \"%p\0\""] 
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
                       ,["p_print_ln",
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
                         "MOV r1, r0",
                         "LDR r0, =msg_p_read_int",
                         "ADD r0, r0, #4",
                         "BL scanf",
                         "POP {pc}"])


genFeature ReadChar = (["msg_p_read_char:", 
                         ".word 4",
                         ".ascii \" %c\\0\""] 
                       ,["p_read_char:",
                         "PUSH {lr}",
                         "MOV r1, r0",
                         "LDR r0, =msg_p_read_char",
                         "BL puts",
                         "MOV r0, #0",
                         "BL fflush",
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

--Calls another feature: p_throw_runtime_error.
genFeature FreePair = (["msg_p_free_pair:",
                        ".word 50",
                        ".ascii \"NullReferenceError: derefence a null reference\\n\\0\""]
                      ,["p_free_pair:",
                        "PUSH {lr}",
                        "CMP r0, #0",
                        "LDREQ r0, =msg_p_free_pair",
                        "BEQ p_throw_runtime_error",
                        "PUSH {r0}",
                        "LDR r0, [r0]",
                        "BL free",
                        "LDR r0, [sp]",
                        "LDR r0, [r0, #4]",
                        "BL free",
                        "POP {r0}",
                        "BL free",
                        "POP {pc}"])

strInstr :: Type -> String
strInstr TyBool = "STRB"
strInstr TyChar = "STRB"
strInstr _      = "STR"

ldrInstr :: Type -> String
ldrInstr TyBool = "LDRB"
ldrInstr TyChar = "LDRSB"
ldrInstr _      = "LDR"

