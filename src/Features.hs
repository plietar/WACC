module Features where

import Data.Monoid
import Data.Set (Set)
import qualified Data.Map as Map
import Common.AST
import Data.List (intercalate)
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
              | ThrowDoubleFreeError
              | ThrowRuntimeError
              | ThrowOverflowError
              | NoRuntime
              | NoRuntimeGC
              | StructVTable String [Int]
              | GCTypeInformation Type
              deriving (Show, Eq, Ord)

genFeatures :: Set Feature -> ([String], [String])
genFeatures features
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
dependantOn NoRuntime          = Set.fromList [NoRuntime, ThrowDoubleFreeError, PrintString]
dependantOn NoRuntimeGC          = Set.fromList [NoRuntimeGC, ThrowDoubleFreeError, PrintString]
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
genFeature NoRuntime = ([],
                         [ ".global main"
                         , "main:"
                         , "PUSH {lr}"
                         , "MOV r0, #-5"
                         , "MOV r1, #2"
                         , "BL mallopt"
                         , "MOV r0, #6"
                         , "LDR r1, =p_throw_double_free"
                         , "BL signal"
                         , "BL wacc_main" 
                         , "POP {pc}" ])

genFeature NoRuntimeGC = ([],
                         [ ".global main"
                         , "main:"
                         , "PUSH {lr}"
                         , "BL GCInit"
                         , "MOV r0, #-5"
                         , "MOV r1, #2"
                         , "BL mallopt"
                         , "MOV r0, #6"
                         , "LDR r1, =p_throw_double_free"
                         , "BL signal"
                         , "BL wacc_main"
                         , "BL GCFree"
                         , "POP {pc}" ])

genFeature (StructVTable lbl offsets)
 = ([ ".global " ++ lbl
    , lbl ++ ":" ] ++
    map (\x -> ".word " ++ show x) offsets, [])

genFeature (GCTypeInformation ty) = (typeInfo ty ++ typeDescription ty, [])

mangleVTableName :: [String] -> String
mangleVTableName vars
 = "_vtable_" ++ intercalate "_" vars

-- Garbage Collection Methods
typeInfo :: Type -> [String]
typeInfo baseTy@(TyArray elemTy)
  = [ "_ty_info_" ++ mangleTypeName baseTy ++ ":"
    , ".byte 1"   -- isArray
    , ".word _ty_string_" ++ mangleTypeName baseTy
    , ".word 0"   -- length (unnecessary for arrays)
    , ".byte " ++ isPointer elemTy ] -- isPointer

typeInfo baseTy@(TyTuple elemTys)
  = [ "_ty_info_" ++ mangleTypeName baseTy ++ ":"
    , ".byte 0"
    , ".word _ty_string_" ++ mangleTypeName baseTy
    , ".word " ++ show (length elemTys) ] ++
    map ((".byte " ++) . isPointer) elemTys

typeInfo baseTy@(TyStruct members)
  = [ "_ty_info_" ++ mangleTypeName baseTy ++ ":"
    , ".byte 0"
    , ".word _ty_string_" ++ mangleTypeName baseTy
    , ".word " ++ show (Map.size members)] ++
    map ((".byte " ++) . isPointer) (Map.elems members) 

typeDescription :: Type -> [String]
typeDescription ty
  = [ "_ty_string_" ++ mangleTypeName ty ++ ":"
    , ".ascii \"" ++ show ty ++ "\\0\"" ]

isPointer :: Type -> String
isPointer (TyArray _) = "1"
isPointer (TyTuple _) = "1"
isPointer TyNull = "1"
isPointer _ = "0"

-- Mangle a name from its types
mangleTypeName :: Type -> String
mangleTypeName TyInt = "i"
mangleTypeName TyChar = "c"
mangleTypeName (TyArray t) = "A" ++ mangleTypeName t
mangleTypeName (TyTuple ts) = "T" ++ show (length ts) ++ concatMap mangleTypeName ts
mangleTypeName TyBool = "b"
mangleTypeName TyVoid = "v"
mangleTypeName TyNull = "n"
mangleTypeName TyAny = "a"
mangleTypeName (TyFFI name) = "F" ++ show (length name) ++ name
mangleTypeName (TyStruct members)
  = "S" ++ show (Map.size members)
        ++ concatMap (\(name, ty) -> "s" ++ show (length name) ++ name ++ mangleTypeName ty) (Map.assocs members)

