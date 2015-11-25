module ARMTypes where

data Register = R1 | R2 | R3 | R4 | R5 | R6| R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 
  deriving (Eq, Show)

sp = R13
lr = R14
pc = R15
allRegs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15 ]

data Instruction = Define String
                 | SUB Operand Operand Operand
                 | LDR Operand Operand
                 | STR Operand Operand
                 | BL String
                 | ADD Operand Operand Operand
                 | MUL Operand Operand
                 | MOV Operand Operand
                 | PUSH String
                 | POP String
                 | COMM String

data Operand = Ref Register
             | ImmNum Int
             | ImmName String
             | Ind Register Int -- [SP + 4] i.e offset

instance Show Operand where
  show (Ref R13) = "sp"
  show (Ref R14) = "lr"
  show (Ref R15) = "pc"
  show (Ref r) = show r
  show (ImmNum i) = "=" ++ show i
  show (ImmName s) = "#" ++ s
  show (Ind r offset) = "[" ++ show r ++", #" ++ show offset ++ "]"

instance Show Instruction where
  show (Define s) = s ++ ": "
  show (PUSH s) = "\tPUSH {" ++ s ++ "}"
  show (POP s)  = "\tPOP  {" ++ s ++ "}"
  show (MOV o1 o2) = "\tMOV  " ++ show o1 ++ ", " ++ show o2
  show (SUB o1 o2 o3) = "\tSUB  " ++ show o1 ++ ", " ++ show o2 ++ ", " ++ show o3
  show (ADD o1 o2 o3) = "\tADD  " ++ show o1 ++ ", " ++ show o2 ++ ", " ++ show o3
  show (MUL o1 o2) = "\tMUL  " ++ show o1 ++ ", " ++ show o2
  show (STR o1 o2) = "\tSTR  " ++ show o1 ++ ", " ++ show o2
  show (LDR o1 o2) = "\tLDR  " ++ show o1 ++ ", " ++ show o2
  show (BL s)      = "\tBL   " ++ s
  show (COMM name) = "." ++ show name
