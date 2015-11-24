module FunctionTypes where

data Register = r1 | r2 | r3 | r4 | r5 | r6| r7 | r8 | r9 | r10 |r11 | r12 | r13 | r14 | r15 
  deriving (Eq, Show)

{lr} = r14
{pc} = r15
allRegs = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15 ]
  
data Instructions = Define String
  | SUB Operand Operand
  | LDR Operand Operand
  | STR Operand Operand
  | BL String
  | ADD Operand Operand
  | MOV Operand Operand
  | PUSH String
  | POP String
  | COMM String
  deriving Show

data Operand = Ref Register
  |ImmNum Int 
  |ImmName String
  |Abs String
  |Ind Register
  deriving Show

instance Show Operand where
  show (Reg r) = show r
  show (ImmNum i) = "#" ++ show i

instance Show Instr where
  show (Define s) = s ++ ": "
  show (Push s) = "\tPUSH " ++ s 
  show (Pop s)  = "\tPOP  " ++ s
  show (Mov o1 o2) = "\tMOV  " ++ show o1 ++ ", " ++ show o2
  show (Sub o1 o2) = "\tSUB  " ++ show o1 ++ ", " ++ show o2
  show (Add o1 o2) = "\tADD  " ++ show o1 ++ ", " ++ show o2
  show (Mul o1 o2) = "\tMUL  " ++ show o1 ++ ", " ++ show o2
  show (Str o1 o2) = "\tSTR  " ++ show o1 ++ ", " ++ show o2
  show (Ldr o1 o2) = "\tLDR  " ++ show o1 ++ ", " ++ show o2
  --show (Bra s) = "\tbra  " ++ s
  --show (Blt s) = "\tblt  " ++ s
  --show (Bgt s) = "\tbgt  " ++ s
  --show (Bge s) = "\tbge  " ++ s
  --show (Halt)  = "\thalt "
  show (Comm name) = "." ++ show name 