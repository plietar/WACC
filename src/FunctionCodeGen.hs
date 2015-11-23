transFunction :: FuncDef -> [Instruction]
transFunction returnType fname [] body 
  = transExp body [registerNotInUse]

transFunction returnType fname [(paramType, paramName)] body
  = saveFun fname ++ [PUSH {lr}] ++ caller [(paramType, paramName)] ++ [POP {pc}] ++ [POP {pc}]

saveFun fname 
  = 

caller :: [(Type, String)] -> [Instruction]
caller params 
  = [SUB sp, sp, bytes] ++ [BL fname] ++ [PUSH returnValue]
  where 
  	bytes = ??

caller fname 
  = [BL fname] ++ [POP {pc}]

callee :: 
  = [PUSH fp] ++ [MOV sp ] ++

lr = r14
pc = r15 

-- Caller
-- pop param into register for param pass
-- call function
-- push return value onto stack

-- Callee
-- push old FP
-- make space for frame -- push
-- copy sp to fp
-- populate frame 
-- execute body
-- may push return value
-- clear frame space
-- pop fp
-- return

