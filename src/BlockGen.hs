module BlockGen where
import Data.Map as Map
import AST 
import ARMTypes
import Common
import Data.Maybe
import AST


--data Environment = Environment
--  {
--    variableOffset :: Map String Int
--  }
--
--type CodeGenEnvironment a = StateT Environment WACCResult a

blockGeneration :: (Annotated Block TypeA) -> Map String Int
blockGeneration (_, Block stmts)
  = genTable stmts 0 Map.empty

-- Generate a table with variables and offsets from the stack pointer.
-- Note: to actually find offset do (map.size - 1 - (lookup var map))
genTable :: [(Annotated Stmt TypeA)] -> Int -> Map String Int -> Map String Int
genTable ((_, StmtVar _ s _) : rest) index table
  = genTable rest (index + 1) (Map.insert s index table)
genTable (_ : rest) index table
  = genTable rest index table
genTable [] _ t
  = t


genBlock :: (Annotated Block TypeA) -> WACCResult [Instruction]
genBlock (_, Block stmts) = do
  blockCode <- fmap concat (mapM (genStatement variables allRegs) stmts)
  return (initFrame ++ blockCode ++ endFrame)
  where
    variables = genTable stmts 0 Map.empty
    initFrame = [PUSH "{LR}", SUB (Ref SP) (Ref SP) (ImmNum (4 * Map.size variables))]
    endFrame = [ADD (Ref SP) (Ref SP) (ImmNum (4 * Map.size variables)), POP "{PC}"]

genStatement :: Map String Int -> [Register] 
    -> Annotated Stmt TypeA -> WACCResult [Instruction]
genStatement offsetTable regs (_, StmtVar t s rhs) = do
  rhsCode  <- genAssignRHS offsetTable regs rhs
  case Map.lookup s offsetTable of
    Nothing -> Error CodeGenError ["Code gen error"]
    Just offset -> OK (rhsCode ++ storeInReg)
    where
      -- I assume the lookup will succeed since the variable was just added to the table
      varPos = 4 * (Map.size offsetTable - fromJust (Map.lookup s offsetTable) - 1)
      storeInReg = [STR (Ref (head regs)) (Ind SP varPos)]
      -- result = [LDR (Ref (head regs)) (ImmNum (-1)), STR (Ref (head regs)) (Ind SP varPos)]
genStatement _ _ (_, stmt) = OK [BL "ignore line"]


genAssignRHS :: Map String Int -> [Register] -> Annotated AssignRHS TypeA -> WACCResult [Instruction]
genAssignRHS offsetTable regs (_, RHSExpr e) =
  case e of
    (_, ExprLit literal) -> genExprLit offsetTable regs literal

genAssignRHS offsetTable regs (_, RHSArrayLit exps)     = OK []
genAssignRHS offsetTable regs (_, RHSNewPair e1 e2)     = OK []
genAssignRHS offsetTable regs (_, RHSPairElem pairElem) = OK []
genAssignRHS offsetTable regs (_, RHSCall ident exps)   = OK []

genExprLit :: Map String Int -> [Register] -> Annotated Literal TypeA -> WACCResult [Instruction]
genExprLit offsetTable regs (_, LitInt x)
  = OK [LDR (Ref (head regs)) (ImmNum (fromInteger x))]
genExprLit _ _ _
  = OK []

