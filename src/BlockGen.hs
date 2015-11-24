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
--  , outputProgram :: [Instructions]
--  }
--
--type RuntimeEnvironment a = StateT Environment 
 blockGeneration :: Block -> Map String Int
blockGeneration block
  = genTable block 0 Map.empty


data Instruction =
   SUB String
  | PUSH String
  | LDR String
  | STR String
  | BL String
  | ADD String
  | MOV String
  deriving (Show)

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


genBlock :: Block -> WACCResult [Instruction]
genBlock block = do
  blockCode <- fmap concat (mapM (genStatement variables allRegs) block)
  return (initFrame ++ blockCode ++ endFrame)
  where
    variables = genTable block 0 Map.empty
    initFrame = [PUSH "{LR}", SUB (Ref SP) (Ref SP) (ImmNum (4 * Map.size variables))]
    endFrame = [ADD (Ref SP) (Ref SP) (ImmNum (4 * Map.size variables)), POP "{PC}"]

genStatement :: Map String Int -> [Register] -> (Pos, Stmt) -> WACCResult [Instruction]

genStatement offsetTable regs (_, StmtVar t s rhs)
  = case Map.lookup s offsetTable of
      Nothing -> Error RuntimeError [""]
      Just offset -> OK result
    where
      -- Need to call generateAssignRHS instead of the -1
      result = [LDR (Ref (head regs)) (ImmNum (-1)), STR (Ref (head regs)) (Ind SP varPos)]
      -- I assume the lookup will succeed since the variable was just added to the table
      varPos = 4 * (Map.size offsetTable - fromJust (Map.lookup s offsetTable) - 1)
genStatement _ _ (_, stmt) = OK [BL "ignore line"]

