module BlockGen where
import Data.Map as Map
import AST 


--data Environment = Environment
--  {
--    variableOffset :: Map String Int
--  , outputProgram :: [Instructions]
--  }
--
--type RuntimeEnvironment a = StateT Environment 
--data Instructions =
--  | SUB String
--  | PUSH String
--  | LDR String
--  | STR String
--  | BL String
--  | ADD String
--  | MOV String
--  deriving (Show)
  
blockGeneration :: Block -> Map String Int
blockGeneration block
  = genTable block 0 Map.empty


genTable :: Block -> Int -> Map String Int -> Map String Int
genTable ((_, StmtVar _ s _) : rest) index table
  = genTable rest (index + 1) (Map.insert s index table)
genTable (_ : rest) index table
  = genTable rest index table
genTable [] _ t
  = t


