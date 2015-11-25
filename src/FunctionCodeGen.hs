module FunctionCodeGen where

import AST
--import BlockGen
import ARMTypes
import CodeGen 
import Control.Monad.Reader
import Data.Map as Map
import Control.Frame

genFunction :: Annotated FuncDef TypeA -> [IR]
genFunction (_, FuncDef returnType fname params body) 
  = runReaderT topFrame $ do
	tell [ ILabel {iLabel = NamedLabel ("f_" ++ fname)}
		, IFunctionBegin ]
	genBlock body
	tell [IFunctionEnd]
	where 
	  topFrame = Frame { offsets = p, parent = Nothing, allocated = False, size = length params + 1 }
	  p = saveParam params 1 Map.empty


saveParam :: [(Type, String)] -> Int -> Map String Int -> Map String Int
saveParam [] _ t = t
saveParam [(_, s) : rest] index table 
  = saveParam rest (index + 1) (Map.insert s index table)


