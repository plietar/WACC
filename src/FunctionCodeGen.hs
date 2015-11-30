module FunctionCodeGen where

import AST
import ARMTypes
import CodeGen 
import StmtGen
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map as Map
import Control.Monad.State

genFunction :: Annotated FuncDef TypeA -> [IR]
genFunction (_, FuncDef returnType fname params body) 
  = execWriter (runReaderT (evalStateT generation initialState) topFrame) 
    where 
      topFrame = Frame { offsets = p, parent = Nothing, allocated = False, CodeGen.size = length params + 1}
      p = saveParam params 1 Map.empty
      initialState = CodeGenState {
        variables = Prelude.map Var [0..],
        labels = Prelude.map UnnamedLabel [0..]
      }
      generation = do 
        tell [ ILabel {iLabel = NamedLabel (show fname)}
             , IFunctionBegin ]
        genBlock body
        tell [ IFunctionEnd ]


saveParam :: [(Type, String)] -> Int -> Map String Int -> Map String Int
saveParam [] _ t = t
saveParam ((_, s) : rest) index table 
  = saveParam rest (index + 1) (Map.insert s index table)


