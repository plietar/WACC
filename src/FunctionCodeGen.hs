module FunctionCodeGen where

import Common.AST
import CodeGen 
import StmtGen

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map as Map

genFunction :: Annotated FuncDef TypeA -> [IR]
genFunction (_, FuncDef _ fname params body) 
  = execWriter (runReaderT (evalStateT generation initialState) topFrame) 
    where 
      topFrame = Frame { offsets = p
                       , parent = Nothing
                       , allocated = False
                       , frameSize = length params + 1}
      p = saveParam params 1 Map.empty
      initialState = CodeGenState {
        variables = Prelude.map Var [0..],
        labels = Prelude.map UnnamedLabel [0..]
      }
      generation = do 
        tell [ ILabel { iLabel = NamedLabel (show fname) }
             , IFunctionBegin ]
        genBlock body
        retVal <- allocateVar
        tell [ ILiteral { iDest = retVal, iLiteral = LitInt 0 }
             , IReturn { iValue = retVal } ]


saveParam :: [(Type, String)] -> Int -> Map String Int -> Map String Int
saveParam [] _ t = t
saveParam ((_, s) : rest) index table 
  = saveParam rest (index + 1) (Map.insert s index table)


