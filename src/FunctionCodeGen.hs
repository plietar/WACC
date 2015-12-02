module FunctionCodeGen where

import Common.AST
import CodeGenTypes
import CodeGen 

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Tuple(swap)

genFunction :: Annotated FuncDef TypeA -> [IR]
genFunction (_, FuncDef _ fname params body)
  = execWriter (runReaderT (evalStateT generation initialState) topFrame)
    where
      topFrame = setVariables (map swap params) 4 rootFrame
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

