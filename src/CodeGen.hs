module CodeGen where

import AST
import Common
import Control.Monad.Writer
import Control.Monad.State

data Var = Var Int
data Label = NamedLabel String | UnnamedLabel Int

data IR
  = ILiteral { iDest :: Var, iLiteral :: Literal }

  | IBinOp { iBinOp :: BinOp, iDest :: Var, iLeft :: Var, iRight :: Var }
  | IUnOp { iUnOp :: UnOp, iDest :: Var, iValue :: Var }

  | ICondJump { iLabel :: Label, iValue :: Var }
  | IJump { iLabel :: Label }
  | ICall { iLabel :: Label, iArgs :: [Var] }
  | ILabel { iLabel :: Label }

  | IFrameAllocate { iSize :: Int }
  | IFrameFree { iSize :: Int }
  | IFrameRead { iOffset :: Int, iDest :: Var }
  | IFrameWrite { iOffset :: Int, iValue :: Var }

  | IArrayAllocate { iDest :: Var, iSize :: Int }
  | IArrayRead { iArray :: Var, iIndex :: Var, iDest :: Var }
  | IArrayWrite { iArray :: Var, iIndex :: Var, iValue :: Var }
  | IArrayLength { iArray :: Var, iDest :: Var }

  | IPairAllocate { iDest :: Var }
  | IPairRead { iPair :: Var, iDest :: Var, iSide :: PairSide }
  | IPairWrite { iPair :: Var, iValue :: Var, iSide :: PairSide }

  | INullCheck { iValue :: Var }
  | IBoundsCheck { iArray :: Var, iIndex :: Var }

  | IPrint { iValue :: Var, iType :: Type, iNewline :: Bool }
  | IRead { iDest :: Var, iType :: Type }
  | IFree { iValue :: Var, iType :: Type }
  | IExit { iValue :: Var }

  | IFunctionBegin { }
  | IFunctionEnd { }

data CodeGenState = CodeGenState {
  variables :: [Var],
  labels :: [Label]
}

type CodeGen a = StateT CodeGenState (Writer [IR]) a
allocateVar :: CodeGen Var
allocateVar = do
  (v:vs) <- gets variables
  modify (\s -> s { variables = vs })
  return v

allocateLabel :: CodeGen Label
allocateLabel = do
  (l:ls) <- gets labels
  modify (\s -> s { labels = ls })
  return l

