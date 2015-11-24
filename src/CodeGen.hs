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

  | INewFrame { iSize :: Int }
  | IFreeFrame { iSize :: Int }
  | IReadFrame { iOffset :: Int, iDest :: Var }
  | IWriteFrame { iOffset :: Int, iValue :: Var }

  | INewArray { iDest :: Var, iSize :: Int }
  | IReadArray { iArray :: Var, iIndex :: Var, iDest :: Var }
  | IWriteArray { iArray :: Var, iIndex :: Var, iValue :: Var }
  | IArrayLength { iArray :: Var, iDest :: Var }

  | INewPair { iDest :: Var }
  | IReadPair { iPair :: Var, iDest :: Var, iSide :: PairSide }
  | IWritePair { iPair :: Var, iValue :: Var, iSide :: PairSide }

  | INullCheck { iValue :: Var }
  | IBoundsCheck { iArray :: Var, iIndex :: Var }

  | IPrint { iValue :: Var, iType :: Type, iNewline :: Bool }
  | IRead { iDest :: Var, iType :: Type }
  | IFree { iValue :: Var, iType :: Type }
  | IExit { iValue :: Var }

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

