{-# LANGUAGE RecordWildCards #-}

module CodeGen where

import Common.AST

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Data.Map as Map
import Data.Maybe


data Var = Var Int
  deriving (Show, Ord, Eq)
data Label = NamedLabel String | UnnamedLabel Int
  deriving (Show, Ord, Eq)

data IR
  = ILiteral { iDest :: Var, iLiteral :: Literal }

  | IBinOp { iBinOp :: BinOp, iDest :: Var, iLeft :: Var, iRight :: Var }
  | IUnOp { iUnOp :: UnOp, iDest :: Var, iValue :: Var }

  | ICondJump { iLabel :: Label, iValue :: Var }
  | IJump { iLabel :: Label }
  | ICall { iLabel :: Label, iArgs :: [Var], iDest :: Var }
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
  | IReturn { iValue :: Var }
  deriving Show

data CodeGenState = CodeGenState {
  variables :: [Var],
  labels :: [Label]
}

type CodeGen a = StateT CodeGenState (ReaderT Frame (Writer [IR])) a
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


data Frame = Frame
  { offsets   :: Map String Int
  , parent    :: Maybe Frame
  , allocated :: Bool
  , frameSize :: Int
  }

emptyFrame :: Frame
emptyFrame = Frame Map.empty Nothing False 0

variableOffset :: String -> CodeGen Int
variableOffset s = asks (getOffset s)

getOffset :: String -> Frame -> Int
getOffset var Frame{..} =
  case Map.lookup var offsets of
    Nothing -> frameSize + getOffset var (fromJust parent)
    Just offset -> offset

totalFrameSize :: Frame -> Int
totalFrameSize Frame{..}
  = frameSize + fromMaybe 0 (totalFrameSize <$> parent)

