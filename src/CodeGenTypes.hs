{-# LANGUAGE RecordWildCards #-}

module CodeGenTypes where

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
  deriving (Ord, Eq)

instance Show Label where
  show (UnnamedLabel i) = "L" ++ show i
  show (NamedLabel   n) = n

data IR
  = ILiteral { iDest :: Var, iLiteral :: Literal }
  | IBinOp { iBinOp :: BinOp, iDest :: Var, iLeft :: Var, iRight :: Var }
  | IUnOp { iUnOp :: UnOp, iDest :: Var, iValue :: Var }
  | IMove { iValue :: Var, iDest :: Var }

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

typeSize :: Type -> Int
typeSize TyBool = 1
typeSize TyChar = 1
typeSize (TyPair t t') = typeSize t + typeSize t'
typeSize (TyArray t) = 4
typeSize _ = 4

rootFrame :: Frame
rootFrame = Frame Map.empty Nothing False 0
childFrame :: Frame -> Frame
childFrame parent = Frame Map.empty (Just parent) True 0
 
setVariables :: [(String, Type)] -> Int -> Frame -> Frame
setVariables variables initialOffset original
  = original { offsets = Map.fromList offsetTable
             , frameSize = totalSize }
  where
    (offsetTable, totalSize) = walkVariables variables initialOffset
    walkVariables [] off = ([], off)
    walkVariables ((n,t):vs) off
      = let (vs', off') = walkVariables vs (off + typeSize t)
        in ((n,off):vs', off')

variableOffset :: String -> CodeGen Int
variableOffset s = asks (getOffset s)

getOffset :: String -> Frame -> Int
getOffset var Frame{..} =
  case Map.lookup var offsets of
    Nothing -> frameSize + getOffset var (fromJust parent)
    Just offset -> offset

totalAllocatedFrameSize :: Frame -> Int
totalAllocatedFrameSize Frame{..}
  = if allocated then frameSize else 0 +
    fromMaybe 0 (totalAllocatedFrameSize <$> parent)

