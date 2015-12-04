{-# LANGUAGE RecordWildCards #-}

module CodeGenTypes where

import Common.AST

import Control.Applicative
import Control.Monad.RWS
import Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set


data Var = Var Int
  deriving (Show, Ord, Eq)
data Label = NamedLabel String | UnnamedLabel Int
  deriving (Ord, Eq)
data Operand = OperandVar Var Int | OperandLit Int
  deriving (Show)

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
  | ICall { iLabel :: Label, iArgs :: [(Type, Var)], iDest :: Var }
  | ILabel { iLabel :: Label }

  | IFrameAllocate { iSize :: Int }
  | IFrameFree { iSize :: Int }
  | IFrameRead { iOffset :: Int, iDest :: Var, iType :: Type }
  | IFrameWrite { iOffset :: Int, iValue :: Var, iType :: Type }

  | IHeapRead { iHeapVar :: Var, iDest :: Var, iOperand :: Operand, iType :: Type }
  | IHeapWrite { iHeapVar :: Var, iValue :: Var, iOperand :: Operand, iType :: Type }

  | IArrayAllocate { iDest :: Var, iSize :: Int }
  | IPairAllocate { iDest :: Var }

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
  labels :: [Label],
  frame :: Frame
}

type CodeGen = RWS () [IR] CodeGenState

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
  , definedVariables :: Set String 
  }


typeSize :: Type -> Int
typeSize TyInt = 4
typeSize TyBool = 1
typeSize TyChar = 1
-- Size of a reference to a pair (e.g. in an array)
typeSize (TyPair _ _) = 4 
typeSize (TyArray _) = 4
typeSize t = error (show t)

rootFrame :: Frame
rootFrame = Frame Map.empty Nothing False 0 Set.empty
childFrame :: Frame -> Frame
childFrame parent = Frame Map.empty (Just parent) True 0 Set.empty
 
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
variableOffset s = do
  frame <- gets frame
  return (getOffset s frame)

getOffset :: String -> Frame -> Int
getOffset var Frame{..} =
  if defined then 
    case Map.lookup var offsets of
      Nothing -> frameSize + getOffset var (fromJust parent)
      Just offset -> offset
  else 
    case parent of
      Nothing -> (1000)
      Just f -> frameSize + getOffset var f
--    (frameSize + getOffset var (fromJust parent))
    where
      defined = Set.member var definedVariables

totalAllocatedFrameSize :: Frame -> Int
totalAllocatedFrameSize Frame{..}
  = (if allocated then frameSize else 0) +
    fromMaybe 0 (totalAllocatedFrameSize <$> parent)

