{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module CodeGenTypes where

import Common.AST

import Control.Applicative
import Control.Monad.RWS
import Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set


data Var = Local Int | Temp Int | Reg Int | Spilled Int
  deriving (Ord, Eq)

{-
argPassingRegs = Reg <$> [0..1]
callerSaveRegs = Reg <$> [0..1]
calleeSaveRegs = Reg <$> [2..3]
allRegs = Reg <$> [0..3]
-}

argPassingRegs :: [Var]
argPassingRegs = Reg <$> [0..3]

callerSaveRegs :: [Var]
callerSaveRegs = Reg <$> [0..3]

calleeSaveRegs :: [Var]
calleeSaveRegs = Reg <$> [4..11]

allRegs :: [Var]
allRegs = Reg <$> [0..11]

instance Show Var where
  show (Local n) = "local_" ++ show n
  show (Temp n) = "temp_" ++ show n
  show (Spilled n) = "spill_" ++ show n
  show (Reg n) = "r" ++ show n

data Label = NamedLabel String | UnnamedLabel Int
  deriving (Ord, Eq)

data Operand = OperandVar Var Int | OperandLit Int
  deriving (Show)

instance Show Label where
  show (UnnamedLabel i) = "L" ++ show i
  show (NamedLabel   n) = n

data IR
  = IFunctionBegin { iArgs :: [Var] }
  | IReturn

  | ILiteral { iDest :: Var, iLiteral :: Literal }
  | IBinOp { iDest :: Var, iBinOp :: BinOp, iLeft :: Var, iRight :: Var }
  | IUnOp { iDest :: Var, iUnOp :: UnOp, iValue :: Var }
  | IMove { iDest :: Var, iValue :: Var }

  | ICondJump { iLabel :: Label, iValue :: Var }
  | IJump { iLabel :: Label }
  | ICall { iLabel :: Label, iArgs :: [Var] }
  | ILabel { iLabel :: Label }

  | IFrameAllocate { iSize :: Int }
  | IFrameFree { iSize :: Int }
  | IFrameRead { iOffset :: Int, iDest :: Var, iType :: Type }
  | IFrameWrite { iOffset :: Int, iValue :: Var, iType :: Type }

  | IHeapRead { iHeapVar :: Var, iDest :: Var, iOperand :: Operand, iType :: Type }
  | IHeapWrite { iHeapVar :: Var, iValue :: Var, iOperand :: Operand, iType :: Type }

  deriving Show

data CodeGenState = CodeGenState {
  tempNames :: [Var],
  localNames :: [Var],
  labels :: [Label],

  frame :: Frame
}

type CodeGen = RWS () [IR] CodeGenState

allocateTemp :: CodeGen Var
allocateTemp = do
  (v:vs) <- gets tempNames
  modify (\s -> s { tempNames = vs })
  return v

allocateLocal :: CodeGen Var
allocateLocal = do
  (v:vs) <- gets localNames
  modify (\s -> s { localNames = vs })
  return v

allocateLabel :: CodeGen Label
allocateLabel = do
  (l:ls) <- gets labels
  modify (\s -> s { labels = ls })
  return l


data Frame = Frame
  { parent    :: Maybe Frame
  , frameLocals :: Map String Var
  , definedLocals :: Set String 
  , frameSavedRegs :: [(Var, Var)] }


typeSize :: Type -> Int
typeSize TyInt = 4
typeSize TyBool = 1
typeSize TyChar = 1
-- Size of a reference to a pair (e.g. in an array)
typeSize (TyPair _ _) = 4 
typeSize (TyArray _) = 4
typeSize t = error (show t)

emptyFrame :: Frame
emptyFrame = Frame Nothing Map.empty Set.empty []

setupFrame :: Map String Var -> [(Var, Var)] -> CodeGen ()
setupFrame args savedRegs
  = let f = Frame Nothing args (Set.fromList (Map.keys args)) savedRegs
    in modify (\s -> s { frame = f })

withChildFrame :: [String] -> CodeGen a -> CodeGen a
withChildFrame names m = do
  newLocals <- Map.fromList <$> forM names (\n -> (n,) <$> allocateLocal)

  parentFrame <- gets frame
  let childFrame = Frame (Just parentFrame) newLocals Set.empty (frameSavedRegs parentFrame)

  modify (\s -> s { frame = childFrame } )
  ret <- m
  modify (\s -> s { frame = parentFrame } )

  return ret

getFrameLocal :: String -> Frame -> Var
getFrameLocal name Frame{..}
  = if Set.member name definedLocals
    then frameLocals ! name
    else getFrameLocal name (fromJust parent)

