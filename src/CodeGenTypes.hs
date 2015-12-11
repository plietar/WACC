{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module CodeGenTypes where

import Common.AST
import Features

import Control.Applicative
import Control.Monad.RWS
import Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set

data Var = Local Int | Temp Int | Reg Int | Spilled Int Var
  deriving (Ord, Eq)

{-
argPassingRegs = Reg <$> [0,1]
callerSaveRegs = Reg <$> [0,1,14]
calleeSaveRegs = Reg <$> [4,5,14]
allRegs = Reg <$> [0,1,4,5,14]
spReg = Reg 13
lrReg = Reg 14
pcReg = Reg 15
-}

argPassingRegs :: [Var]
argPassingRegs = Reg <$> [0..3]

callerSaveRegs :: [Var]
callerSaveRegs = Reg <$> ([0..3] ++ [14])

calleeSaveRegs :: [Var]
calleeSaveRegs = Reg <$> ([4..12] ++ [14])

allRegs :: [Var]
allRegs = Reg <$> ([0..12] ++ [14])

spReg :: Var
spReg = Reg 13

lrReg :: Var
lrReg = Reg 14

pcReg :: Var
pcReg = Reg 15

instance Show Var where
  show r | r == spReg = "sp"
  show r | r == lrReg = "lr"
  show r | r == pcReg = "pc"

  show (Local n)     = "local_" ++ show n
  show (Temp n)      = "temp_" ++ show n
  show (Spilled n _) = "spill_" ++ show n
  show (Reg n)       = "r" ++ show n

data Label = NamedLabel String | UnnamedLabel Int
  deriving (Ord, Eq)

data Operand = OperandVar Var Int | OperandLit Int
  deriving (Show)

instance Show Label where
  show (UnnamedLabel i) = "L" ++ show i
  show (NamedLabel   n) = n

data IR
  = IFunctionBegin { iSavedRegs :: [Var], iArgs :: [Var] }
  | IReturn { iSavedRegs :: [Var], iArgs :: [Var] }

  | ILiteral { iDest :: Var, iLiteral :: Literal }
  | IMul { iHigh :: Var, iLow :: Var, iLeft :: Var, iRight :: Var }
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

  | IPushArg { iValue :: Var }
  | IClearArgs { iSize :: Int }

  deriving Show

data CodeGenState = CodeGenState {
  tempNames :: [Var],
  localNames :: [Var],
  labels :: [Label],

  frame :: Frame
}

data CodeGenOutput = CodeGenOutput {
  instructions :: [IR],
  features :: Set Feature
}

instance Monoid CodeGenOutput where
  mempty = CodeGenOutput [] mempty
  mappend (CodeGenOutput a b) (CodeGenOutput a' b')
    = CodeGenOutput (mappend a a') (mappend b b')

type CodeGen = RWS () CodeGenOutput CodeGenState

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
  , definedLocals :: Set String }


typeSize :: Type -> Int
typeSize TyInt = 4
typeSize TyBool = 1
typeSize TyChar = 1
-- Size of a reference to a pair (e.g. in an array)
typeSize (TyPair _ _) = 4 
typeSize (TyArray _) = 4
typeSize t = error (show t)

emptyFrame :: Frame
emptyFrame = Frame Nothing Map.empty Set.empty

setupFrame :: Map String Var -> CodeGen ()
setupFrame args
  = let f = Frame Nothing args (Set.fromList (Map.keys args))
    in modify (\s -> s { frame = f })

withChildFrame :: [String] -> CodeGen a -> CodeGen a
withChildFrame names m = do
  newLocals <- Map.fromList <$> forM names (\n -> (n,) <$> allocateLocal)

  parentFrame <- gets frame
  let childFrame = Frame (Just parentFrame) newLocals Set.empty

  modify (\s -> s { frame = childFrame } )
  ret <- m
  modify (\s -> s { frame = parentFrame } )

  return ret

getFrameLocal :: String -> Frame -> Var
getFrameLocal name Frame{..}
  = if Set.member name definedLocals
    then frameLocals ! name
    else getFrameLocal name (fromJust parent)

