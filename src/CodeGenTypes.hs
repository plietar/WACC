{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module CodeGenTypes where

import Common.AST
import Features

import Arguments
import Control.Applicative
import Control.Monad.RWS
import Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set

data Var = Local Int | Temp Int | Reg Int | Spilled Int Var | GeneratorState
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
callerSaveRegs = Reg <$> ([0..3] ++ [12, 14])

calleeSaveRegs :: [Var]
calleeSaveRegs = Reg <$> ([4..11] ++ [14])

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
  show GeneratorState = "generator"

data Operand = OperandVar Var Int | OperandLit Int

instance Show Operand where
  show (OperandVar var 0) = show var
  show (OperandVar var n) = show var ++ ", lsl #" ++ show n
  show (OperandLit n)     = "#" ++ show n

data Condition = CondEQ | CondNE | CondGE

instance Show Condition where
  show CondEQ = "EQ"
  show CondNE = "NE"
  show CondGE = "GE"


data IR
  = IFunctionBegin { iSavedRegs :: [Var], iArgs :: [Var] }
  | IReturn { iSavedRegs :: [Var], iArgs :: [Var] }

  | ILiteral { iDest :: Var, iLiteral :: Literal }
  | IMul { iHigh :: Var, iLow :: Var, iLeft :: Var, iRight :: Var }
  | IBinOp { iDest :: Var, iBinOp :: BinOp, iLeft :: Var, iOperand :: Operand }
  | IUnOp { iDest :: Var, iUnOp :: UnOp, iValue :: Var }
  | IMove { iDest :: Var, iValue :: Var }

  | ICompare { iValue :: Var, iOperand :: Operand }
  | ICondJump { iLabel :: Label, iCondition :: Condition }
  | IJump { iLabel :: Label }
  | IJumpReg { iValue :: Var }
  | ICall { iLabel :: Label, iArgs :: [Var] }
  | ILabel { iLabel :: Label }

  | IGeneratorSize { iDest :: Var }
  | IFrameAllocate { iSize :: Int }
  | IFrameFree { iSize :: Int }

  | IFrameRead { iOffset :: Int
               , iDest :: Var
               , iType :: Type }

  | IFrameWrite { iOffset :: Int
                , iValue :: Var
                , iType :: Type }

  | IHeapRead { iHeapVar :: Var, iDest :: Var, iOperand :: Operand, iType :: Type }
  | IHeapWrite { iHeapVar :: Var, iValue :: Var, iOperand :: Operand, iType :: Type }

  | IPushArg { iValue :: Var }
  | IClearArgs { iSize :: Int }

  | IYield { iSavedRegs :: [Var]
           , iSavedContext :: [Var]
           , iValue :: Var }
  | IRestore { iSavedContext :: [Var]
             , iValue :: Var }

  deriving Show

data CodeGenReader = CodeGenReader {
  asyncContext :: Bool,
  vtableOffsets :: Map Identifier Int,
  typeTags :: Map Type Int
}

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

type CodeGen = RWST CodeGenReader CodeGenOutput CodeGenState WACCArguments

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
typeSize TyInt        = 4
typeSize TyBool       = 1
typeSize TyChar       = 1
typeSize (TyArray _)  = 4
typeSize (TyTuple _)  = 4
typeSize TyNull       = 0
typeSize t = error ("No type size for: " ++ (show t))

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

