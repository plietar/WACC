{-# LANGUAGE RecordWildCards #-}

module RegisterAllocation.DataFlow where

import CodeGenTypes

import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.List

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (DynGraph,Graph)
import qualified Data.Graph.Inductive.Graph as Graph

import Data.Foldable (foldrM)
import Control.Monad.Writer

type IRL = (IR, (Set Var, Set Var))

data FlowInfo = FlowInfo
                { vUsed :: Set Var
                , vDef  :: Set Var
                , vIn   :: Set Var
                , vOut  :: Set Var }
  deriving Show

allVariables :: Graph gr => gr [IR] () -> Map Var Int
allVariables cfg = Map.fromList (zip (Set.elems vars) [0..])
  where
    vars = Set.union (Set.unions (map irUse irs)) (Set.unions (map irDef irs))
    irs = concatMap snd (Graph.labNodes cfg)

movesGraph :: Graph gr => Map Var Int -> gr [IR] () -> gr Var ()
movesGraph vars cfg = Graph.mkGraph nodes (map (\e -> Graph.toLEdge e ()) (nub edges))
  where
    irs :: [IR]
    irs = concatMap snd (Graph.labNodes cfg)

    nodes :: [Graph.LNode Var]
    nodes = map swap (Map.assocs vars)

    edges :: [Graph.Edge]
    edges = irMove irs

    irMove :: [IR] -> [Graph.Edge]
    irMove [] = []
    irMove (IMove {..} : is) = (vars ! iDest, vars ! iValue) :
                               (vars ! iValue, vars ! iDest) :
                               (irMove is)
    irMove (_ : is) = irMove is

interferenceGraph :: Graph gr => Map Var Int -> gr ([IRL]) () -> gr Var ()
interferenceGraph vars cfg = Graph.mkGraph nodes (map (\e -> Graph.toLEdge e ()) (nub edges))
  where
    nodes :: [Graph.LNode Var]
    nodes = map swap (Map.assocs vars)

    edges :: [Graph.Edge]
    edges = map (\(v1,v2) -> (vars ! v1, vars ! v2)) (concatMap livePairs liveSets)

    liveSets :: [Set Var]
    liveSets = concatMap (concatMap (\(_, (lI, lO)) -> [lI, lO])) . map snd $ Graph.labNodes cfg

    livePairs :: Set Var -> [(Var, Var)]
    livePairs live = [(v1,v2) | v1 <- vs, v2 <- vs, v1 /= v2]
      where vs = Set.elems live

liveVariables :: DynGraph gr => gr ([IR], FlowInfo) () -> gr [IRL] ()
liveVariables = Graph.nmap bbFlow
  where
    bbFlow :: ([IR], FlowInfo) -> [IRL]
    bbFlow (irs, blockInfo)
      = reverse (execWriter (foldrM irFlow (vOut blockInfo) irs))

    irFlow :: IR -> Set Var -> Writer [IRL] (Set Var)
    irFlow ir out = do
      let lI = Set.union (Set.difference out (irDef ir)) (irUse ir)
      tell [(ir, (lI, Set.union out (irDef ir)))]
      return lI

blockDataFlow :: DynGraph gr => gr [IR] () -> gr ([IR], FlowInfo) ()
blockDataFlow cfg = Graph.gmap (\(x, n, l, y) -> (x, n, (l, final ! n), y)) cfg
  where
    blocks :: Map Int [IR]
    blocks = Map.fromList $ Graph.labNodes cfg

    initial :: Map Int FlowInfo
    initial = Map.map (\bb -> FlowInfo { vUsed = bbUse bb
                                       , vDef = bbDef bb
                                       , vIn = Set.empty
                                       , vOut = Set.empty}) blocks
    final :: Map Int FlowInfo
    final = blockDataFlow' initial (Graph.nodes cfg)

    blockDataFlow' :: Map Int FlowInfo -> [Int] -> Map Int FlowInfo
    blockDataFlow' blockInfoMap []       = blockInfoMap
    blockDataFlow' blockInfoMap (idx:ws)
      = let (blockInfo', toUpdate) = step (Graph.context cfg idx) (blockInfoMap ! idx) blockInfoMap
        in blockDataFlow' (Map.insert idx blockInfo' blockInfoMap) (ws ++ toUpdate)

    step :: Graph.Context [IR] () -> FlowInfo -> Map Int FlowInfo -> (FlowInfo, [Int])
    step ctx blockInfo blockInfoMap = (blockInfo', toUpdate)
      where
        vOut' = Set.unions (map (\cIdx -> vIn (blockInfoMap ! cIdx)) (Graph.suc' ctx))
        vIn'  = Set.union (Set.difference vOut' (vDef blockInfo)) (vUsed blockInfo)
        blockInfo' = blockInfo { vIn = vIn', vOut = vOut' }

        toUpdate :: [Int]
        toUpdate = if vIn blockInfo /= vIn'
                   then Graph.pre' ctx
                   else []

bbDef :: [IR] -> Set Var
bbDef bb = Set.unions (map irDef bb)

bbUse :: [IR] -> Set Var
bbUse bb = bbUse' (reverse bb) Set.empty
  where
    bbUse' :: [IR] -> Set Var -> Set Var
    bbUse' []       use = use
    bbUse' (ir:irs) use = bbUse' irs (Set.union (Set.difference use (irDef ir)) (irUse ir))

irDef :: IR -> Set Var
irDef IFunctionBegin{..} = Set.fromList iArgs
irDef ILiteral{..}       = Set.singleton iDest
irDef IBinOp{..}         = Set.singleton iDest
irDef IMul{..}           = Set.fromList [ iHigh, iLow ]
irDef IUnOp{..}          = Set.singleton iDest
irDef IMove{..}          = Set.singleton iDest
irDef ICall{..}          = Set.fromList (callerSaveRegs)
irDef IFrameRead{..}     = Set.singleton iDest
irDef IHeapRead{..}      = Set.singleton iDest
irDef IGeneratorSize{..} = Set.singleton iDest
irDef _                  = Set.empty

irUse :: IR -> Set Var
irUse IReturn{..}        = Set.fromList iArgs
irUse IBinOp{..}
  = case iOperand of
    OperandVar v _ -> Set.fromList [ iLeft, v ]
    OperandLit x -> Set.fromList [ iLeft ]
irUse IMul{..}           = Set.fromList [ iRight, iLeft ]
irUse IUnOp{..}          = Set.singleton iValue
irUse IMove{..}          = Set.singleton iValue
irUse ICall{..}          = Set.fromList  iArgs
irUse IFrameWrite{..}    = Set.singleton iValue
irUse IHeapWrite{..}
  = case iOperand of
    OperandVar v _ -> Set.fromList [ iHeapVar, iValue, v ]
    OperandLit x -> Set.fromList [ iHeapVar, iValue ]
irUse IHeapRead{..}
  = case iOperand of
    OperandVar v _ -> Set.fromList [ iHeapVar, v ]
    OperandLit x -> Set.fromList [ iHeapVar ]
irUse IPushArg{..}       = Set.singleton iValue
irUse ICompare{..}
  = case iOperand of
    OperandVar v _ -> Set.fromList [ iValue, v ]
    OperandLit x -> Set.fromList [ iValue ]
irUse IJumpReg{..}      = Set.singleton iValue

irUse _                  = Set.empty

