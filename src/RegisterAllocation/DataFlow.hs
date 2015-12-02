{-# LANGUAGE RecordWildCards #-}

module RegisterAllocation.DataFlow where

import CodeGen

import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.List

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (Graph)
import qualified Data.Graph.Inductive.Graph as Graph

data FlowInfo = FlowInfo
                { vUsed :: Set Var
                , vDef  :: Set Var
                , vIn   :: Set Var
                , vOut  :: Set Var }
  deriving Show

interferenceGraph :: Graph gr => [Set Var] -> gr Var ()
interferenceGraph liveSets = Graph.mkGraph nodes (map (\e -> Graph.toLEdge e ()) (nub edges))
  where
    nodes :: [Graph.LNode Var]
    nodes = zip [0..] (Set.elems (Set.unions liveSets))

    varMap :: Map Var Int
    varMap = Map.fromList (map swap nodes)

    edges :: [Graph.Edge]
    edges = map (\(v1,v2) -> (varMap ! v1, varMap ! v2)) (concatMap livePairs liveSets)

    livePairs :: Set Var -> [(Var, Var)]
    livePairs live = [(v1,v2) | v1 <- vs, v2 <- vs, v1 /= v2]
      where vs = Set.elems live

liveVariables :: Graph gr => gr [IR] () -> Map Int FlowInfo -> [Set Var]
liveVariables cfg flowInfo = concatMap bbFlow nodes
  where
    nodes :: [Int]
    nodes = Graph.nodes cfg

    bbFlow :: Int -> [Set Var]
    bbFlow idx
      = let irs = fromJust (Graph.lab cfg idx)
            blockInfo = flowInfo ! idx
        in  scanr irFlow (vOut blockInfo) irs

    irFlow :: IR -> Set Var -> Set Var
    irFlow ir out = Set.union (Set.difference out (irDef ir)) (irUse ir)

blockDataFlow :: Graph gr => gr [IR] () -> Map Int FlowInfo
blockDataFlow cfg = blockDataFlow' initial (Graph.nodes cfg)
  where
    blocks :: Map Int [IR]
    blocks = Map.fromList $ Graph.labNodes cfg

    initial :: Map Int FlowInfo
    initial = Map.map (\bb -> FlowInfo { vUsed = bbUse bb
                                       , vDef = bbDef bb
                                       , vIn = Set.empty
                                       , vOut = Set.empty}) blocks

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
irDef ILiteral{..}       = Set.singleton iDest
irDef IBinOp{..}         = Set.singleton iDest
irDef IUnOp{..}          = Set.singleton iDest
irDef IMove{..}          = Set.singleton iDest
irDef ICall{..}          = Set.singleton iDest
irDef IFrameRead{..}     = Set.singleton iDest
irDef IArrayAllocate{..} = Set.singleton iDest
irDef IArrayRead{..}     = Set.singleton iDest
irDef IArrayLength{..}   = Set.singleton iDest
irDef IPairAllocate{..}  = Set.singleton iDest
irDef IPairRead{..}      = Set.singleton iDest
irDef IRead{..}          = Set.singleton iDest
irDef _                  = Set.empty

irUse :: IR -> Set Var
irUse IBinOp{..}         = Set.fromList [ iRight, iLeft ]
irUse IUnOp{..}          = Set.singleton iValue
irUse IMove{..}          = Set.singleton iValue
irUse ICall{..}          = Set.fromList  iArgs
irUse IFrameWrite{..}    = Set.singleton iValue
irUse IArrayRead{..}     = Set.fromList [ iArray, iIndex ]
irUse IArrayWrite{..}    = Set.fromList [ iArray, iIndex, iValue ]
irUse IArrayLength{..}   = Set.singleton iArray
irUse IPairRead{..}      = Set.singleton iPair
irUse IPairWrite{..}     = Set.fromList [ iPair, iValue ]
irUse INullCheck{..}     = Set.singleton iValue
irUse IBoundsCheck{..}   = Set.fromList [ iArray, iIndex ]
irUse IPrint{..}         = Set.singleton iValue
irUse IFree{..}          = Set.singleton iValue
irUse IExit{..}          = Set.singleton iValue
irUse IReturn{..}        = Set.singleton iValue
irUse _                  = Set.empty

