{-# LANGUAGE RecordWildCards #-}

module RegisterAllocation.DataFlow where

import RegisterAllocation.ControlFlow

import CodeGen
import Data.Maybe

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data FlowInfo = FlowInfo
                { vUsed :: Set Var
                , vDef  :: Set Var
                , vIn   :: Set Var
                , vOut  :: Set Var }
  deriving Show

type RIG = Map Var (Set Var)

--dataFlow :: BasicBlocks -> ControlFlow -> ControlFlow -> [(Int, Set Var, Set Var)]
dataFlow blocks cfg revCfg = interferenceGraph
  where
    interferenceGraph :: RIG
    interferenceGraph = Map.unionsWith Set.union (map liveSets liveVariables)
      where
        liveSets :: Set Var -> RIG
        liveSets live = Map.fromList (liveSets' [] (Set.elems live))
          where
            liveSets' :: [Var] -> [Var] -> [(Var, Set Var)]
            liveSets' _ [] = []
            liveSets' xs (y:ys) = (y, Set.fromList (xs ++ ys)) : (liveSets' (y:xs) ys)

    liveVariables :: [Set Var]
    liveVariables = concatMap snd (Map.toList (Map.mapWithKey (\idx irs -> irFlow irs (blockFlow ! idx)) blocks))

    irFlow :: [IR] -> FlowInfo -> [Set Var]
    irFlow irs blockInfo = scanr irFlow' (vOut blockInfo) irs
      where
        irFlow' :: IR -> Set Var -> Set Var
        irFlow' ir out = Set.union (Set.difference out (irDef ir)) (irUse ir)

    blockFlow :: Map Int FlowInfo
    blockFlow = blockFlow' initial (Map.keys blocks)
      where
        initial = Map.map (\bb -> FlowInfo { vUsed = bbUse bb
                                           , vDef = bbDef bb
                                           , vIn = Set.empty
                                           , vOut = Set.empty}) blocks

        blockFlow' blockInfoMap []       = blockInfoMap
        blockFlow' blockInfoMap (idx:ws)
          = let (blockInfo', toUpdate) = step idx (blockInfoMap ! idx) blockInfoMap
            in blockFlow' (Map.insert idx blockInfo' blockInfoMap) (ws ++ Set.elems toUpdate)

    step :: Int -> FlowInfo -> Map Int FlowInfo -> (FlowInfo, Set Int)
    step idx blockInfo blockInfoMap = (blockInfo', toUpdate)
      where
        vOut' = Set.unions (map (\cIdx -> vIn (blockInfoMap ! cIdx)) (Set.elems (children idx)))
        vIn'  = Set.union (Set.difference vOut' (vDef blockInfo)) (vUsed blockInfo)
        blockInfo' = blockInfo { vIn = vIn', vOut = vOut' }

        toUpdate = if vIn blockInfo /= vIn'
                   then parent idx
                   else Set.empty

    children :: Int -> Set Int
    children idx = fromMaybe Set.empty (Map.lookup idx cfg)
    parent :: Int -> Set Int
    parent idx = fromMaybe Set.empty (Map.lookup idx revCfg)

    bbDef :: [IR] -> Set Var
    bbDef bb = Set.unions (map irDef bb)

    bbUse :: [IR] -> Set Var
    bbUse bb = bbUse' (reverse bb) Set.empty 
      where
        bbUse' :: [IR] -> Set Var -> Set Var
        bbUse' []       use = use
        bbUse' (ir:irs) use = bbUse' irs (Set.union (Set.difference use (irDef ir)) (irUse ir))

    irDef :: IR -> Set Var
    irDef (ILiteral{..})       = Set.singleton iDest
    irDef (IBinOp{..})         = Set.singleton iDest
    irDef (IUnOp{..})          = Set.singleton iDest
    irDef (ICall{..})          = Set.singleton iDest
    irDef (IFrameRead{..})     = Set.singleton iDest
    irDef (IArrayAllocate{..}) = Set.singleton iDest
    irDef (IArrayRead{..})     = Set.singleton iDest
    irDef (IArrayLength{..})   = Set.singleton iDest
    irDef (IPairAllocate{..})  = Set.singleton iDest
    irDef (IPairRead{..})      = Set.singleton iDest
    irDef (IRead{..})          = Set.singleton iDest
    irDef _                    = Set.empty

    irUse :: IR -> Set Var
    irUse (IBinOp{..})         = Set.fromList [ iRight, iLeft ]
    irUse (IUnOp{..})          = Set.singleton iValue
    irUse (ICall{..})          = Set.fromList  iArgs
    irUse (IFrameWrite{..})    = Set.singleton iValue
    irUse (IArrayRead{..})     = Set.fromList [ iArray, iIndex ]
    irUse (IArrayWrite{..})    = Set.fromList [ iArray, iIndex, iValue ]
    irUse (IArrayLength{..})   = Set.singleton iArray
    irUse (IPairRead{..})      = Set.singleton iPair
    irUse (IPairWrite{..})     = Set.fromList [ iPair, iValue ]
    irUse (INullCheck{..})     = Set.singleton iValue
    irUse (IBoundsCheck{..})   = Set.fromList [ iArray, iIndex ]
    irUse (IPrint{..})         = Set.singleton iValue
    irUse (IFree{..})          = Set.singleton iValue
    irUse (IExit{..})          = Set.singleton iValue
    irUse _                    = Set.empty

