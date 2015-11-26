{-# LANGUAGE RecordWildCards #-}

module RegisterAllocation.ControlFlow where

import CodeGen
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type BasicBlocks = Map Int [IR]
type ControlFlow = Map Int (Set Int)
type LabeledBlocks = Map Label Int

basicBlocks :: [IR] -> (BasicBlocks, ControlFlow, ControlFlow)
basicBlocks ir = (blocks, cfg, revCfg)
  where
    (blocks, labeledBlocks, cfg) = splitIR ir 0 []
    splitIR :: [IR] -> Int -> [IR] -> (BasicBlocks, LabeledBlocks, ControlFlow)
    splitIR [] idx [] = (Map.empty, Map.empty, Map.empty)
    splitIR [] idx b
      = let bs  = Map.singleton idx (reverse b)
            lb  = Map.empty
            cfg = Map.empty
        in (bs, lb, cfg)

    splitIR (i@ILabel {..}:is) idx []
      = let (bs, lb, cfg) = splitIR is idx [i]
            lb' = Map.insert iLabel idx lb
        in (bs, lb', cfg)

    splitIR (i@ILabel {..}:is) idx b
      = let (bs, lb, cfg) = splitIR is (idx + 1) [i]
            bs'  = Map.insert idx (reverse b) bs
            lb'  = Map.insert iLabel (idx + 1) lb
            cfg' = Map.insert idx (Set.singleton (idx + 1)) cfg
        in (bs', lb', cfg')

    splitIR (i:is) idx b | Just ts <- targets i idx
      = let (bs, lb, cfg) = splitIR is (idx + 1) []
            bs'  = Map.insert idx (reverse (i:b)) bs
            cfg' = if not (Set.null ts) then Map.insert idx ts cfg else cfg
        in (bs', lb, cfg')

    splitIR (i:is) idx b = splitIR is idx (i:b)

    targets IJump{..}     _   = Just $ Set.singleton (fromJust (Map.lookup iLabel labeledBlocks))
    targets ICondJump{..} idx = Just $ Set.fromList [idx + 1, fromJust (Map.lookup iLabel labeledBlocks)]
    targets IFunctionEnd  _   = Just $ Set.empty
    targets _ _ = Nothing

    revCfg = Map.foldrWithKey addRevTargets Map.empty cfg
    addRevTargets :: Int -> Set Int -> ControlFlow -> ControlFlow
    addRevTargets source targets acc
      = Set.fold (\target acc -> Map.insertWith Set.union target (Set.singleton source) acc) acc targets

