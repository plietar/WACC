{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module RegisterAllocation.ControlFlow where

import CodeGen
import Data.Maybe

import Data.Map (Map,(!))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (Graph, DynGraph)
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Query.BFS as Graph

deadCodeElimination :: DynGraph gr => gr [IR] () -> gr [IR] ()
deadCodeElimination cfg = Graph.nfilter (\n -> Set.member n reachable) cfg
  where
    reachable = Set.fromList (Graph.bfs 0 cfg)

basicBlocks :: Graph gr => [IR] -> gr [IR] ()
basicBlocks ir = Graph.mkGraph cfgNodes (map (\e -> Graph.toLEdge e ()) cfgEdges)
  where
    (cfgNodes, cfgEdges, labeledBlocks) = splitIR ir 0 []

    splitIR :: [IR] -> Int -> [IR] -> ([Graph.LNode [IR]], [Graph.Edge], Map Label Int)
    splitIR [] idx [] = ([], [], Map.empty)
    splitIR [] idx b
      = let nodes  = [(idx, reverse b)]
            edges  = []
            labels = Map.empty
        in (nodes, edges, labels)

    splitIR (i@ILabel {..}:is) idx []
      = let (nodes, edges, labels) = splitIR is idx [i]
            labels' = Map.insert iLabel idx labels
        in (nodes, edges, labels')

    splitIR (i@ILabel {..}:is) idx b
      = let (nodes, edges, labels) = splitIR is (idx + 1) [i]
            nodes' = (idx, reverse b) : nodes
            edges' = (idx, idx + 1) : edges
            labels' = Map.insert iLabel (idx + 1) labels
        in (nodes', edges', labels')

    splitIR (i:is) idx b | Just ts <- targets i idx
      = let (nodes, edges, labels) = splitIR is (idx + 1) []
            nodes' = (idx, reverse (i:b)) : nodes
            edges' = map (idx,) ts ++ edges
        in (nodes', edges', labels)

    splitIR (i:is) idx b = splitIR is idx (i:b)

    targets IJump{..}     _   = Just [labeledBlocks ! iLabel]
    targets ICondJump{..} idx = Just [idx + 1, labeledBlocks ! iLabel]
    targets IReturn{..}   _   = Just []
    targets IExit{..}     _   = Just []
    targets _             _   = Nothing

