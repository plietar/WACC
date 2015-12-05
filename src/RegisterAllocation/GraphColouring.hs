{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module RegisterAllocation.GraphColouring
  (assignRegisters, applyColouring) where
import Common.WACCResult

import Data.Graph.Inductive (Graph, DynGraph, Node)
import qualified Data.Graph.Inductive as Graph
import Data.List
import qualified Data.Map as Map
import CodeGenTypes
import Data.Map (Map, (!))
import Data.Maybe
import Control.Applicative
import Control.Monad (when)
import Debug.Trace

assignRegisters :: DynGraph gr => gr Var () -> gr Var () -> WACCResult (Map Var Var)
assignRegisters rig moves
  = case colourGraph allRegs rig moves of
    Just c  -> OK c
    Nothing -> codegenError "Graph Colouring failed"

-- Merge nodes X and Y, using X's label
mergeNodes :: DynGraph gr =>  Node -> Node -> gr Var () -> gr Var ()
mergeNodes x y g = Graph.insEdges (fwd ++ rev) g'
  where
    g'  = Graph.delNode y g
    fwd = (x,,()) <$> Graph.suc g' y 
    rev = (,x,()) <$> Graph.pre g' y


-- Colour a graph such that no to vertices in the same edge share
-- the same colour
colourGraph :: DynGraph gr => [Var] -> gr Var () -> gr Var () -> Maybe (Map Var Var)
colourGraph colours rig moves = do
  (stack, precolouring, coalescings) <- buildStack rig moves [] [] (length colours)

  -- Augment the precolouring with the coalesced nodes
  let precolouring' = Map.union precolouring (Map.mapMaybe (\n -> Map.lookup n precolouring) coalescings)

  colouring <- augmentColouring rig colours precolouring' stack

  let remap i = fromMaybe (remap (coalescings ! i)) (Map.lookup i colouring)
      coalescings' = Map.map remap coalescings
      colouring' = Map.union colouring coalescings'

  return (Map.mapKeys (fromJust . Graph.lab rig) colouring')

-- Build a stack of all nodes in the graph by always pushing a node
-- that is valid (i.e has less than maxR number of neighbors) and update
-- the graph at each step
buildStack :: DynGraph gr => gr Var () -> gr Var () -> [Node] -> [(Node, Node)] -> Int -> Maybe ([Node], Map Node Var, Map Node Node)
buildStack rig moves stack coalescings maxR
  | isSimplifiedGraph rig = Just (stack, Map.fromList (Graph.labNodes rig), Map.fromList coalescings)
  | Just n <- findValidNode rig moves (Graph.nodes rig) maxR 
    = buildStack (Graph.delNode n rig) moves (n : stack) coalescings maxR
  | Just (rig', moves', c) <- tryMerge maxR rig moves
    = buildStack rig' moves' stack (c:coalescings) maxR
  | Just moves' <- tryFreeze maxR rig moves
    = buildStack rig moves' stack coalescings maxR
  | otherwise = Nothing

isSimplifiedGraph :: Graph gr => gr Var () -> Bool
isSimplifiedGraph rig = all (isPrecoloured . snd) (Graph.labNodes rig)

isPrecoloured :: Var -> Bool
isPrecoloured (Reg x) = True
isPrecoloured _       = False

isMoveRelated :: Graph gr => gr Var () -> Node -> Bool
isMoveRelated moves node = Graph.outdeg moves node > 0

canSimplifyNode :: Graph gr => gr Var () -> gr Var () -> Int -> Node -> Bool
canSimplifyNode rig moves maxR node
  = (not . isPrecoloured . fromJust . Graph.lab rig $ node)
    && not (isMoveRelated moves node)
    && (Graph.outdeg rig node) < maxR

-- Get a node from the graph which has less than maxR neighbors,
-- where maxR is the number of available colours to colour the graph
findValidNode :: Graph gr => gr Var () -> gr Var () -> [Node] -> Int -> Maybe Node
findValidNode rig moves (x:xs) maxR
  | canSimplifyNode rig moves maxR x = Just x
  | otherwise                        = findValidNode rig moves xs maxR
findValidNode _ _ [] _               = Nothing

tryMerge :: DynGraph gr => Int -> gr Var () -> gr Var () -> Maybe (gr Var (), gr Var (), (Node, Node))
tryMerge maxR rig moves = tryMerge' (Graph.edges moves)
  where
    tryMerge' [] = Nothing
    tryMerge' ((x,y):es) = tryMergeNodes maxR rig moves x y
                          <|> tryMerge' es 

tryMergeNodes :: DynGraph gr => Int -> gr Var () -> gr Var () -> Node -> Node -> Maybe (gr Var (), gr Var (), (Node, Node))
tryMergeNodes maxR rig moves x y = do
  when (Graph.hasNeighbor rig x y) Nothing

  let lx = fromJust (Graph.lab rig x)
      ly = fromJust (Graph.lab rig y)

  (x',y') <- case (lx, ly) of
    (Reg _, Reg _) -> Nothing
    (Reg _, _    ) -> Just (x, y)
    (_    , Reg _) -> Just (y, x)
    (_    , _    ) -> Just (x, y)

  let rig'   = mergeNodes x' y' rig
      moves' = mergeNodes x' y' moves
      ok = ((< maxR) . length . filter (>= maxR) . map (Graph.outdeg rig') . Graph.suc rig') x'

  if ok
  then return (rig', moves', (y', x'))
  else Nothing

tryFreeze :: DynGraph gr => Int -> gr Var () -> gr Var () -> Maybe (gr Var ())
tryFreeze maxR rig moves = tryFreeze' (Graph.edges moves)
  where
    tryFreeze' [] = Nothing
    tryFreeze' ((x,y):es) = tryFreezeNodes maxR rig moves x y
                          <|> tryFreeze' es 

tryFreezeNodes :: DynGraph gr => Int -> gr Var () -> gr Var () -> Node -> Node -> Maybe (gr Var ())
tryFreezeNodes maxR rig moves x y
  = if (Graph.outdeg rig x < maxR) && (Graph.outdeg rig y < maxR)
    then Just (Graph.delEdges [(x,y),(y,x)] moves)
    else Nothing

-- Find a valid colouring for a graph and (Maybe) return
-- the mapping that is found 
augmentColouring :: Graph gr => gr Var () -> [Var] -> Map Node Var -> [Node] -> Maybe (Map Node Var)
augmentColouring _ _ colouring [] = Just colouring
augmentColouring rig colours colouring (node:xs) = do
  col <- colourNode (Graph.suc rig node) colours colouring
  trace (show (col, (fromJust . Graph.lab rig) node, map (fromJust . Graph.lab rig) (Graph.suc rig node), map (\n -> Map.lookup n colouring) (Graph.suc rig node))) (return ())
  let colouring' = Map.insert node col colouring
  augmentColouring rig colours colouring' xs

-- Find available colour that does not clash with any of 
-- its neihbors.
-- Nothing if there isnt an available colour
colourNode :: Eq c => [Node] -> [c] -> Map Node c -> Maybe c
colourNode _ [] _       = Nothing
colourNode [] (col:_) _ = Just col
colourNode (n : rest) cols coloured
  = case Map.lookup n coloured of
      Nothing -> colourNode rest cols coloured
      Just c -> colourNode rest (cols \\ [c]) coloured



-- Apply Graph Colouring to the Intermediate Representation
applyColouring :: Map Var Var -> [IR] -> [IR]
applyColouring colouring
  = map (\ir -> colourIR ir colouring)

-- Helper method
get :: Var -> Map Var Var -> Var
get v colouring = (colouring ! v)

colourIR :: IR -> Map Var Var -> IR
colourIR ILiteral{..} colouring
  = ILiteral { iDest = get iDest colouring
             , iLiteral = iLiteral }

colourIR IBinOp{..} colouring
  = IBinOp { iBinOp = iBinOp
           , iDest  = get iDest colouring
           , iLeft  = get iLeft colouring
           , iRight = get iRight colouring }

colourIR IUnOp{..} colouring
  = IUnOp { iUnOp   = iUnOp
           , iDest  = get iDest colouring
           , iValue = get iValue colouring }

colourIR IMove{..} colouring
  = IMove { iValue = get iValue colouring
           , iDest = get iDest colouring }

colourIR ICondJump{..} colouring
  = ICondJump { iLabel = iLabel
              , iValue = get iValue colouring }

colourIR IFrameRead{..} colouring
  = IFrameRead { iOffset = iOffset
               , iDest   = get iDest colouring
               , iType   = iType }

colourIR IFrameWrite{..} colouring
  = IFrameWrite { iOffset = iOffset
                , iValue  = get iValue colouring
                , iType   = iType }

colourIR IHeapRead{..} colouring
  = IHeapRead { iHeapVar = get iHeapVar colouring 
              , iDest    = get iDest colouring
              , iOperand = operand
              , iType    = iType }
  where
    operand = case iOperand of
      OperandLit x -> OperandLit x
      OperandVar v s -> OperandVar (get v colouring) s

colourIR IHeapWrite{..} colouring
    = IHeapWrite { iHeapVar = get iHeapVar colouring 
                 , iValue = get iValue colouring
                 , iOperand = operand
                 , iType = iType}
    where
      operand = case iOperand of
        OperandLit x -> OperandLit x
        OperandVar v s -> OperandVar (get v colouring) s

-- Base Case
colourIR x colouring  = x

