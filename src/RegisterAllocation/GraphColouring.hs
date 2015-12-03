{-# LANGUAGE RecordWildCards #-}

module RegisterAllocation.GraphColouring
  (colourGraph, applyColouring) where
import Data.Graph.Inductive
import Data.List
import qualified Data.Map as Map
import CodeGenTypes
import Data.Map (Map,(!))
import Data.Maybe
import Control.Applicative

-- Colour a graph such that no to vertices in the same edge share
-- the same colour
colourGraph :: (Graph gr, Ord a, Eq c) => gr a () -> [c] -> Maybe (Map a c)
colourGraph rig colours
  = case stack of
    Just s -> Map.mapKeys (fromJust . lab rig) <$> findColouring s rig colours
    Nothing -> Nothing
  where
    stack = buildStack rig [] (length colours)
    
-- Build a stack of all nodes in the graph by always pushing a node
-- that is valid (i.e has less than maxR number of neighbors) and update
-- the graph at each step
buildStack :: Graph gr => gr a () -> [Node] -> Int -> Maybe [Node]
buildStack rig stack maxR
  | isEmpty rig = Just stack
  | otherwise = case findValidNode rig (nodes rig) maxR of 
        Just n  -> buildStack (delNode n rig) (n : stack) maxR
        Nothing -> Nothing
     
-- Get a node from the graph which has less than maxR neighbors,
-- where maxR is the number of available colours to colour the graph
findValidNode :: Graph gr => gr a () -> [Node] -> Int -> Maybe Node
findValidNode rig (x:xs) maxR
  | length (suc rig x) < maxR = Just x
  | otherwise                 = findValidNode rig xs maxR
findValidNode _ [] _ = Nothing

-- Find a valid colouring for a graph and (Maybe) return
-- the mapping that is found 
findColouring :: Eq c => Graph gr => [Node] -> gr a () -> [c] -> Maybe (Map Node c)
findColouring nodes rig allCol
  = foldl maybeColour (Just Map.empty) nodes
  where
    maybeColour Nothing _ = Nothing
    maybeColour (Just colouring) node = 
      case getNewColour (suc rig node) allCol colouring of
        Just col -> Just $ Map.insert node col colouring
        Nothing  -> Nothing

-- Find available colour that does not clash with any of 
-- its neihbors.
-- Nothing if there isnt an available colour
getNewColour :: Eq c => [Node] -> [c] -> Map Node c -> Maybe c
getNewColour (_:_) [] _
  = Nothing
getNewColour (n : rest) cols coloured
  = case Map.lookup n coloured of
      Nothing -> getNewColour rest cols coloured
      Just c -> getNewColour rest (cols \\ [c]) coloured
getNewColour [] cols _
  = Just (head cols)

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

colourIR ICall{..} colouring
  = ICall { iLabel = iLabel
          , iArgs  = map (\(ty, v) -> (ty, get v colouring)) iArgs
          , iDest  = get iDest colouring }

colourIR IFrameRead{..} colouring
  = IFrameRead { iOffset = iOffset
               , iDest  = get iDest colouring }

colourIR IFrameWrite{..} colouring
  = IFrameWrite { iOffset = iOffset
                , iValue = get iValue colouring }

colourIR IArrayAllocate{..} colouring
  = IArrayAllocate { iDest = get iDest colouring
                   , iSize = iSize }

colourIR IHeapRead{..} colouring
  = IHeapRead { iHeapVar = get iHeapVar colouring 
              , iDest    = get iDest colouring
              , iOperand = operand }
  where
    operand = case iOperand of
      OperandLit x -> OperandLit x
      OperandVar v s -> OperandVar (get v colouring) s

colourIR IHeapWrite{..} colouring
    = IHeapWrite { iHeapVar = get iHeapVar colouring 
                 , iValue = get iValue colouring
                 , iOperand = operand }
    where
      operand = case iOperand of
        OperandLit x -> OperandLit x
        OperandVar v s -> OperandVar (get v colouring) s

colourIR IPairAllocate{..} colouring
  = IPairAllocate { iDest = get iDest colouring }

colourIR INullCheck{..} colouring
  = INullCheck { iValue = get iValue colouring }

colourIR IBoundsCheck{..} colouring
  = IBoundsCheck { iArray = get iArray colouring
                 , iIndex = get iIndex colouring }

colourIR IPrint{..} colouring
  = IPrint { iValue = get iValue colouring
          , iType = iType
          , iNewline = iNewline }

colourIR IRead{..} colouring
  = IRead { iDest = get iDest colouring
          , iType = iType }

colourIR IFree{..} colouring
  = IFree { iValue = get iValue colouring
          , iType  = iType }

colourIR IExit{..} colouring
  = IExit { iValue = get iValue colouring }

colourIR IReturn{..} colouring
  = IReturn { iValue = get iValue colouring }
-- Base Case
colourIR x colouring  = x



