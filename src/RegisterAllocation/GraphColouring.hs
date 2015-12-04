{-# LANGUAGE RecordWildCards #-}

module RegisterAllocation.GraphColouring
  (colourGraph, applyColouring) where
import Data.Graph.Inductive (Graph, Node)
import qualified Data.Graph.Inductive as Graph
import Data.List
import qualified Data.Map as Map
import CodeGenTypes
import Data.Map (Map,(!))
import Data.Maybe
import Control.Applicative

-- Colour a graph such that no to vertices in the same edge share
-- the same colour
colourGraph :: (Graph gr) => gr Var () -> [Var] -> Maybe (Map Var Var)
colourGraph rig colours = do
  (stack, precolouring) <- buildStack rig [] (length colours)
  colouring <- augmentColouring rig colours precolouring stack
  return (Map.mapKeys (fromJust . Graph.lab rig) colouring)
    
-- Build a stack of all nodes in the graph by always pushing a node
-- that is valid (i.e has less than maxR number of neighbors) and update
-- the graph at each step
buildStack :: Graph gr => gr Var () -> [Node] -> Int -> Maybe ([Node], Map Node Var)
buildStack rig stack maxR
  | isSimplifiedGraph rig = Just (stack, Map.fromList (Graph.labNodes rig))
  | otherwise = case findValidNode rig (Graph.nodes rig) maxR of 
        Just n  -> buildStack (Graph.delNode n rig) (n : stack) maxR
        Nothing -> Nothing

isSimplifiedGraph :: Graph gr => gr Var () -> Bool
isSimplifiedGraph rig = all (isPrecoloured . snd) (Graph.labNodes rig)

isPrecoloured :: Var -> Bool
isPrecoloured (Reg x) = True
isPrecoloured _       = False

canSimplifyNode :: Graph gr => gr Var () -> Int -> Node -> Bool
canSimplifyNode rig maxR node = (not . isPrecoloured . fromJust . Graph.lab rig $ node) && length (Graph.suc rig node) < maxR

-- Get a node from the graph which has less than maxR neighbors,
-- where maxR is the number of available colours to colour the graph
findValidNode :: Graph gr => gr Var () -> [Node] -> Int -> Maybe Node
findValidNode rig (x:xs) maxR
  | canSimplifyNode rig maxR x = Just x
  | otherwise                  = findValidNode rig xs maxR
findValidNode _ [] _           = Nothing

-- Find a valid colouring for a graph and (Maybe) return
-- the mapping that is found 
augmentColouring :: Graph gr => gr Var () -> [Var] -> Map Node Var -> [Node] -> Maybe (Map Node Var)
augmentColouring _ _ colouring [] = Just colouring
augmentColouring rig colours colouring (node:xs) = do
  col <- colourNode (Graph.suc rig node) colours colouring
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

colourIR IArrayAllocate{..} colouring
  = IArrayAllocate { iDest = get iDest colouring
                   , iSize = iSize }

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

-- Base Case
colourIR x colouring  = x



