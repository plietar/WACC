{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module RegisterAllocation.GraphColouring
  (allocateRegisters) where
import Common.WACCResult
import Common.AST
import RegisterAllocation.DataFlow

import Data.Graph.Inductive (Graph, DynGraph, Node)
import qualified Data.Graph.Inductive as Graph
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import CodeGenTypes
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.Maybe
import Control.Applicative
import Control.Monad (when)
import Control.Monad.RWS
import Debug.Trace
import GHC.Exts
import Control.Monad.Writer

gLab x g n = fromMaybe (error ("Node:" ++ show x)) (Graph.lab g n)

graphNMapM :: (Monad m, DynGraph gr) => (a -> m b) -> gr a c -> m (gr b c)
graphNMapM f gr = do
  nodes <- mapM (\(l, n) -> (l,) <$> f n) (Graph.labNodes gr)
  return (Graph.mkGraph nodes (Graph.labEdges gr))

spillNode :: DynGraph gr => Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var () -> [Var] -> Int
             -> (Map Var Int, gr [IRL] (), gr Var (), gr Var (), gr Var (), [Var], [(Node, Var)], Int)
spillNode allVars cfg oRig rig moves spill frameOffset
  = (allVars', cfg'', oRig', rig', moves', spill', newNodes, frameOffset + 4)
  where
    (cfg', spill', newVars) = runRWS (graphNMapM findUses cfg) () spill

    cfg'' = Graph.nmap (map (\(ir, (lI, lO)) -> (ir, (Set.delete spilledVar lI, Set.delete spilledVar lO)))) cfg'

    allVars' = foldr (\(n,v) -> Map.insert v n) (Map.delete spilledVar allVars) newNodes

    oRig' = Graph.insEdges newEdges (Graph.insNodes newNodes (Graph.delNode spilledNode oRig))
    rig' = Graph.insEdges newEdges' (Graph.insNodes newNodes (Graph.delNode spilledNode rig))
    moves' = Graph.insNodes newNodes (Graph.delNode spilledNode moves)

    newNodes = map (\(n, (v, _)) -> (n,v)) newNodesL
    newEdges = nub $ concatMap (\(n, (_, o)) -> concatMap ((\x -> [(n,x,()), (x,n,())]) . (allVars !)) (Set.elems o)) newNodesL
    newEdges' = nub $ concatMap (\(n, (_, o)) -> concatMap ((\x -> [(n,x,()), (x,n,())])) (filter (isJust . Graph.lab rig) (map (allVars !) (Set.elems o)))) newNodesL
    newNodesL = zip (Graph.newNodes (length newVars) oRig) newVars

    (spilledNode, spilledVar) = (head lnodes)
    lnodes = sortWith (\(n, _) -> Down (Graph.deg rig n)) $ filter (\(_,l) -> not (isPrecoloured l)) (Graph.labNodes rig)

    findUses :: [IRL] -> RWS () [(Var, Set Var)] [Var] [IRL]
    findUses [] = return []
    findUses (irl@(ir, (lI, lO)) :irs)
      = if (Set.member spilledVar (irUse ir) || Set.member spilledVar (irDef ir))
        then do
          (s:ss) <- get
          put ss

          let addLI = if Set.member spilledVar (irUse ir)
                      then Set.union lI
                      else id

          let addLO = if Set.member spilledVar (irDef ir)
                      then Set.union lO
                      else id
          tell [(s, Set.delete spilledVar (addLI (addLO (Set.empty))))]

          let (ir', (lI', lO')) = mapIRL (\x -> if x == spilledVar then s else x) irl

          let addLoad = if Set.member spilledVar (irUse ir)
                        then ((IFrameRead { iDest = s, iOffset = frameOffset, iType = TyInt }, (Set.delete s lI', lI')):)
                        else id

          let addStore = if Set.member spilledVar (irDef ir)
                         then ((IFrameWrite { iValue = s, iOffset = frameOffset, iType = TyInt }, (lO', Set.delete s lO')):)
                         else id

          addLoad <$> ((ir', (lI', lO')) :) <$> addStore <$> findUses irs
        else ((irl:) <$> findUses irs)

allocateRegisters :: DynGraph gr => Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> WACCResult (gr [IR] (), Map Var Var)
allocateRegisters allVars cfg rig moves = maybe (codegenError "Graph Colouring failed") OK $ do
  (cfg', colouring, frameSize) <- colourGraph allRegs allVars cfg rig moves
  return (Graph.nmap (map (fixFrameSize frameSize) . simplifyMoves . applyColouring colouring . map fst) cfg', colouring)
  where
    fixFrameSize frameSize IFrameAllocate{} = IFrameAllocate { iSize = frameSize }
    fixFrameSize frameSize IFrameFree{}     = IFrameFree     { iSize = frameSize }
    fixFrameSize _ i = i

-- Merge nodes X and Y, using X's label
mergeNodes :: DynGraph gr =>  Node -> Node -> gr Var () -> gr Var ()
mergeNodes x y g = Graph.insEdges edges (Graph.delNode y g)
  where
    edges = filter (not . Graph.hasLEdge g) (fwd ++ rev)
    fwd = (x,,()) <$> filter (/= x) (Graph.suc g y)
    rev = (,x,()) <$> filter (/= x) (Graph.pre g y)

-- Colour a graph such that no to vertices in the same edge share
-- the same colour
colourGraph :: DynGraph gr => [Var] -> Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> Maybe (gr [IRL] (), Map Var Var, Int)
colourGraph colours allVars cfg rig moves = do
  let allNodes = Map.fromList (Graph.labNodes rig)

  (cfg', rig', stack, precolouring, spillings, frameSize)
    <- buildStack allVars cfg rig rig moves [] (Spilled <$> [0..]) [] 0 (length colours)

  colouring <- augmentColouring rig' colours precolouring stack

  let allNodes' = Map.union allNodes (Map.fromList spillings)

  return (cfg', Map.mapKeys (allNodes' !) colouring, frameSize)

showRIG :: Graph gr => gr Var () -> [String]
showRIG rig = execWriter $ do
  forM_ (Graph.nodes rig) $ \idx -> do
    let ctx = Graph.context rig idx
    tell [(show (Graph.lab' ctx) ++ " - " ++
           show (map (fromJust . Graph.lab rig) (Graph.suc' ctx)))]

-- Build a stack of all nodes in the graph by always pushing a node
-- that is valid (i.e has less than maxR number of neighbors) and update
-- the graph at each step
buildStack :: DynGraph gr => Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var () -> [Node] -> [Var] -> [(Node, Var)] -> Int -> Int
                          -> Maybe (gr [IRL] (), gr Var (), [Node], Map Node Var, [(Node, Var)], Int)
buildStack allVars cfg oRig rig moves stack spill spillings frameOffset maxR 
  | isSimplifiedGraph rig = Just (cfg, oRig, stack, Map.fromList (Graph.labNodes rig), spillings, frameOffset)
  | Just n <- findValidNode rig moves (Graph.nodes rig) maxR 
    = buildStack allVars cfg oRig (Graph.delNode n rig) moves (n : stack) spill spillings frameOffset maxR
  | Just (allVars', cfg', oRig', rig', moves') <- tryMerge maxR allVars cfg oRig rig moves
    = buildStack allVars' cfg' oRig' rig' moves' stack spill spillings frameOffset maxR
  | Just moves' <- tryFreeze maxR rig moves
    = buildStack allVars cfg oRig rig moves' stack spill spillings frameOffset maxR
  | (allVars', cfg', oRig', rig', moves', spill', ss, frameOffset') <- spillNode allVars cfg oRig rig moves spill frameOffset
    = buildStack allVars' cfg' oRig' rig' moves' stack spill' (ss ++ spillings) frameOffset' maxR

isSimplifiedGraph :: Graph gr => gr Var () -> Bool
isSimplifiedGraph rig = all (isPrecoloured . snd) (Graph.labNodes rig)

isPrecoloured :: Var -> Bool
isPrecoloured (Reg x) = True
isPrecoloured _       = False

isMoveRelated :: Graph gr => gr Var () -> Node -> Bool
isMoveRelated moves node = Graph.outdeg moves node > 0

canSimplifyNode :: Graph gr => gr Var () -> gr Var () -> Int -> Node -> Bool
canSimplifyNode rig moves maxR node
  = (not . isPrecoloured . gLab 1 rig $ node)
    && not (isMoveRelated moves node)
    && (Graph.outdeg rig node) < maxR

-- Get a node from the graph which has less than maxR neighbors,
-- where maxR is the number of available colours to colour the graph
findValidNode :: Graph gr => gr Var () -> gr Var () -> [Node] -> Int -> Maybe Node
findValidNode rig moves (x:xs) maxR
  | canSimplifyNode rig moves maxR x = Just x
  | otherwise                        = findValidNode rig moves xs maxR
findValidNode _ _ [] _               = Nothing

tryMerge :: DynGraph gr => Int -> Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var ()
                        -> Maybe (Map Var Int, gr [IRL] (), gr Var (), gr Var (), gr Var ())
tryMerge maxR allVars cfg oRig rig moves = tryMerge' (Graph.edges moves)
  where
    tryMerge' [] = Nothing
    tryMerge' ((x,y):es) = tryMergeNodes maxR allVars cfg oRig rig moves x y
                          <|> tryMerge' es 

tryMergeNodes :: DynGraph gr => Int -> Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var () -> Node -> Node
                             -> Maybe (Map Var Int, gr [IRL] (), gr Var (), gr Var (), gr Var ())
tryMergeNodes maxR allVars cfg oRig rig moves x y = do
  when (Graph.hasNeighbor rig x y) Nothing

  let lx = (gLab 2) rig x
      ly = (gLab 3) rig y

  (x', y', lx', ly') <- case (lx, ly) of
    (Reg _, Reg _) -> Nothing
    (Reg _, _    ) -> Just (x, y, lx, ly)
    (_    , Reg _) -> Just (y, x, ly, lx)
    (_    , _    ) -> Just (x, y, lx, ly)

  let allVars' = Map.delete ly' allVars
      cfg'   = Graph.nmap (map (mapIRL (\v -> if v == ly' then lx' else v))) cfg
      oRig'  = mergeNodes x' y' oRig
      rig'   = mergeNodes x' y' rig
      moves' = mergeNodes x' y' moves
      ok = ((< maxR) . length . filter (>= maxR) . map (Graph.outdeg rig') . Graph.suc rig') x'

  if ok
  then return (allVars', cfg', oRig', rig', moves')
  else Nothing

tryFreeze :: DynGraph gr => Int -> gr Var () -> gr Var () -> Maybe (gr Var ())
tryFreeze maxR rig moves = tryFreeze' (Graph.edges moves)
  where
    tryFreeze' [] = Nothing
    tryFreeze' ((x,y):es) = tryFreezeNodes maxR rig moves x y
                          <|> tryFreeze' es 

tryFreezeNodes :: DynGraph gr => Int -> gr Var () -> gr Var () -> Node -> Node -> Maybe (gr Var ())
tryFreezeNodes maxR rig moves x y
  = if (Graph.outdeg rig x < maxR || (isPrecoloured . fromJust . Graph.lab rig) x) &&
       (Graph.outdeg rig y < maxR || (isPrecoloured . fromJust . Graph.lab rig) y)
    then Just (Graph.delEdges [(x,y),(y,x)] moves)
    else Nothing

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
  = map (mapIR (colouring !))

mapIRL :: (Var -> Var) -> IRL -> IRL
mapIRL f (ir, (lI, lO)) = (mapIR f ir, (Set.map f lI, Set.map f lO))

mapIR :: (Var -> Var) -> IR -> IR
mapIR colouring ILiteral{..}
  = ILiteral { iDest = colouring iDest
             , iLiteral = iLiteral }

mapIR colouring IBinOp{..}
  = IBinOp { iBinOp = iBinOp
           , iDest  = colouring iDest
           , iLeft  = colouring iLeft
           , iRight = colouring iRight }

mapIR colouring IUnOp{..}
  = IUnOp { iUnOp   = iUnOp
           , iDest  = colouring iDest
           , iValue = colouring iValue }

mapIR colouring IMove{..}
  = IMove { iValue = colouring iValue
           , iDest = colouring iDest }

mapIR colouring ICondJump{..}
  = ICondJump { iLabel = iLabel
              , iValue = colouring iValue }

mapIR colouring IFrameRead{..}
  = IFrameRead { iOffset = iOffset
               , iDest   = colouring iDest
               , iType   = iType }

mapIR colouring IFrameWrite{..}
  = IFrameWrite { iOffset = iOffset
                , iValue  = colouring iValue
                , iType   = iType }

mapIR colouring IHeapRead{..}
  = IHeapRead { iHeapVar = colouring iHeapVar 
              , iDest    = colouring iDest
              , iOperand = operand
              , iType    = iType }
  where
    operand = case iOperand of
      OperandLit x -> OperandLit x
      OperandVar v s -> OperandVar (colouring v) s

mapIR colouring IHeapWrite{..}
    = IHeapWrite { iHeapVar = colouring iHeapVar 
                 , iValue = colouring iValue
                 , iOperand = operand
                 , iType = iType}
    where
      operand = case iOperand of
        OperandLit x -> OperandLit x
        OperandVar v s -> OperandVar (colouring v) s

-- Base Case
mapIR colouring x = x

simplifyMoves :: [IR] -> [IR]
simplifyMoves = filter (not . isUselessMove)
  where
    isUselessMove IMove{..} = iDest == iValue
    isUselessMove _         = False

