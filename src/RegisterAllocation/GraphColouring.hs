{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module RegisterAllocation.GraphColouring
  (allocateRegisters) where
import Common.WACCResult
import Common.AST
import Common.Stuff
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
import Control.Monad.Writer
import Data.Tuple (swap)
import GHC.Exts
import Debug.Trace

graphNMapM :: (Functor m, Monad m, DynGraph gr) => (a -> m b) -> gr a c -> m (gr b c)
graphNMapM f gr = do
  nodes <- mapM (\(l, n) -> (l,) <$> f n) (Graph.labNodes gr)
  return (Graph.mkGraph nodes (Graph.labEdges gr))

isSpillable :: Var -> Bool
isSpillable v
  = case v of
    (Reg _) -> False
    GeneratorState -> False
    _ -> True

spillNode :: DynGraph gr => Map Var Int -> Map Int Var -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var () -> [Var -> Var]
             -> (Var, Map Var Int, Map Int Var, gr [IRL] (), gr Var (), gr Var (), gr Var (), [Var -> Var])
spillNode allVars allNodes cfg oRig rig moves spill
  = (spilledVar, allVars', allNodes', cfg'', oRig', rig', moves', spill')
  where
    (spilledNode, spilledVar) = head lnodes
    lnodes = sortWith (\(n, _) -> Down (Graph.deg rig n)) $ filter (\(_,l) -> isSpillable l) (Graph.labNodes rig)

    (cfg', spill', newVars) = runRWS (graphNMapM (mapM findUses) cfg) () spill

    cfg'' = Graph.nmap (map (\(ir, (lI, lO)) -> (ir, (Set.delete spilledVar lI, Set.delete spilledVar lO)))) cfg'

    allVars'  = foldr (uncurry Map.insert . swap) (Map.delete spilledVar allVars) newNodes
    allNodes' = foldr (uncurry Map.insert) (Map.delete spilledNode allNodes) newNodes

    oRig' = Graph.insEdges newEdges (Graph.insNodes newNodes (Graph.delNode spilledNode oRig))
    rig' = Graph.insEdges newEdges' (Graph.insNodes newNodes (Graph.delNode spilledNode rig))
    moves' = Graph.insNodes newNodes (Graph.delNode spilledNode moves)

    newNodes = map (\(n, (v, _)) -> (n,v)) newNodesL
    newEdges = nub $ concatMap (\(n, (_, o)) -> concatMap ((\x -> [(n,x,()), (x,n,())]) . (allVars !)) (Set.elems o)) newNodesL
    newEdges' = nub $ concatMap (\(n, (_, o)) -> concatMap ((\x -> [(n,x,()), (x,n,())])) (filter (isJust . Graph.lab rig) (map (allVars !) (Set.elems o)))) newNodesL
    newNodesL = zip (Graph.newNodes (length newVars) oRig) newVars


    findUses :: IRL -> RWS () [(Var, Set Var)] [Var -> Var] IRL
    findUses irl@(ir, (lI, lO))
      = if (Set.member spilledVar (irUse ir) || Set.member spilledVar (irDef ir))
        then do
          (fs:ss) <- get
          let s = fs spilledVar
          put ss

          let addLI = if Set.member spilledVar (irUse ir)
                      then Set.union lI
                      else id

          let addLO = if Set.member spilledVar (irDef ir)
                      then Set.union lO
                      else id
          tell [(s, Set.delete spilledVar (addLI (addLO (Set.empty))))]

          return (mapIRL (\x -> if x == spilledVar then s else x) irl)
        else return irl

addLoadStoreSpilled :: Map Var Int -> [IR] -> [IR]
addLoadStoreSpilled _ [] = []
addLoadStoreSpilled offsets (ir:irs)
  = mapMaybe addLoad (Set.elems (irUse ir)) ++
    [ir] ++
    mapMaybe addStore (Set.elems (irDef ir)) ++
    addLoadStoreSpilled offsets irs
  where
    addLoad v@(Spilled _ spilledVar) = Just (IFrameRead { iDest = v, iOffset = offsets ! spilledVar, iType = TyInt })
    addLoad _ = Nothing

    addStore v@(Spilled _ spilledVar) = Just (IFrameWrite { iValue = v, iOffset = offsets ! spilledVar, iType = TyInt })
    addStore _ = Nothing

addLoadStoreSpilledGenerator :: Int -> Map Var Int -> [IR] -> [IR]
addLoadStoreSpilledGenerator _ _ [] = []
addLoadStoreSpilledGenerator generatorOffset offsets (ir:irs)
  = mapMaybe addLoad (Set.elems (irUse ir)) ++
    [ir] ++
    mapMaybe addStore (Set.elems (irDef ir)) ++
    addLoadStoreSpilledGenerator generatorOffset offsets irs
  where
    addLoad v@(Spilled _ spilledVar)
      = Just (IHeapRead { iHeapVar = GeneratorState
                        , iDest = v
                        , iOperand = OperandLit (generatorOffset + offsets ! spilledVar)
                        , iType = TyInt })
    addLoad _ = Nothing

    addStore v@(Spilled _ spilledVar)
      = Just (IHeapWrite { iHeapVar = GeneratorState
                         , iValue = v
                         , iOperand = OperandLit (generatorOffset + offsets ! spilledVar)
                         , iType = TyInt })
    addStore _ = Nothing

allocateRegisters :: DynGraph gr => Bool -> Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> WACCResult (gr [IR] (), Map Var Var)
allocateRegisters async allVars cfg rig moves = maybe (codegenError "Graph Colouring failed") OK $ do
  (cfg', colouring, spilledVars) <- colourGraph allRegs allVars cfg rig moves
  let spilledOffsets = Map.fromList (zip spilledVars [0,4..])
      spilledSize    = 4 * length spilledVars
      frameSize      = if async then 0 else spilledSize

      usedRegs       = Set.fromList (Map.elems colouring)
      savedRegs      = filter (`Set.member` usedRegs) calleeSaveRegs
      saveSize       = 4 * length savedRegs

      yieldSize      = 4 + 4 * (maximum (concatMap (map yieldSize' . snd) (Graph.labNodes cfg')))

      yieldSize' :: IRL -> Int
      yieldSize' (IYield{},   (lI, _)) = Set.size (Set.delete GeneratorState lI)
      yieldSize' (IRestore{}, (_, lO)) = Set.size (Set.delete GeneratorState lO)
      yieldSize' _ = 0

      generatorSize  = yieldSize + spilledSize

      fixSavedRegisters :: IR -> IR
      fixSavedRegisters ir@IFunctionBegin{..} = ir { iSavedRegs = savedRegs }
      fixSavedRegisters ir@IReturn{..}        = ir { iSavedRegs = savedRegs }
      fixSavedRegisters ir@IYield{..}         = ir { iSavedRegs = savedRegs }
      fixSavedRegisters ir = ir

      fixFrameSize :: IR -> IR
      fixFrameSize ir@IFrameAllocate{..} = ir { iSize   = iSize + frameSize }
      fixFrameSize ir@IFrameFree{..}     = ir { iSize   = iSize + frameSize }
      fixFrameSize ir@IFrameRead{..}     = ir { iOffset = iOffset + frameSize + saveSize }
      fixFrameSize ir@IFrameWrite{..}    = ir { iOffset = iOffset + frameSize + saveSize }
      fixFrameSize ir = ir

      fixGeneratorSize IGeneratorSize{..}
        = ILiteral { iDest = iDest, iLiteral = LitInt (fromIntegral generatorSize) }
      fixGeneratorSize ir = ir


      fixYield :: IRL -> IRL
      fixYield (ir@IYield{..}, (lI, lO))   = let context = Set.elems (Set.delete GeneratorState lO)
                                             in  ( ir { iSavedContext = context }
                                                 , (lI, lO) )
      fixYield (ir@IRestore{..}, (lI, lO)) = let context = Set.elems (Set.delete GeneratorState lI)
                                             in  ( ir { iSavedContext = context }
                                                 , (lI, lO) )
      fixYield irl = irl

      fixIR :: [IRL] -> [IR]
      fixIR = simplifyMoves .
              applyColouring colouring .
              (if async
               then addLoadStoreSpilledGenerator yieldSize spilledOffsets
               else addLoadStoreSpilled spilledOffsets) .
              map fixFrameSize .
              map fixGeneratorSize .
              map fixSavedRegisters .
              map fst .
              map fixYield

  return (Graph.nmap fixIR cfg', colouring)
  where

-- Merge nodes X and Y, using X's label
mergeNodes :: DynGraph gr =>  Node -> Node -> gr Var () -> gr Var ()
mergeNodes x y g = Graph.insEdges edges (Graph.delNode y g)
  where
    edges = filter (not . Graph.hasLEdge g) (fwd ++ rev)
    fwd = (x,,()) <$> filter (/= x) (Graph.suc g y)
    rev = (,x,()) <$> filter (/= x) (Graph.pre g y)

-- Colour a graph such that no to vertices in the same edge share
-- the same colour
colourGraph :: DynGraph gr => [Var] -> Map Var Int -> gr [IRL] () -> gr Var () -> gr Var () -> Maybe (gr [IRL] (), Map Var Var, [Var])
colourGraph colours allVars cfg rig moves = do
  let allNodes = Map.fromList (Graph.labNodes rig)

  (allNodes', cfg', rig', stack, precolouring, spilledVars)
    <- buildStack allVars allNodes cfg rig rig moves [] (Spilled <$> [0..]) [] (length colours)

  colouring <- augmentColouring rig' colours precolouring stack
  return (cfg', Map.mapKeys (allNodes' !) colouring, spilledVars)

showRIG :: Graph gr => gr Var () -> [String]
showRIG rig = execWriter $ do
  forM_ (Graph.nodes rig) $ \idx -> do
    let ctx = Graph.context rig idx
    tell [(show (Graph.lab' ctx) ++ " - " ++
           show (map (fromJust . Graph.lab rig) (Graph.suc' ctx)))]

-- Build a stack of all nodes in the graph by always pushing a node
-- that is valid (i.e has less than maxR number of neighbors) and update
-- the graph at each step
buildStack :: DynGraph gr => Map Var Int -> Map Int Var -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var () -> [Node] -> [Var -> Var] -> [Var] -> Int
                          -> Maybe (Map Int Var, gr [IRL] (), gr Var (), [Node], Map Node Var, [Var])
buildStack allVars allNodes cfg oRig rig moves stack spill spilledVars maxR
  | isSimplifiedGraph rig = Just (allNodes, cfg, oRig, stack, Map.fromList (Graph.labNodes rig), spilledVars)

  | Just n <- findValidNode rig moves (Graph.nodes rig) maxR
    = buildStack allVars allNodes cfg oRig (Graph.delNode n rig) moves (n : stack) spill spilledVars maxR

  | Just (allVars', allNodes', cfg', oRig', rig', moves') <- tryMerge maxR allVars allNodes cfg oRig rig moves
    = buildStack allVars' allNodes' cfg' oRig' rig' moves' stack spill spilledVars maxR

  | Just moves' <- tryFreeze maxR rig moves
    = buildStack allVars allNodes cfg oRig rig moves' stack spill spilledVars maxR

  | (spilledVar, allVars', allNodes', cfg', oRig', rig', moves', spill') <- spillNode allVars allNodes cfg oRig rig moves spill
    = buildStack allVars' allNodes' cfg' oRig' rig' moves' stack spill' (spilledVar : spilledVars) maxR

isSimplifiedGraph :: Graph gr => gr Var () -> Bool
isSimplifiedGraph rig = all (isPrecoloured . snd) (Graph.labNodes rig)

isPrecoloured :: Var -> Bool
isPrecoloured (Reg x) = True
isPrecoloured _       = False

isMoveRelated :: Graph gr => gr Var () -> Node -> Bool
isMoveRelated moves node = Graph.outdeg moves node > 0

canSimplifyNode :: Graph gr => gr Var () -> gr Var () -> Int -> Node -> Bool
canSimplifyNode rig moves maxR node
  = (not . isPrecoloured . fromJust . Graph.lab rig) node
    && (not . isMoveRelated moves) node
    && (Graph.outdeg rig node) < maxR

-- Get a node from the graph which has less than maxR neighbors,
-- where maxR is the number of available colours to colour the graph
findValidNode :: Graph gr => gr Var () -> gr Var () -> [Node] -> Int -> Maybe Node
findValidNode rig moves (x:xs) maxR
  | canSimplifyNode rig moves maxR x = Just x
  | otherwise                        = findValidNode rig moves xs maxR
findValidNode _ _ [] _               = Nothing

tryMerge :: DynGraph gr => Int -> Map Var Int -> Map Int Var -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var ()
                        -> Maybe (Map Var Int, Map Int Var, gr [IRL] (), gr Var (), gr Var (), gr Var ())
tryMerge maxR allVars allNodes cfg oRig rig moves = tryMerge' (Graph.edges moves)
  where
    tryMerge' [] = Nothing
    tryMerge' ((x,y):es) = tryMergeNodes maxR allVars allNodes cfg oRig rig moves x y
                          <|> tryMerge' es

tryMergeNodes :: DynGraph gr => Int -> Map Var Int -> Map Int Var -> gr [IRL] () -> gr Var () -> gr Var () -> gr Var () -> Node -> Node
                             -> Maybe (Map Var Int, Map Int Var, gr [IRL] (), gr Var (), gr Var (), gr Var ())
tryMergeNodes maxR allVars allNodes cfg oRig rig moves x y = do
  when (Graph.hasNeighbor rig x y) Nothing

  let lx = fromJust (Graph.lab rig x)
      ly = fromJust (Graph.lab rig y)

  (a, b, la, lb) <- case (lx, ly) of
    (Reg _, Reg _) -> Nothing
    (GeneratorState, _) -> Nothing
    (_, GeneratorState) -> Nothing
    (Reg _, _    ) -> Just (x, y, lx, ly)
    (_    , Reg _) -> Just (y, x, ly, lx)
--    (GeneratorState, _) -> Just (x, y, lx, ly)
--    (_, GeneratorState) -> Just (y, x, ly, lx)
    (_    , _    ) -> Just (x, y, lx, ly)

  let allVars'  = Map.delete lb allVars
      allNodes' = Map.delete b allNodes
      cfg'      = Graph.nmap (map (mapIRL (\v -> if v == lb then la else v))) cfg
      oRig'     = mergeNodes a b oRig
      rig'      = mergeNodes a b rig
      moves'    = mergeNodes a b moves
      ok        = ((< maxR) . length . filter (>= maxR) . map (Graph.outdeg rig') . Graph.suc rig') a

  if ok
  then return (allVars', allNodes', cfg', oRig', rig', moves')
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
           , iOperand = mapOperand colouring iOperand }

mapIR colouring IMul{..}
  = IMul { iHigh  = colouring iHigh
         , iLow   = colouring iLow
         , iLeft  = colouring iLeft
         , iRight = colouring iRight }

mapIR colouring IUnOp{..}
  = IUnOp { iUnOp   = iUnOp
           , iDest  = colouring iDest
           , iValue = colouring iValue }

mapIR colouring IMove{..}
  = IMove { iValue = colouring iValue
           , iDest = colouring iDest }

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
              , iOperand = mapOperand colouring iOperand
              , iType    = iType }

mapIR colouring IHeapWrite{..}
    = IHeapWrite { iHeapVar = colouring iHeapVar
                 , iValue = colouring iValue
                 , iOperand = mapOperand colouring iOperand
                 , iType = iType}

mapIR colouring IPushArg{..}
  = IPushArg { iValue = colouring iValue }

mapIR colouring ICompare{..}
  = ICompare { iValue   = colouring iValue
             , iOperand = mapOperand colouring iOperand }

mapIR colouring IJumpReg{..}
  = IJumpReg { iValue = colouring iValue }

mapIR colouring IYield{..}
  = IYield { iSavedRegs = iSavedRegs
           , iValue = colouring iValue
           , iSavedContext = map colouring iSavedContext }

mapIR colouring IRestore{..}
  = IRestore { iValue = colouring iValue
             , iSavedContext = map colouring iSavedContext }

mapIR colouring IGeneratorSize {..}
  = IGeneratorSize { iDest = colouring iDest }

-- Base Case
mapIR colouring x = x

mapOperand :: (Var -> Var) -> Operand -> Operand
mapOperand colouring (OperandLit x) = OperandLit x
mapOperand colouring (OperandVar var shift) = OperandVar (colouring var) shift


simplifyMoves :: [IR] -> [IR]
simplifyMoves = filter (not . isUselessMove)
  where
    isUselessMove IMove{..} = iDest == iValue
    isUselessMove _         = False

