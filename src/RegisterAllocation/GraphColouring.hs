module RegisterAllocation.GraphColouring where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph.Inductive
import Data.Map (Map)
import Data.Set (Set)
import Data.List
import CodeGen
import GHC.Exts


type Stack = [Int]
type Colour = Int
type Colours = [Colour]

--testing
n = [1,2,3,4,5,6]
es = [[3,6],[3,5,6],[1,2,4,5,6],[3,5,6],[2,3,4,6],[1,2,3,4,5]]

ledges :: [LEdge ()]
ledges = map (\(a,b) -> (a,b,())) $ concatMap (\(x,xs) -> zip (repeat x) xs) (zip n es)

lnodes :: [LNode Int]
lnodes = map (\a -> (a, 0)) n

testg :: Gr Int ()
testg = mkGraph lnodes ledges
--testing

colourGraph :: Graph gr => gr a () -> Colours -> Maybe (Map Node Colour)
colourGraph rig colours
  = case stack of
    Just s -> findColouring s rig colours 
    Nothing -> Nothing
  where
    stack = extractWhileNonempty rig [] (length colours)
    
extractWhileNonempty :: Graph gr => gr a () -> Stack -> Int -> Maybe Stack
extractWhileNonempty rig stack maxR
  | isEmpty rig = Just stack
  | otherwise = case findValidNode rig (nodes rig) maxR of 
        Just n  -> extractWhileNonempty (delNode n rig) (n : stack) maxR
        Nothing -> Nothing
     
findValidNode :: Graph gr => gr a () -> [Node] -> Int -> Maybe Node
findValidNode rig (x:xs) maxR
  | length (neighbors rig x) < maxR = Just x
  | otherwise                       = findValidNode rig xs maxR
findValidNode _ [] _ = Nothing

findColouring :: Graph gr => [Node] -> gr a () -> [Colour] -> Maybe (Map Node Colour)
findColouring nodes rig allCol
  = foldl maybeColour (Just Map.empty) nodes
  where
    maybeColour :: Maybe (Map Node Colour) -> Node -> Maybe (Map Node Colour) 
    maybeColour Nothing _ = Nothing
    maybeColour (Just colouring) node = 
      case getNewColour (neighbors rig node) allCol colouring of
        Just col -> Just $ Map.insert node col colouring
        Nothing  -> Nothing
    
getNewColour :: [Node] -> [Colour] -> Map Node Colour -> Maybe Colour
getNewColour (_:_) [] _
  = Nothing
getNewColour (n : rest) cols coloured
  = case Map.lookup n coloured of
      Nothing -> getNewColour rest cols coloured
      Just c -> getNewColour rest (cols \\ [c]) coloured
getNewColour [] cols _
  = Just (head cols)
