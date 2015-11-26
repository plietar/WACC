module RegisterAllocation.GraphColouring where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.List
import CodeGen
import GHC.Exts


type RIG = Map Var (Set Var)
type Stack = [Var]
type Colour = Int

maxRegisters :: Int
maxRegisters = 32

colours :: [Int]
colours = [1..32]

sampleRig :: RIG
sampleRig = Map.fromList $ zip (map Var [1..6]) $ map Set.fromList $ map (map Var) lists
  where
    lists = [[3,6],[3,5,6],[1,2,4,5,6],[3,5,6],[2,3,4,6],[1,2,3,4,5]] 

push :: a -> [a] -> Maybe ((), [a])
push e stack = Just ((), e : stack)


pop :: [a] -> Maybe (a, [a])
pop (x : xs) = Just (x, xs)
pop [] = Nothing

colourGraph :: RIG -> Maybe (Map Var Colour)
colourGraph rig
  = case stack of
    Just s -> findColouring s rig colours 
    Nothing -> Nothing
  where
    stack = extractWhileNonempty sortedGraph []
    sortedGraph = sortWith (\(_, set) -> Set.size set) (Map.toList rig)
    --vars = sortWith (\(Var x) -> x) (map fst (Map.toList rig))
    

extractWhileNonempty :: [(Var, Set Var)] -> Stack -> Maybe Stack
extractWhileNonempty ((var, edges) : xs) stack
  | Set.size edges < maxRegisters = extractWhileNonempty (remove var xs) pushedStack  
  | otherwise                     = Nothing
  where
    Just (_, pushedStack) = push var stack
extractWhileNonempty [] stack = Just stack 

remove :: Var -> [(Var, Set Var)] -> [(Var, Set Var)]
remove x ((y, ys) : rest) 
  | x == y = rest
  | otherwise = (y, Set.difference ys (Set.fromList [x])) : remove x rest 
remove _ [] = []



findColouring :: [Var] -> RIG -> [Colour] -> Maybe (Map Var Colour)
findColouring vars rig allCol
  = foldl maybeColour (Just Map.empty) vars
  where
    maybeColour :: Maybe (Map Var Colour) -> Var -> Maybe (Map Var Colour) 
    maybeColour Nothing _ = Nothing
    maybeColour (Just colouring) var = 
      case differentColour var rig colouring allCol of
        Just col -> Just $ Map.insert var col colouring
        Nothing  -> Nothing


differentColour :: Var -> RIG -> Map Var Colour -> [Colour] -> Maybe Colour
differentColour v rig coloured allCol
  = case allNeighbors of
      Nothing -> Just (head allCol)
      Just k  -> getNewColour (Set.elems k) allCol
  where
    allNeighbors = Map.lookup v rig
    -- getnewcolour given the list of neighbors
    getNewColour :: [Var] ->  [Colour] -> Maybe Colour
    getNewColour (n : rest) cols 
      = case Map.lookup n coloured of
          Nothing -> getNewColour rest cols
          Just c -> getNewColour rest (cols \\ [c])
    getNewColour (_:_) [] = Nothing
    getNewColour [] cols = Just (head cols)

sampleMapColour :: Map Var Colour
sampleMapColour = Map.fromList [(Var 1, 2), (Var 2, 3),(Var 3, 4),(Var 4, 3),(Var 6, 1)]
--(Var 5, 2),

testDifferentColour :: Maybe Colour
testDifferentColour = differentColour (Var 5) sampleRig sampleMapColour colours 
