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
type Colours = [Colour]

colourGraph :: RIG -> Colours -> Maybe (Map Var Colour)
colourGraph rig colours
  = case stack of
    Just s -> findColouring s rig colours 
    Nothing -> Nothing
  where
    stack = extractWhileNonempty sortedGraph [] (length colours)
    sortedGraph = sortWith (\(_, set) -> Set.size set) (Map.toList rig)
    

extractWhileNonempty :: [(Var, Set Var)] -> Stack -> Int -> Maybe Stack
extractWhileNonempty ((var, edges) : xs) stack maxColours
  | Set.size edges <= maxColours = extractWhileNonempty (remove var xs) (var : stack) maxColours
  | otherwise                    = Nothing
extractWhileNonempty [] stack _  = Just stack

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
    getNewColour :: [Var] -> [Colour] -> Maybe Colour
    getNewColour (n : rest) cols 
      = case Map.lookup n coloured of
          Nothing -> getNewColour rest cols
          Just c -> getNewColour rest (cols \\ [c])
    getNewColour (_:_) [] = Nothing
    getNewColour [] cols = Just (head cols)

