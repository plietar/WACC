module ScopedMap
  (ScopedMap
  , insertIfNotExists
  , ScopedMap.insert
  , ScopedMap.lookup
  , ScopedMap.empty
  , newScope
  ) where
import Data.Maybe
import Data.Map as Map
import Control.Applicative

data ScopedMap k v = Leaf (Map k v) (ScopedMap k v) | Root (Map k v)
insertIfNotExists :: Ord k => k -> v -> ScopedMap k v -> Maybe (ScopedMap k v)
insertIfNotExists key value (Leaf table parent)
  = if Map.notMember key table
    then Just (Leaf (Map.insert key value table) parent)
    else Nothing
insertIfNotExists key value (Root table)
  = if Map.notMember key table
    then Just (Root (Map.insert key value table))
    else Nothing

insert :: Ord k => k -> v -> ScopedMap k v -> ScopedMap k v
insert key value table
  = fromMaybe table (insertIfNotExists key value table)

lookup :: Ord k => k -> ScopedMap k v -> Maybe v
lookup key (Leaf table parent)
  = Map.lookup key table <|> ScopedMap.lookup key parent
lookup key (Root table)
  = Map.lookup key table

empty :: ScopedMap k v
empty = Root Map.empty

newScope :: ScopedMap k v -> ScopedMap k v
newScope parent = Leaf Map.empty parent

