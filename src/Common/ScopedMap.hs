module Common.ScopedMap
  (ScopedMap
  , insertIfNotExists
  , insert
  , lookup
  , empty
  , newScope
  , localTable
  ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Prelude hiding (lookup)

data ScopedMap k v = ScopedMap {
  table :: Map k v,
  parent :: Maybe (ScopedMap k v)
}

insertIfNotExists :: Ord k => k -> v -> ScopedMap k v -> Maybe (ScopedMap k v)
insertIfNotExists key value scopedMap
  = if Map.notMember key (table scopedMap)
    then Just (scopedMap { table = Map.insert key value (table scopedMap) })
    else Nothing

insert :: Ord k => k -> v -> ScopedMap k v -> ScopedMap k v
insert key value scopedMap
  = fromMaybe scopedMap (insertIfNotExists key value scopedMap)

lookup :: Ord k => k -> ScopedMap k v -> Maybe v
lookup key scopedMap
  = Map.lookup key (table scopedMap) <|> (lookup key =<< parent scopedMap)

empty :: ScopedMap k v
empty = ScopedMap { table = Map.empty, parent = Nothing }

newScope :: ScopedMap k v -> ScopedMap k v
newScope parent = ScopedMap { table = Map.empty, parent = Just parent }

localTable :: ScopedMap k v -> Map k v
localTable = table

