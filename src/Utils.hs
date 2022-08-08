module Utils where

import qualified Data.Set as Set

indexedUnion :: Ord b => (a -> Set.Set b) -> [a] -> Set.Set b
indexedUnion f = foldl (\a x -> (f  x `Set.union` a)) Set.empty

