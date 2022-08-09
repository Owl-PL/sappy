module Utils where

import qualified Data.Set as Set
import Data.Foldable

indexedUnion :: Ord b => (a -> Set.Set b) -> Set.Set a -> Set.Set b
indexedUnion = foldMap'

indexedAnd :: (a -> Bool) -> Set.Set a -> Bool
indexedAnd = indexedOp (&&) True

indexedInsert :: Ord b => (a -> b) -> Set.Set a -> Set.Set b
indexedInsert = indexedOp Set.insert Set.empty 

indexedOp :: Foldable s
          => (c -> b -> b)
          -> b
          -> (a -> c)
          -> s a
          -> b
indexedOp op start i = foldl (\a x -> (i x) `op` a) start

indexedOp' :: Foldable s
           => (c -> b -> b)
           -> b
           -> (a -> c)
           -> s a
           -> b
indexedOp' op start i = foldr (\x r -> (i x) `op` r) start
