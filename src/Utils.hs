{-# LANGUAGE ViewPatterns #-}
module Utils where

import qualified Data.Set as Set
import Data.Foldable

indexedUnion :: Ord b => (a -> Set.Set b) -> Set.Set a -> Set.Set b
indexedUnion = indexedOp Set.union Set.empty

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

closeSet :: Ord a
        => (a -> a)
        -> [a]
        -> Set.Set a
closeSet op = foldl updateAcc Set.empty
  where
    updateAcc acc a = if (op a `Set.member` acc) then acc else (a `Set.insert` acc)

mapUnion :: Ord ks
         => Set.Set s
         -> (s -> ks)
         -> Set.Set ks
         -> Set.Set ks
mapUnion mapSet mapOp set = (Set.map mapOp mapSet) `Set.union` set 

listToSepString :: Show a => String -> [a] -> String
listToSepString sep (toList -> []) = ""
listToSepString sep (toList -> (x:xs)) = (show x)++(indexedOp' (++) "" (\y -> sep++(show y)) xs)

setToSepString :: Show a => String -> Set.Set a -> String
setToSepString sep (toList -> l) = listToSepString sep l

setToCommaSepString :: Show a => Set.Set a -> String
setToCommaSepString = setToSepString ","

setToNewlineSepString :: Show a => Set.Set a -> String
setToNewlineSepString = setToSepString "\n"

showSet :: Show a => Set.Set a -> String
showSet s = "{"++(setToCommaSepString s)++"}"

showSetNoEmpty :: Show a => Set.Set a -> String
showSetNoEmpty s | null s = ""
showSetNoEmpty s = showSet s
