module SDF3.SyntaxValidation where

import SDF3.Syntax

import qualified Data.Set as Set

validProduction :: Set.Set Sort -> Production Symbol -> Bool
validProduction sorts (Prod sort const sym _)
  = (sort `Set.member` sorts) && (symSorts `Set.isSubsetOf` sorts)
 where
   symSorts = sortsSym sym

   
validProduction sorts (TemplateProd sort const sym _) 
  = (sort `Set.member` sorts) && (symSorts `Set.isSubsetOf` sorts)
 where
   symSorts = sortsTSym sym

sortsSym :: Symbol -> Set.Set Sort
sortsSym (SortSym sort)          = Set.singleton sort
sortsSym (ListSym sort _ _)      = Set.singleton sort
sortsSym (OptionalSym sym)       = sortsSym sym
sortsSym (Sequence sym1 sym2)    = (sortsSym sym1) `Set.union` (sortsSym sym2)
sortsSym (Alternative sym1 sym2) = (sortsSym sym1) `Set.union` (sortsSym sym2)

sortsTSym :: TemplateSymbol -> Set.Set Sort
sortsTSym = undefined
