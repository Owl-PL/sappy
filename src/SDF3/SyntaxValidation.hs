module SDF3.SyntaxValidation where

import SDF3.Syntax

import Utils

import qualified Data.Set as Set

specSorts :: Spec -> SpecSorts
specSorts (Spec _ imps secs) = (importSpecSorts imps) `unionSpecSorts` (secSpecSorts secs)

secSpecSorts :: [Section] -> SpecSorts
secSpecSorts secs
  = SpecSorts cfsorts lexsorts
  where
    cfsorts  = indexedUnion sectionCFSorts secs
    lexsorts = indexedUnion sectionLexSorts secs

importSpecSorts :: [Spec] -> SpecSorts
importSpecSorts = foldl (\ a x -> (importSpecSort x `unionSpecSorts` a)) emptySpecSorts
 where
   importSpecSort :: Spec -> SpecSorts
   importSpecSort ((Spec _ imps secs)) = (importSpecSorts imps) `unionSpecSorts` (secSpecSorts secs)

sectionCFSorts :: Section -> Set.Set Sort
sectionCFSorts (CFSorts sorts) = Set.fromList sorts
sectionCFSorts _ = Set.empty

sectionLexSorts :: Section -> Set.Set Sort
sectionLexSorts (LexSorts sorts) = Set.fromList sorts
sectionLexSorts _ = Set.empty                   

validProduction :: Set.Set Sort -> Production Symbol -> Bool
validProduction sorts (Prod sort const sym _)
  = (sort `Set.member` sorts) && (symSorts `Set.isSubsetOf` sorts)
 where
   symSorts = sortsInSym sym

   
validProduction sorts (TemplateProd sort const tsym _) 
  = (sort `Set.member` sorts) && (tsymSorts `Set.isSubsetOf` sorts)
 where
   tsymSorts = sortsInTSym tsym

validTemplateOption :: Set.Set Sort -> TemplateOption Symbol -> Bool
validTemplateOption sorts (AttrSym sym _) = symSorts `Set.isSubsetOf` sorts
  where
    symSorts = sortsInSym sym
validTemplateOption _ _ = True    

validPriority :: Set.Set Sort -> Priority Sort -> Bool
validPriority sorts (TransPriority p1 p2)
  = (p1Sorts `Set.isSubsetOf` sorts) && (p2Sorts `Set.isSubsetOf` sorts)
  where
    p1Sorts = sortsInProdRefs p1
    p2Sorts = sortsInProdRefs p2
validPriority sorts (NontransPriority p1 p2)
  = (p1Sorts `Set.isSubsetOf` sorts) && (p2Sorts `Set.isSubsetOf` sorts)
  where
    p1Sorts = sortsInProdRefs p1
    p2Sorts = sortsInProdRefs p2
validPriority sorts (IndexTransPriority p1 _ p2)
  = (p1Sort `Set.member` sorts) && (p2Sort `Set.member` sorts)
  where
    p1Sort = sortInProdRef p1
    p2Sort = sortInProdRef p2
validPriority sorts (IndexNontransPriority p1 _ p2)
  = (p1Sort `Set.member` sorts) && (p2Sort `Set.member` sorts)
  where
    p1Sort = sortInProdRef p1
    p2Sort = sortInProdRef p2
validPriority sorts (AttrTransPriority (_, p1) (_, p2))
  = (p1Sorts `Set.isSubsetOf` sorts) && (p2Sorts `Set.isSubsetOf` sorts)
  where
    p1Sorts = sortsInProdRefs p1
    p2Sorts = sortsInProdRefs p2
validPriority sorts (AttrNontransPriority (_, p1) (_, p2))
  = (p1Sorts `Set.isSubsetOf` sorts) && (p2Sorts `Set.isSubsetOf` sorts)
  where
    p1Sorts = sortsInProdRefs p1
    p2Sorts = sortsInProdRefs p2

sortsInRestriction :: [Restriction Sort] -> Set.Set Sort
sortsInRestriction = foldl (\a x -> (sortInRestriction x) `Set.insert` a) Set.empty

sortInRestriction :: Restriction Sort -> Sort
sortInRestriction (Restrict sort _) = sort

sortsInProdRefs :: [ProductionRef Sort] -> Set.Set Sort
sortsInProdRefs = foldl (\a x -> (sortInProdRef x) `Set.insert` a) Set.empty

sortInProdRef :: ProductionRef Sort -> Sort
sortInProdRef (ProdRef sort _) = sort

sortsInSym :: Symbol -> Set.Set Sort
sortsInSym (SortSym sort)          = Set.singleton sort
sortsInSym (ListSym sort _ _)      = Set.singleton sort
sortsInSym (OptionalSym sym)       = sortsInSym sym
sortsInSym (Sequence sym1 sym2)    = (sortsInSym sym1) `Set.union` (sortsInSym sym2)
sortsInSym (Alternative sym1 sym2) = (sortsInSym sym1) `Set.union` (sortsInSym sym2)
sortsInSym _ = Set.empty

sortsInTSym :: TemplateSymbol -> Set.Set Sort
sortsInTSym (TLitSort sort) = Set.singleton sort
sortsInTSym (TOptSort sort) = Set.singleton sort
sortsInTSym (TListSort sort _ _) = Set.singleton sort
sortsInTSym (TSeqence tsym1 tsym2) = (sortsInTSym tsym1) `Set.union` (sortsInTSym tsym2)
sortsInTSym _ = Set.empty
