module SDF3.Normalization where

import Utils
import SDF3.Spec

import qualified Data.Set as Set
import qualified Control.Monad.State.Lazy as State

data Syn = CF | Lex

normalizeSpec :: Spec -> KernSpec
normalizeSpec (Spec _ imps secs) = nimps `unionKernSpec` nsecs
  where
    nimps = normalizeImports imps
    nsecs = indexedOp unionKernSpec emptyKernSpec normalizeSection secs

normalizeImports :: Set.Set Spec -> KernSpec
normalizeImports = indexedOp unionKernSpec emptyKernSpec normalizeSpec

normalizeSection :: Section -> KernSpec
normalizeSection = normalizeSection' emptyKernSpec

normalizeSection' :: KernSpec -> Section -> KernSpec
normalizeSection' kspec (CFSorts sorts) =
  setKernSorts kspec $ mapUnion sorts (normalizeSort CF) ksorts
  where
    ksorts = getKernSorts kspec    

normalizeSection' kspec (LexSorts sorts) =
  setKernSorts kspec $ mapUnion sorts (normalizeSort Lex) ksorts
  where
    ksorts = getKernSorts kspec    

normalizeSection' kspec (LexSyntax prods) =
  setKernSyntax kspec $ mapUnion prods (normalizeProduction Lex) kprods
  where
    kprods = getKernSyntax kspec    

normalizeSection' kspec (CFSyntax prods) =
  setKernSyntax kspec $ mapUnion prods (normalizeProduction CF) kprods
  where
    kprods = getKernSyntax kspec

normalizeSection' kspec (LexStartSymbols sorts) =
  setKernStartSymbols kspec $ mapUnion sorts (normalizeSort Lex) ksorts
  where
    ksorts = getKernStartSymbols kspec

normalizeSection' kspec (CFStartSymbols sorts) =
  setKernStartSymbols kspec $ mapUnion sorts (normalizeSort CF) ksorts
  where
    ksorts = getKernStartSymbols kspec    

normalizeSection' kspec (TemplateOptions opts) =
  setKernTemplateOptions kspec $ mapUnion opts (normalizeTemplateOption Lex) kopts
  where
    kopts = getKernTemplateOptions kspec


normalizeSection' kspec (CFPriorities prs) =
  setKernPriorities kspec $ mapUnion prs normalizePriority kprs
  where
    kprs = getKernPriorities kspec

normalizeSection' kspec (LexRestriction res) =
  setKernRestrictions kspec $ mapUnion res (normalizeRestriction Lex) kres
  where
    kres = getKernRestrictions kspec

normalizeSection' kspec (CFRestriction res) =
  setKernRestrictions kspec $ mapUnion res (normalizeRestriction CF) kres
  where
    kres = getKernRestrictions kspec    

-- | Normalizes a sort.
normalizeSort :: Syn -> Sort -> KernelSort
normalizeSort Lex  (SortLit sort) = KernLexSort sort
normalizeSort CF (SortLit sort) = KernCFSort sort
normalizeSort _ Layout = KernLayout

normalizeSymbol :: Syn -> Symbol Sort -> Symbol KernelSort
normalizeSymbol synb (CCSym cc) = CCSym cc
normalizeSymbol synb (SortSym sort) = SortSym nsort
  where
    nsort = normalizeSort synb sort
normalizeSymbol synb (OptionalSym sym) = OptionalSym nsym
  where
    nsym = normalizeSymbol synb sym
normalizeSymbol synb (ListSym sym m) = ListSym nsym m
  where
    nsym = normalizeSymbol synb sym
normalizeSymbol synb (Sequence sym1 sym2) = Sequence nsym1 nsym2
  where
    nsym1 = normalizeSymbol synb sym1
    nsym2 = normalizeSymbol synb sym2
normalizeSymbol synb (Alternative sym1 sym2) = Alternative nsym1 nsym1
  where
    nsym1 = normalizeSymbol synb sym1
    nsym2 = normalizeSymbol synb sym2

normalizeTemplateSymbol :: Syn -> TemplateSymbol Sort -> TemplateSymbol KernelSort
normalizeTemplateSymbol synb (TLitSym s) = TLitSym s
normalizeTemplateSymbol synb (TLitSort sort) = TLitSort nsort
  where
    nsort = normalizeSort synb sort 
normalizeTemplateSymbol synb (TOptSort sort) = TOptSort nsort
  where
    nsort = normalizeSort synb sort 
normalizeTemplateSymbol synb (TListSym tsym sep m) = TListSym ntsym sep m
  where
    ntsym = normalizeTemplateSymbol synb tsym
normalizeTemplateSymbol synb (TSequence tsym1 tsym2) = TSequence ntsym1 ntsym2
  where
    ntsym1 = normalizeTemplateSymbol synb tsym1 
    ntsym2 = normalizeTemplateSymbol synb tsym2

-- | Normalizes a production.
normalizeProduction :: Syn -> Production Sort -> Production KernelSort
normalizeProduction synb (Prod sort const sym attrs) =
  Prod (normalizeSort synb sort)
       const
       (normalizeSymbol synb sym)
       attrs

normalizeProduction synb (TemplateProd sort const tsym attrs) =
  TemplateProd (normalizeSort synb sort)
               const
               (normalizeTemplateSymbol synb tsym)
               attrs

normalizeTemplateOption :: Syn -> TemplateOption Sort -> TemplateOption KernelSort
normalizeTemplateOption synb (Keyword s) = Keyword s
normalizeTemplateOption synb (Tokenize cc) = Tokenize cc
normalizeTemplateOption synb (AttrSym sort attr) = AttrSym nsort attr
  where
    nsort = normalizeSort synb sort

normalizePriority :: Priority Sort -> Priority KernelSort

normalizePriority (TransPriorityEl pset1 pset2) = TransPriorityEl npset1 npset2
  where
    npset1 = Set.map (normalizeProductionRef CF) pset1
    npset2 = Set.map (normalizeProductionRef CF) pset2

normalizePriority (TransPriority pset next) = TransPriority npset nnext
  where
    npset = Set.map (normalizeProductionRef CF) pset
    nnext = normalizePriority next
    
normalizePriority (NontransPriorityEl pset1 pset2) = NontransPriorityEl npset1 npset2
  where
    npset1 = Set.map (normalizeProductionRef CF) pset1
    npset2 = Set.map (normalizeProductionRef CF) pset2

normalizePriority (NontransPriority pset next) = NontransPriority npset nnext
  where
    npset = Set.map (normalizeProductionRef CF) pset
    nnext = normalizePriority next
        
normalizePriority (IndexTransPriorityEl p1 i p2) = IndexTransPriorityEl np1 i np2
  where
    np1 = normalizeProductionRef CF p1
    np2 = normalizeProductionRef CF p2

normalizePriority (IndexTransPriority p i next) = IndexTransPriority np i nnext
  where
    np = normalizeProductionRef CF p
    nnext = normalizePriority next
    
normalizePriority (IndexNontransPriorityEl p1 i p2) = IndexNontransPriorityEl np1 i np2
  where
    np1 = normalizeProductionRef CF p1
    np2 = normalizeProductionRef CF p2

normalizePriority (IndexNontransPriority p i next) = IndexNontransPriority np i nnext
  where
    np = normalizeProductionRef CF p
    nnext = normalizePriority next
    
normalizePriority (AttrNontransPriorityEl (attr1, pset1) (attr2, pset2))
  = AttrNontransPriorityEl (attr1, npset1) (attr2, npset2)
  where
    npset1 = Set.map (normalizeProductionRef CF) pset1
    npset2 = Set.map (normalizeProductionRef CF) pset2

normalizePriority (AttrNontransPriority (attr, pset) next)
  = AttrNontransPriority (attr, npset) nnext
  where
    npset = Set.map (normalizeProductionRef CF) pset
    nnext = normalizePriority next
    
normalizePriority (AttrTransPriorityEl (attr1, pset1) (attr2, pset2))
  = AttrTransPriorityEl (attr1, npset1) (attr2, npset2)
  where
    npset1 = Set.map (normalizeProductionRef CF) pset1
    npset2 = Set.map (normalizeProductionRef CF) pset2

normalizePriority (AttrTransPriority (attr, pset) next)
  = AttrTransPriority (attr, npset) nnext
  where
    npset = Set.map (normalizeProductionRef CF) pset
    nnext = normalizePriority next

normalizeRestriction :: Syn -> Restriction Sort -> Restriction KernelSort
normalizeRestriction synb (Restrict sym lh) = Restrict nsym lh
  where
    nsym = normalizeSymbol synb sym

normalizeProductionRef :: Syn -> ProductionRef Sort -> ProductionRef KernelSort
normalizeProductionRef synb (ProdRef sort const) = ProdRef nsort const
 where
   nsort = normalizeSort synb sort
