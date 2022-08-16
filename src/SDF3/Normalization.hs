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
normalizeTemplateOption synb (RejectSym sym attr) = RejectSym nsym attr
  where
    nsym = normalizeSymbol synb sym

normalizePriority :: Priority Sort -> Priority KernelSort
normalizePriority (TransP tp)         = TransP        $ normalizeTransPriority tp
normalizePriority (IndexedTransP itp) = IndexedTransP $ normalizeIndexedTransPriority itp
normalizePriority (NonTransP ntp)     = NonTransP     $ normalizeNonTransPriority ntp

normalizeTransPriority :: TransPriority Sort -> TransPriority KernelSort
normalizeTransPriority (ElementTP p1 p2)   = ElementTP  (normalizeProductionRef CF p1) (normalizeProductionRef CF p2)
normalizeTransPriority (ContinueTP p next) = ContinueTP (normalizeProductionRef CF p) (normalizeTransPriority next)

normalizeIndexedTransPriority :: IndexedTransPriority Sort -> IndexedTransPriority KernelSort
normalizeIndexedTransPriority (ElementITP p1 i p2)   = ElementITP  (normalizeProductionRef CF p1) i (normalizeProductionRef CF p2)
normalizeIndexedTransPriority (ContinueITP p i next) = ContinueITP (normalizeProductionRef CF p)  i (normalizeIndexedTransPriority next)

normalizeNonTransPriority :: NonTransPriority Sort -> NonTransPriority KernelSort
normalizeNonTransPriority (ElementNTP p1 p2)    = ElementNTP  (normalizeProductionRef CF p1)   (normalizeProductionRef CF p2)
normalizeNonTransPriority (ElementINTP p1 i p2) = ElementINTP (normalizeProductionRef CF p1) i (normalizeProductionRef CF p2)

normalizeRestriction :: Syn -> Restriction Sort -> Restriction KernelSort
normalizeRestriction synb (Restrict sym lh) = Restrict nsym lh
  where
    nsym = normalizeSymbol synb sym

normalizeProductionRef :: Syn -> ProductionRef Sort -> ProductionRef KernelSort
normalizeProductionRef synb (ProdRef sort const) = ProdRef nsort const
 where
   nsort = normalizeSort synb sort
