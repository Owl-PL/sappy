-- | The SDF3 syntax is divided into two main parts:
--
--   1. The surface specification.
--
--   2. The kernel specification.
--
--   The first is the specification defined by the user while the second is
--   the normalized version of the surface specification.
module SDF3.Spec where

import qualified Data.Set as Set

-- * Surface Specification

-- | The surface specification consists of a module name, any imported
--   specifications and a set of sections.
data Spec
  = Spec { moduleName :: String,         -- ^ The module name.
           imports    :: Set.Set Spec,   -- ^ All imported specifications.
           sections   :: Set.Set Section -- ^ The set of sections making up the specificaiton.
         }

data SpecSorts
  = SpecSorts { cfSorts  :: Set.Set Sort,
                lexSorts :: Set.Set Sort
              }

emptySpecSorts :: SpecSorts
emptySpecSorts = SpecSorts Set.empty Set.empty

unionSpecSorts :: SpecSorts -> SpecSorts -> SpecSorts
unionSpecSorts (SpecSorts c1 l1) (SpecSorts c2 l2) = SpecSorts (c1 `Set.union` c2) (l1 `Set.union` l2)

-- | Sections make up the entirity of the specification, and
--   consist of:
data Section
  = CFSorts         (Set.Set Sort)                   -- ^ Context-free sorts (non-terminals).
  | LexSorts        (Set.Set Sort)                   -- ^ Lexical syntax sorts (non-terminals).
  | LexSyntax       (Set.Set (Production Sort))    -- ^ The lexical syntax.
  
  | CFSyntax        (Set.Set (Production Sort))    -- ^ The context-free syntax.
  
  | LexStartSymbols (Set.Set Sort)                   -- ^ The lexical start symbols
                                                     --   (non-terminals).
  | CFStartSymbols  (Set.Set Sort)                   -- ^ The context-free start symbols
                                                     --   (non-terminals).
  | TemplateOptions (Set.Set (TemplateOption Sort))  -- ^ The set of template options.
  
  | CFPriorities    (Set.Set (Priority Sort))        -- ^ The set of context-free priorities;
                                                     --   used for disambiguation.
  | LexRestriction  (Set.Set (Restriction Sort))     -- ^ Lexical restrictions;
                                                     --   used for disambiguation.
  | CFRestriction   (Set.Set (Restriction Sort))     -- ^ Context-free restrictions;
                                                     --   used for disambiguation.    

-- | Productions make up the lexical and context-free syntax sections,
--   and consist of:
data Production sort
    -- | An ordinary production of the form
    --      
    -- @sym.const = sym {attributes}@
    -- 
    -- where @sym@ is a word over 'SDF3.Symbol'.         
  = Prod sort String (Symbol sort) (Set.Set Attribute)
    -- | A template production of the form
    --      
    -- @sym.const = [w] {attributes}@
    --
    -- or
    --
    -- @sym.const = \<w\> {attributes}@
    -- 
    -- where @w@ is a word over 'SDF3.TemplateSymbol'.
  | TemplateProd sort String (TemplateSymbol sort) (Set.Set Attribute)
  deriving (Eq,Ord)

lexProd :: sort -> (Symbol sort) -> Set.Set Attribute -> Production sort
lexProd sort = Prod sort ""

-- | Template options place restrictions on the lexical syntax.  They consist of:
data TemplateOption sort
  = Keyword  (Set.Set CharClass)  -- ^ Used to setup follow restrictions on keywords.
  | Tokenize [Char]               -- ^ Specifies which characters have layout around them.
  | AttrSym  sort Attribute       -- ^ Mainly used to setup reject rules for keywords.
  deriving (Eq,Ord)

-- | Priorities are used to place weighted restrictions on productions
--   to prevent ambiguties; e.g., precedence.
data Priority sort
  = TransPriority         (Set.Set (ProductionRef sort))     (Set.Set (ProductionRef sort)) -- ^ The transitive ordering.
  
  | NontransPriority      (Set.Set (ProductionRef sort))     (Set.Set (ProductionRef sort)) -- ^ The non-transitive ordering.
  
  | IndexTransPriority    (ProductionRef sort) Int (ProductionRef sort)                     -- ^ The transitive ordering with an index.
  
  | IndexNontransPriority (ProductionRef sort) Int (ProductionRef sort)                     -- ^ The non-transitive ordering with an index.
  
  | AttrNontransPriority                                                                    -- ^ The transitive ordering with attribute labels.
    (Attribute, (Set.Set (ProductionRef sort)))
    (Attribute, (Set.Set (ProductionRef sort)))
    
  | AttrTransPriority                                                                       -- ^ The non-transitive ordering with attribute labels.
    (Attribute, (Set.Set (ProductionRef sort)))
    (Attribute, (Set.Set (ProductionRef sort)))
  deriving (Eq,Ord)

-- | Restrictions filter applications of productions for certain
--   non-terminals (@sort@) if the following character, the lookahead,
--   is in a particular character class ('SDF3.Lookahead').
data Restriction sort
  = Restrict sort        -- ^ The symbol to restrict.
             Lookahead   -- ^ A character class
  deriving (Eq,Ord)

-- | A production reference is of the form:
--
--   @sym.const@
--
--  where @sym@ is a some non-terminal and @const@ is some constructor.
data ProductionRef sort
  = ProdRef sort      -- ^ Some non-terminal.
            String    -- ^ Some constructor of @sym@.
  deriving (Eq,Ord)

-- | Symbols are the basic lexical structure of surface
--   specifications.  They consist of:
data Symbol sort
  = CCSym       CharClass                    -- ^ Character classes
  | SortSym     sort                         -- ^ Sorts (non-terminals)
  | OptionalSym (Symbol sort)                -- ^ Optional symbols (@sym?@)
  | ListSym     (Symbol sort) LMode          -- ^ Lists of symbols (@sym*@ or @sym+@)
  | Sequence    (Symbol sort) (Symbol sort)  -- ^ Sequences of symbols (@sym sym@)
  | Alternative (Symbol sort) (Symbol sort)  -- ^ Alternative symbols (@sym | sym@)
  deriving (Eq,Ord)

-- | Template symbols are the basic lexical structure of template
--   productions in the surface specification.  They consist of:
data TemplateSymbol sort
  = TLitSym   String                                      -- ^ Literal symbols (strings)
  | TLitSort  sort                                        -- ^ Sorts (non-terminals)
  | TOptSort  sort                                        -- ^ Optional sorts (@sym?@)
  | TListSort sort String LMode                           -- ^ Lists of symbols (@sym*@ or @sym+@)  
  | TSeqence (TemplateSymbol sort) (TemplateSymbol sort)  -- ^ Sequences of symbols (@sym sym@)
  deriving (Eq,Ord)

-- | Sorts define the non-terminals of the specification.
data Sort = SortLit String  -- ^ A sort defined by the user.
          | Layout          -- ^ A reserved sort name used to indicate the
                            --   whitespace that can appear between
                            --   context-free symbols.
  deriving (Eq,Ord)
            
-- | Character classes describe sets of lexical characters.  They consist of:
data CharClass
  = Class        [Char]                -- ^ A character class; e.g, @[a-z]@, @[A-Z]@, etc.
  | Complement   CharClass             -- ^ The complement of a character class.
  | Difference   CharClass CharClass   -- ^ The difference of two character classes.
  | Union        CharClass CharClass   -- ^ The union of two character classes.
  | Intersection CharClass CharClass   -- ^ The intersection of two character classes.
  deriving (Eq,Ord)

-- | Attributes describe restrictions on productinos. They consist of:
data Attribute
  = Left            -- ^ Left associative.
  | Right           -- ^ Right associative.
  | Nonassoc        -- ^ Non-associative.
  | Assoc           -- ^ Associative.
  | Bracket         -- ^ Bracketing.
  | Reject          -- ^ Keyword reservation.
  deriving (Eq,Ord)

-- | A set of character classes that describe the lookahead symbol.
type Lookahead = Set.Set CharClass

-- | Describe whether a symbol list can be empty or non-empty.
data LMode = ZeroManyList  -- ^ List contains zero or more elements; e.g., can be empty.
           | OneManyList   -- ^ List contains one or more elements; e.g., must be non-empty.
  deriving (Eq,Ord)

-- * Kernel Specification

-- | The kernel specification is the normalized version of the surface
--   language.  This implies that all imports have been combined into
--   a single specification, and hence, there is no need for any
--   module names, and symbols have been explicitly annotated with a
--   label indicating whether or not they are context-free syntax of
--   lexical syntax.
--
--   Therefore, a kernel specficiation is simply a set of kernel sections.
data KernSpec = KernSpec {
  kernSorts             :: Set.Set KernelSort,                     -- ^ Set of kernel sorts (non-terminals).
  kernSyntax            :: Set.Set (Production KernelSort),        -- ^ The syntax (productions).
  kernStartSymbols      :: Set.Set (KernelSort),                   -- ^ The start symbols.
  kernTemplateOptions   :: Set.Set (TemplateOption KernelSort),    -- ^ The set of template options.
  kernPriorities        :: Set.Set (Priority KernelSort),          -- ^ The set of priorities; used for disambiguation.
  kernRestrictions      :: Set.Set (Restriction KernelSort)        -- ^ The set of restrictions; used for disambiguation.
} 

-- | Kernel sorts come in three flavors:
data KernelSort
  = KernCFSort String  -- ^ Context-free sorts.
  | KernLexSort String -- ^ Lexical sorts.
  | KernLayout         -- ^ Indicates where whitespace can be placed.
  deriving (Eq,Ord)

emptyKernSpec :: KernSpec
emptyKernSpec = KernSpec Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty

unionKernSpec :: KernSpec -> KernSpec -> KernSpec
unionKernSpec (KernSpec ksorts1 ksyn1 kst1 kto1 kpr1 krs1) (KernSpec ksorts2 ksyn2 kst2 kto2 kpr2 krs2)
  = KernSpec (ksorts1 `Set.union` ksorts2)
             (ksyn1   `Set.union` ksyn2)
             (kst1    `Set.union` kst2)
             (kto1    `Set.union` kto2)
             (kpr1    `Set.union` kpr2)
             (krs1    `Set.union` krs2)

getKernSorts :: KernSpec -> Set.Set KernelSort
getKernSorts = kernSorts

setKernSorts :: KernSpec -> Set.Set KernelSort -> KernSpec
setKernSorts (KernSpec _ ksyn kstartsyms ktopts kpris kres) ksorts =
  KernSpec ksorts ksyn kstartsyms ktopts kpris kres

getKernSyntax :: KernSpec -> Set.Set (Production KernelSort)
getKernSyntax = kernSyntax

setKernSyntax :: KernSpec -> Set.Set (Production KernelSort) -> KernSpec
setKernSyntax (KernSpec ksorts _ kstartsyms ktopts kpris kres) ksyn =  
  KernSpec ksorts ksyn kstartsyms ktopts kpris kres

getKernStartSymbols :: KernSpec -> Set.Set (KernelSort) 
getKernStartSymbols = kernStartSymbols

setKernStartSymbols :: KernSpec -> Set.Set (KernelSort) -> KernSpec
setKernStartSymbols (KernSpec ksorts ksyn _ ktopts kpris kres) kstartsyms = 
  KernSpec ksorts ksyn kstartsyms ktopts kpris kres

getKernTemplateOptions :: KernSpec -> Set.Set (TemplateOption KernelSort)
getKernTemplateOptions = kernTemplateOptions

setKernTemplateOptions :: KernSpec -> Set.Set (TemplateOption KernelSort) -> KernSpec
setKernTemplateOptions (KernSpec ksorts ksyn kstartsyms _ kpris kres) ktopts = 
  KernSpec ksorts ksyn kstartsyms ktopts kpris kres

getKernPriorities :: KernSpec -> Set.Set (Priority KernelSort)
getKernPriorities = kernPriorities

setKernPriorities :: KernSpec -> Set.Set (Priority KernelSort) -> KernSpec
setKernPriorities (KernSpec ksorts ksyn kstartsyms ktopts _ kres) kpris =
  KernSpec ksorts ksyn kstartsyms ktopts kpris kres

getKernRestrictions :: KernSpec -> Set.Set (Restriction KernelSort)
getKernRestrictions = kernRestrictions

setKernRestrictions :: KernSpec -> Set.Set (Restriction KernelSort) -> KernSpec
setKernRestrictions (KernSpec ksorts ksyn kstartsyms ktopts kpris _) kres = 
  KernSpec ksorts ksyn kstartsyms ktopts kpris kres
