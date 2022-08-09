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
  | LexSyntax       (Set.Set (Production Symbol))    -- ^ The lexical syntax.
  
  | CFSyntax        (Set.Set (Production Symbol))    -- ^ The context-free syntax.
  
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
data Production sym
    -- | An ordinary production of the form
    --      
    -- @sym.const = sym {attributes}@
    --
    -- where @sym@ is a word over 'SDF3.Symbol'.         
  = Prod Sort String sym (Set.Set Attribute)
    -- | A template production of the form
    --      
    -- @sym.const = [w] {attributes}@
    --
    -- or
    --
    -- @sym.const = \<w\> {attributes}@
    -- 
    -- where @w@ is a word over 'SDF3.TemplateSymbol'.
  | TemplateProd Sort String TemplateSymbol (Set.Set Attribute)

-- | Template options place restrictions on the lexical syntax.  They consist of:
data TemplateOption sort
  = Keyword  (Set.Set CharClass)  -- ^ Used to setup follow restrictions on keywords.
  | Tokenize [Char]               -- ^ Specifies which characters have layout around them.
  | AttrSym  sort Attribute       -- ^ Mainly used to setup reject rules for keywords.

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

-- | Restrictions filter applications of productions for certain
--   non-terminals (@sort@) if the following character, the lookahead,
--   is in a particular character class ('SDF3.Lookahead').
data Restriction sort
  = Restrict sort        -- ^ The symbol to restrict.
             Lookahead   -- ^ A character class

-- | A production reference is of the form:
--
--   @sym.const@
--
--  where @sym@ is a some non-terminal and @const@ is some constructor.
data ProductionRef sort
  = ProdRef sort      -- ^ Some non-terminal.
            String    -- ^ Some constructor of @sym@.

-- | Symbols are the basic lexical structure of surface
--   specifications.  They consist of:
data Symbol
  = CCSym       CharClass            -- ^ Character classes
  | SortSym     Sort                 -- ^ Sorts (non-terminals)
  | OptionalSym Symbol               -- ^ Optional symbols (@sym?@)
  | ListSym     Sort   String LMode  -- ^ Lists of symbols (@sym*@ or @sym+@)
  | Sequence    Symbol Symbol        -- ^ Sequences of symbols (@sym sym@)
  | Alternative Symbol Symbol        -- ^ Alternative symbols (@sym | sym@)
  deriving (Eq,Ord)

-- | Template symbols are the basic lexical structure of template
--   productions in the surface specification.  They consist of:
data TemplateSymbol 
  = TLitSym  String                         -- ^ Literal symbols (strings)
  | TLitSort  Sort                          -- ^ Sorts (non-terminals)
  | TOptSort Sort                           -- ^ Optional sorts (@sym?@)
  | TListSort Sort String LMode             -- ^ Lists of symbols (@sym*@ or @sym+@)  
  | TSeqence TemplateSymbol TemplateSymbol  -- ^ Sequences of symbols (@sym sym@)

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
--   label indicating whether or not they are context free syntax of
--   lexical syntax.
--
--   Therefore, a kernel specficiation is simply a set of kernel sections.
data KernSpec = KernSpec (Set.Set (KernSection))

-- | The kernel specification is made up of several sections similarly
--   to the surface specification, and consists of:
data KernSection
  = KernSorts             (Set.Set (KernelSort))                   -- ^ Set of kernel sorts (non-terminals).
  | KernSyntax            (Set.Set (Production KernelSymbol))      -- ^ The syntax (productions).
  | KernStartSymbols      (Set.Set (KernelSort))                   -- ^ The start symbols.
  | KernelTemplateOptions (Set.Set (TemplateOption KernelSort))    -- ^ The set of template options.
  | KernPriorities        (Set.Set (Priority KernelSort))          -- ^ The set of priorities; used for disambiguation.
  | KernRestrictions      (Set.Set (Restriction KernelSort))       -- ^ The set of restrictions; used for disambiguation.

-- | The set of kernel symbols, much like 'SDF3.Symbols', make up the
--   lexical strucutre of the kernel specification, but sorts are
--   marked as either lexical structure of context-free structure.
data KernelSymbol
  = KCCSym       CharClass                  -- ^ Character classes
  | KSortSym     KernelSort                 -- ^ Sorts (non-terminals)
  | KOptionalSym KernelSymbol               -- ^ Optional symbols (@sym?@)
  | KListSym     KernelSort   String LMode  -- ^ Lists of lexical symbols (@sym*@ or @sym+@)
  | KSequence    KernelSymbol KernelSymbol  -- ^ Sequences of symbols (@sym sym@)
  | KAlternative KernelSymbol KernelSymbol  -- ^ Alternative symbols (@sym | sym@)  

-- | Kernel sorts come in two flavors:
data KernelSort
  = KernCFSort String  -- ^ Context-free sorts.
  | KernLexSort String -- ^ Lexical sorts.
