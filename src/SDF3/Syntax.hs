-- | The SDF3 syntax is divided into two main parts:
--
--   1. The surface specification.
--
--   2. The kernel specification.
--
--   The first is the specification defined by the user while the second is
--   the normalized version of the surface specification.
module SDF3.Syntax where

-- * Surface Specification

-- | The surface specification consists of a module name, any imported
--   specifications and a list of sections.
data Spec
  = Spec String    -- ^ The module name.
         [Spec]    -- ^ All imported specifications.
         [Section] -- ^ The list of sections making up the specificaiton.

-- | Sections make up the entirity of the specification, and
--   consist of:
data Section
  = CFSorts         [Sort]                  -- ^ Context-free sorts (non-terminals).
  | LexSorts        [Sort]                  -- ^ Lexical syntax sorts (non-terminals).
  | LexSyntax       [Production Symbol]     -- ^ The lexical syntax.
  
  | CFSyntax        [Production Symbol]     -- ^ The context-free syntax.
  
  | LexStartSymbols [Symbol]                -- ^ The lexical start symbols
                                            --   (non-terminals).
  | CFStartSymbols  [Symbol]                -- ^ The context-free start symbols
                                            --   (non-terminals).
  | TemplateOptions [TemplateOption Symbol] -- ^ The set of template options.
  
  | CFPriorities    [Priority Symbol]       -- ^ The set of context-free priorities;
                                            --   used for disambiguation.
  | LexRestriction  [Restriction Symbol]    -- ^ Lexical restrictions;
                                            --   used for disambiguation.
  | CFRestriction   [Restriction Symbol]    -- ^ Context-free restrictions;
                                            --   used for disambiguation.

-- | Productions make up the lexical and context-free syntax sections,
--   and consist of:
data Production sym
    -- | An ordinary production of the form
    --      
    -- @sym.const = sym {attributes}@
    --
    -- where @sym@ is a word over 'SDF3.Symbol'.         
  = Prod Sort String sym [Attribute]
    -- | A template production of the form
    --      
    -- @sym.const = [w] {attributes}@
    --
    -- or
    --
    -- @sym.const = \<w\> {attributes}@
    -- 
    -- where @w@ is a word over 'SDF3.TemplateSymbol'.
  | TemplateProd Sort String TemplateSymbol [Attribute]

-- | Template options place restrictions on the lexical syntax.  They consist of:
data TemplateOption sym
  = Keyword  [CharClass]    -- ^ Used to setup follow restrictions on keywords.
  | Tokenize [Char]         -- ^ Specifies which characters have layout around them.
  | AttrSym  sym Attribute  -- ^ Mainly used to setup reject rules for keywords.

-- | Priorities are used to place weighted restrictions on productions
--   to prevent ambiguties; e.g., precedence.
data Priority sym
  = TransPriority         [ProductionRef sym]     [ProductionRef sym] -- ^ The transitive ordering.
  
  | NontransPriority      [ProductionRef sym]     [ProductionRef sym] -- ^ The non-transitive ordering.
  
  | IndexTransPriority    (ProductionRef sym) Int (ProductionRef sym) -- ^ The transitive ordering with an index.
  
  | IndexNontransPriority (ProductionRef sym) Int (ProductionRef sym) -- ^ The non-transitive ordering with an index.
  
  | AttrNontransPriority                                              -- ^ The transitive ordering with attribute labels.
    (Attribute, [ProductionRef sym])
    (Attribute, [ProductionRef sym])
    
  | AttrTransPriority                                                 -- ^ The non-transitive ordering with attribute labels.
    (Attribute, [ProductionRef sym])
    (Attribute, [ProductionRef sym])

-- | Restrictions filter applications of productions for certain
--   non-terminals (@sym@) if the following character, the lookahead,
--   is in a particular character class ('SDF3.Lookahead').
data Restriction sym
  = Restrict sym        -- ^ The symbol to restrict.
             Lookahead  -- ^ A character class

-- | A production reference is of the form:
--
--   @sym.const@
--
--  where @sym@ is a some non-terminal and @const@ is some constructor.
data ProductionRef sym
  = ProdRef sym       -- ^ Some non-terminal.
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

-- | A list of character classes that describe the lookahead symbol.
type Lookahead = [CharClass]

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
--   Therefore, a kernel specficiation is simply a list of kernel sections.
data KernSpec = KernSpec [KernSection]

-- | The kernel specification is made up of several sections similarly
--   to the surface specification, and consists of:
data KernSection
  = KernSorts             [KernelSort]                   -- ^ Set of kernel sorts (non-terminals).
  | KernSyntax            [Production KernelSymbol]      -- ^ The syntax (productions).
  | KernStartSymbols      [KernelSymbol]                 -- ^ The start symbols.
  | KernelTemplateOptions [TemplateOption KernelSymbol]  -- ^ The set of template options.
  | KernPriorities      [Priority KernelSymbol]          -- ^ The set of priorities; used for disambiguation.
  | KernRestrictions    [Restriction KernelSymbol]       -- ^ The set of restrictions; used for disambiguation.

-- | The set of kernel symbols, much like 'SDF3.Symbols', make up the
--   lexical strucutre of the kernel specification, but sorts are
--   marked as either lexical structure of context-free structure.
data KernelSymbol
  = KCCSym       CharClass                  -- ^ Character classes
  | KSortSym     KernelSort                 -- ^ Sorts (non-terminals)
  | KOptionalSym KernelSymbol               -- ^ Optional symbols (@sym?@)
  | KListSym   KernelSort   String LMode    -- ^ Lists of lexical symbols (@sym*@ or @sym+@)
  | KSequence    KernelSymbol KernelSymbol  -- ^ Sequences of symbols (@sym sym@)
  | KAlternative KernelSymbol KernelSymbol  -- ^ Alternative symbols (@sym | sym@)  

-- | Kernel sorts come in two flavors:
data KernelSort
  = KernCFSort String  -- ^ Context-free sorts.
  | KernLexSort String -- ^ Lexical sorts.
