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
  = LexSyntax       [Production Symbol]     -- ^ The lexical syntax.
  
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
    -- @sym.const = w {attributes}@
    --
    -- where @w@ is a word over 'SDF3.Symbol'.         
  = Prod sym String [sym] [Attribute]
    -- | A template production of the form
    --      
    -- @sym.const = [w] {attributes}@
    --
    -- or
    --
    -- @sym.const = \<w\> {attributes}@
    -- 
    -- where @w@ is a word over 'SDF3.TemplateSymbol'.
  | TemplateProd TemplateSymbol String [TemplateSymbol] [Attribute]

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

data TemplateSymbol
  = TLitSort  Sort
  | TOptSort Sort
  | TListSort Sort String LMode
  | TLitSym  String
  | TSeqence TemplateSymbol TemplateSymbol

-- | Sorts define the non-terminals of the specification.
data Sort = SortLit String  -- ^ A sort defined by the user.
          | Layout          -- ^ A reserved sort name used to indicate the
                            --   whitespace that can appear between
                            --   context-free symbols.

data CharClass
  = Class        [Char]
  | Complement   CharClass
  | Difference   CharClass CharClass
  | Union        CharClass CharClass
  | Intersection CharClass CharClass

-- | Language disamiguation attribtues.
data Attribute
  = Left            -- ^ Left associative.
  | Right           -- ^ Right associative.
  | Nonassoc        -- ^ Non-associative.
  | Assoc           -- ^ Associative.
  | Bracket         -- ^ Bracketing.
  | Reject          -- ^ Keyword reservation.

data LMode = ZeroManyList | OneManyList

type Lookahead = [CharClass]

-- * Kernel Specification

data KernSpec = KernSpec [KernSection]

data KernSection
  = KernSyntax            [Production KernelSymbol]
  | KernStartSymbols      [KernelSymbol]
  | KernelTemplateOptions [TemplateOption KernelSymbol]
  | KernCFPriorities      [Priority KernelSymbol]
  | KernLexRestriction    [Restriction KernelSymbol]
  | KernCFRestriction     [Restriction KernelSymbol]  

data KernelSymbol
  = CFSym  Symbol
  | LexSym Symbol

