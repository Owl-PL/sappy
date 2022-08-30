{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-- | The SDF3 syntax is divided into two main parts:
--
--   1. The surface specification.
--
--   2. The kernel specification.
--
--   The first is the specification defined by the user while the second is
--   the normalized version of the surface specification.
module SDF3.Spec where

import Utils

import qualified Data.Set as Set

-- * Surface Specification

-- | The surface specification consists of a module name, any imported
--   specifications and a set of sections.
data Spec
  = Spec { moduleName :: String,         -- ^ The module name.
           imports    :: Set.Set Spec,   -- ^ All imported specifications.
           sections   :: Set.Set Section -- ^ The set of sections making up the specification.
         }
  deriving (Ord,Eq)

instance (Show Spec) where
  show (Spec modName imps secs) | null imps =
       "module "++modName++" where\n"
    ++ (setToSepString "\n\n" secs)++"\n"
  show (Spec modName imps secs) | otherwise =
       (setToSepString "\n" imps)
    ++ "\nmodule "++modName++" where\n"
    ++ (indexedOp (\x y -> "import "++x++"\n"++y) "" moduleName imps)
    ++ (setToSepString "\n\n" secs)++"\n"

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
  | LexSyntax       (Set.Set (Production Sort))      -- ^ The lexical syntax.
  
  | CFSyntax        (Set.Set (Production Sort))      -- ^ The context-free syntax.
  
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
  deriving (Ord,Eq)                                  

instance (Show Section) where
  show (CFSorts         sorts) = "context-free sorts: "           ++ (setToCommaSepString sorts)
  show (LexSorts        sorts) = "lexical sorts: "                ++ (setToCommaSepString sorts)
  show (LexSyntax       syn)   = "lexical syntax:\n  "            ++ (setToSepString "\n  " syn)
  show (CFSyntax        syn)   = "context-free syntax:\n  "       ++ (setToSepString "\n  " syn)
  show (LexStartSymbols sorts) = "lexical start-symbols: "        ++ (setToCommaSepString sorts)
  show (CFStartSymbols  sorts) = "context-free start-symbols: "   ++ (setToCommaSepString sorts)
  show (TemplateOptions opts)  = "template options:\n  "          ++ (setToSepString "\n  " opts)
  show (CFPriorities    pris)  = "context-free priorities:\n  "   ++ (setToSepString "\n  " pris)
  show (LexRestriction  rest)  = "lexical restrictions:\n  "      ++ (setToSepString "\n  " rest)
  show (CFRestriction   rest)  = "context-free restrictions:\n  " ++ (setToSepString "\n  " rest)  
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
    -- where @w@ is a word over 'SDF3.TemplateSymbol'.
  | TemplateProd sort String (TemplateSymbol sort) (Set.Set Attribute)
  deriving (Eq,Ord)

instance Show sort => Show (Production sort) where
  show (Prod sort const sym atts)          = (show sort)++(showConst const)++" -> "++(show sym)++" "++(showSetNoEmpty atts)
  show (TemplateProd sort const tsym atts) = (show sort)++(showConst const)++" -> ["++(show tsym)++"] "++(showSetNoEmpty atts)

showConst :: String -> String
showConst "" = ""
showConst const = "."++const

lexProd :: sort -> (Symbol sort) -> Set.Set Attribute -> Production sort
lexProd sort = Prod sort ""

-- | Template options place restrictions on the lexical syntax.  They consist of:
data TemplateOption sort
  = Keyword  (Set.Set (CharClass Char))     -- ^ Used to setup follow restrictions on keywords.
  | Tokenize [Char]                         -- ^ Specifies which characters have layout around them.
  | KeywordReject (Symbol sort) Attribute   -- ^ Mainly used to setup reject rules for keywords.
  deriving (Eq,Ord)

instance Show sort => Show (TemplateOption sort) where
  show (Keyword ccs) = "keyword -/- "++(foldr (\cc r -> (show cc)++" "++r) "" ccs)
  show (Tokenize cc) = "tokenize: "++(show cc)
  show (KeywordReject sym att) = (show sym)++" = keyword {"++(show att)++"}"

-- | Priorities are used to place weighted restrictions on productions
--   to prevent ambiguties; e.g., precedence.
data Priority sort
  = TransP (TransPriority sort)
  | IndexedTransP (IndexedTransPriority sort)
  | NonTransP (NonTransPriority sort)
  deriving (Eq,Ord)

instance Show sort => Show (Priority sort) where
  show (TransP tp) = show tp
  show (IndexedTransP itp) = show itp
  show (NonTransP ntp) = show ntp

data TransPriority sort
  = ElementTP  (ProductionRef sort) (ProductionRef sort)  -- ^ The transitive ordering on elements.
  
  | ContinueTP (ProductionRef sort) (TransPriority sort)  -- ^ The transitive ordering.    
  deriving (Eq,Ord)

instance Show sort => Show (TransPriority sort) where
  show (ElementTP  p1 p2)  = (show p1) ++ " > " ++ (show p2)
  show (ContinueTP p next) = (show p)  ++ " > " ++ (show next)

data IndexedTransPriority sort
  = ElementITP (ProductionRef sort) Int (ProductionRef sort)         -- ^ The transitive ordering with an index on elements.

  | ContinueITP (ProductionRef sort) Int (IndexedTransPriority sort) -- ^ The transitive ordering with an index.  
  deriving (Eq,Ord)

instance Show sort => Show (IndexedTransPriority sort) where
  show (ElementITP  p1 i p2)  = (show p1) ++ " " ++ (show i) ++ " .> " ++ (show p2)
  show (ContinueITP p i next) = (show p)  ++ " " ++ (show i) ++ " .> " ++ (show next)

data NonTransPriority sort
  = ElementNTP  (ProductionRef sort) (ProductionRef sort)     -- ^ The non-transitive ordering on elements.
  
  | ElementINTP (ProductionRef sort) Int (ProductionRef sort) -- ^ The non-transitive ordering with an index on elements.
  deriving (Eq,Ord)

instance Show sort => Show (NonTransPriority sort) where
  show (ElementNTP p1 p2) = (show p1) ++ " [>] " ++ (show p2)
  show (ElementINTP p1 i p2) = (show p1) ++ " " ++ (show i) ++ " [.>] " ++ (show p2)

-- | Restrictions filter applications of productions for certain
--   non-terminals (@sort@) if the following character, the lookahead,
--   is in a particular character class ('SDF3.Lookahead').
data Restriction sort
  = Restrict (Symbol sort)  -- ^ The symbol to restrict.
             Lookahead      -- ^ A character class
  deriving (Eq,Ord)

instance Show sort => (Show (Restriction sort)) where
  show (Restrict sym lh) | Set.size lh == 1 = (show sym) ++ " -/- " ++ (show . head . Set.toList $ lh)
  show (Restrict sym lh) | otherwise        = (show sym) ++ " -/- " ++ (showSet lh)

-- | A production reference is of the form:
--
--   @sym.const@
--
--  where @sym@ is a some non-terminal and @const@ is some constructor.
data ProductionRef sort
  = ProdRef sort      -- ^ Some non-terminal.
            String    -- ^ Some constructor of @sym@.
  deriving (Eq,Ord)

instance Show sort => Show (ProductionRef sort) where
  show (ProdRef sort const) = (show sort) ++ "." ++ const

-- | Symbols are the basic lexical structure of surface
--   specifications.  They consist of:
data Symbol sort
  = CCSym       (CharClass Char)             -- ^ Character classes
  | SortSym     sort                         -- ^ Sorts (non-terminals)
  | OptionalSym (Symbol sort)                -- ^ Optional symbols (@sym?@)
  | ListSym     (Symbol sort) LMode          -- ^ Lists of symbols (@sym*@ or @sym+@)
  | Sequence    (Symbol sort) (Symbol sort)  -- ^ Sequences of symbols (@sym sym@)
  | Alternative (Symbol sort) (Symbol sort)  -- ^ Alternative symbols (@sym | sym@)
  deriving (Eq,Ord)

instance Show sort => Show (Symbol sort) where
  show (CCSym cc)                 = show cc
  show (SortSym sort)             = show sort
  show (OptionalSym sym)          = (show sym)++"?"
  show (ListSym (sym@(CCSym (CharClass _))) ZeroManyList) = (show sym) ++ "*"
  show (ListSym sym ZeroManyList) = "(" ++ (show sym) ++ ")*"
  show (ListSym (sym@(CCSym (CharClass _))) OneManyList) = (show sym) ++ "+"
  show (ListSym sym OneManyList)  = "(" ++ (show sym) ++ ")+"
  show (Sequence sym1 sym2)      = (show sym1) ++  " " ++ (show sym2)
  show (Alternative sym1 sym2)      = (show sym1) ++  " | " ++ (show sym2)

-- | Template symbols are the basic lexical structure of template
--   productions in the surface specification.  They consist of:
data TemplateSymbol sort
  = TLitSym   String                                       -- ^ Literal symbols (strings)
  | TLitSort  sort                                         -- ^ Sorts (non-terminals)
  | TOptSort  sort                                         -- ^ Optional sorts (@sym?@)
  
  -- | Lists of symbols (@sym*@ or @sym+@), and list of symbols with a separator (@[sym sep]*@ or @[sym sep]+@)
  | TListSym
    (TemplateSymbol sort) -- ^ The symbol.
    String                -- ^ The separator.
    LMode                 -- ^ The mode of the list.
  | TSequence (TemplateSymbol sort) (TemplateSymbol sort)  -- ^ Sequences of symbols (@sym sym@)
  deriving (Eq,Ord)

instance Show sort => Show (TemplateSymbol sort) where
  show (TLitSym  s)                    = s
  show (TLitSort sort)                 = "<"++(show sort)++">"
  show (TOptSort sort)                 = (show sort) ++ "?"
  show (TListSym sym ""  ZeroManyList) = "(" ++ (show sym) ++ ")*"
  show (TListSym sym sep ZeroManyList) = "(" ++ (show sym) ++ " " ++ sep ++ ")*"
  show (TListSym sym ""  OneManyList)  = "(" ++ (show sym) ++ ")+"
  show (TListSym sym sep OneManyList)  = "(" ++ (show sym) ++ " " ++ sep ++ ")+"
  show (TSequence sym1 sym2)           = (show sym1) ++ " " ++ (show sym2)

-- | Sorts define the non-terminals of the specification.
data Sort = SortLit String  -- ^ A sort defined by the user.
          | Layout          -- ^ A reserved sort name used to indicate the
                            --   whitespace that can appear between
                            --   context-free symbols.
  deriving (Eq,Ord)

instance (Show Sort) where
  show (SortLit sort) = sort
  show (Layout)       = "LAYOUT"

-- * Character Classes.

-- | Character classes describe sets of lexical characters.  The
--   first, `CharClassRep` is essentially the alphabet of the
--   character class including character ranges.  
data CharClassRep rep
  = AlphaChar rep                                    -- ^ A character; e.g., @a@, @b@, etc.
  | RangeChar rep rep                                -- ^ A character range; e.g, @a-z@, @A-Z@, etc.  
  deriving (Eq,Ord)

instance (Show (CharClassRep Char)) where
  show (AlphaChar c)         = tail . init . show $ c
  show (RangeChar start end) = [start]++"-"++[end]

-- | `CharClass` is the second component of character classes which
-- consists of the actual character classs, and operations on
-- character classes.
data CharClass rep
  = EmptyClass                                        -- ^ The empty character class.
  | CharClass    [CharClassRep rep]                   -- ^ A character class.
  | Complement   (CharClass rep)                      -- ^ The complement of a character class.
  | Difference   (CharClass rep) (CharClass rep)      -- ^ The difference of two character classes.
  | Intersection (CharClass rep) (CharClass rep)      -- ^ The intersection of two character classes.
  | Concat       (CharClass rep) (CharClass rep)      -- ^ Concatenation of character classes.
  deriving (Eq,Ord)

instance (Show (CharClass Char)) where
  show EmptyClass             = "[]"
  show (CharClass cc)         = "["++(listToSepString "" cc)++"]"
  show (Complement cc)        = "~"++(show cc)
  show (Difference cc1 cc2)   = (show cc1) ++ " - " ++ (show cc2)
  show (Intersection cc1 cc2) = (show cc1) ++ " /\\ " ++ (show cc2)
  show (Concat (CharClass [AlphaChar c1]) (CharClass [AlphaChar c2])) = show [c1,c2]
  show (Concat cc1 cc2)       = (show cc1) ++ (show cc2)

rangeClass :: rep -> rep -> CharClass rep
rangeClass start end = CharClass [RangeChar start end]

charCC :: [Char] -> CharClass Char
charCC l = CharClass $ map AlphaChar l

cc_az :: CharClass Char
cc_az = rangeClass 'a' 'z'

cc_AZ :: CharClass Char
cc_AZ = rangeClass 'A' 'Z'

cc_09 :: CharClass Char
cc_09 = rangeClass '0' '9'

cc_azAZ :: CharClass Char
cc_azAZ = CharClass [RangeChar 'a' 'z', RangeChar 'A' 'Z']

cc_azAZ09 :: CharClass Char
cc_azAZ09 = CharClass [RangeChar 'a' 'z', RangeChar 'A' 'Z', RangeChar '0' '9']

cc_newlines :: CharClass Char
cc_newlines = CharClass [AlphaChar '\n', AlphaChar '\r']

cc_ws :: CharClass Char
cc_ws = CharClass [AlphaChar ' ', AlphaChar '\t', AlphaChar '\n', AlphaChar '\r']

-- | Attributes describe restrictions on productinos. They consist of:
data Attribute
  = Left            -- ^ Left associative.
  | Right           -- ^ Right associative.
  | Nonassoc        -- ^ Non-associative.
  | Assoc           -- ^ Associative.
  | Bracket         -- ^ Bracketing.
  | Reject          -- ^ Keyword reservation.
  | LongestMatch    -- ^ Longest Match.
  deriving (Eq,Ord)

instance (Show Attribute) where
  show SDF3.Spec.Left  = "left"
  show SDF3.Spec.Right = "right"
  show Nonassoc        = "non-associative"
  show Assoc           = "associative"
  show Bracket         = "bracket"
  show Reject          = "reject"
  show LongestMatch    = "longest-match"

-- | A set of character classes that describe the lookahead symbol.
type Lookahead = Set.Set (CharClass Char)

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
