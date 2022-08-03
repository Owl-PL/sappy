module SDF3.Syntax where

-- | A sort is some string, and can be made optional.
data Sort = SortLit String
          | Layout

data Symbol
  = CCSym       CharClass
  | SortSym     Sort
  | OptionalSym Symbol
  | ListSym     Sort   String LMode
  | Sequence    Symbol Symbol
  | Alternative Symbol Symbol

data KernelSymbol
  = CFSym  Symbol
  | LexSym Symbol

data TemplateSymbol
  = TLitSort  Sort
  | TOptSort Sort
  | TListSort Sort String LMode
  | TLitSym  String
  | TSeqence TemplateSymbol TemplateSymbol

data TemplateOption sym
  = Keyword  [CharClass]
  | Tokenize [Char]
  | AttrSym  sym Attribute

data Production sym
  = Prod         sym String [sym] [Attribute]
  | TemplateProd TemplateSymbol String [TemplateSymbol] [Attribute]

data ProductionRef sym
  = ProdRef sym String

data Priority sym
  = TransPriority        [ProductionRef sym]   [ProductionRef sym]
  | NontransPriority     [ProductionRef sym]   [ProductionRef sym]
  | IndexTransPriority    (ProductionRef sym) Int (ProductionRef sym)
  | IndexNontransPriority (ProductionRef sym) Int (ProductionRef sym)
  | AttrTransPriority     (Attribute, [ProductionRef sym]) (Attribute, [ProductionRef sym])
  | AttrNontransPriority  (ProductionRef sym) Int (ProductionRef sym)

type Lookahead = [CharClass]

data Restriction sym
  = Restrict sym Lookahead

data Section
  = LexSyntax       [Production Symbol]
  | CFSyntax        [Production Symbol]
  | LexStartSymbols [Symbol]
  | CFStartSymbols  [Symbol]
  | TemplateOptions [TemplateOption Symbol]
  | CFPriorities    [Priority Symbol]
  | LexRestriction  [Restriction Symbol]
  | CFRestriction   [Restriction Symbol]

data KernSection
  = KernSyntax        [Production KernelSymbol]
  | KernStartSymbols [KernelSymbol]
  | KernelTemplateOptions [TemplateOption KernelSymbol]
  | KernCFPriorities [Priority KernelSymbol]
  | KernLexRestriction [Restriction KernelSymbol]
  | KernCFRestriction  [Restriction KernelSymbol]  

data Spec = Spec String [String] [Section]

data KernSpec = KernSpec [KernSection]

data CharClass
  = Class        [Char]
  | Complement   CharClass
  | Difference   CharClass CharClass
  | Union        CharClass CharClass
  | Intersection CharClass CharClass

data LMode = ZeroManyList | OneManyList

-- | Language disamiguation attribtues.
data Attribute
  = Left            -- ^ Left associative.
  | Right           -- ^ Right associative.
  | Nonassoc        -- ^ Non-associative.
  | Assoc           -- ^ Associative.
  | Bracket         -- ^ Bracketing.
  | Reject          -- ^ Keyword reservation.

