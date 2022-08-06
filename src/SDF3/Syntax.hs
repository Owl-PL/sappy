module SDF3.Syntax where

-- * Surface Specification

data Spec = Spec String [Spec] [Section]

data Section
  = LexSyntax       [Production Symbol]
  | CFSyntax        [Production Symbol]
  | LexStartSymbols [Symbol]
  | CFStartSymbols  [Symbol]
  | TemplateOptions [TemplateOption Symbol]
  | CFPriorities    [Priority Symbol]
  | LexRestriction  [Restriction Symbol]
  | CFRestriction   [Restriction Symbol]

data Production sym
  = Prod         sym            String [sym]            [Attribute]
  | TemplateProd TemplateSymbol String [TemplateSymbol] [Attribute]

data TemplateOption sym
  = Keyword  [CharClass]
  | Tokenize [Char]
  | AttrSym  sym Attribute

data Priority sym
  = TransPriority         [ProductionRef sym]     [ProductionRef sym]
  | NontransPriority      [ProductionRef sym]     [ProductionRef sym]
  | IndexTransPriority    (ProductionRef sym) Int (ProductionRef sym)
  | IndexNontransPriority (ProductionRef sym) Int (ProductionRef sym)  
  | AttrNontransPriority  (ProductionRef sym) Int (ProductionRef sym)
  | AttrTransPriority
    (Attribute, [ProductionRef sym])
    (Attribute, [ProductionRef sym])

data Restriction sym
  = Restrict sym Lookahead

data ProductionRef sym
  = ProdRef sym String
  
data Symbol
  = CCSym       CharClass
  | SortSym     Sort
  | OptionalSym Symbol
  | ListSym     Sort   String LMode
  | Sequence    Symbol Symbol
  | Alternative Symbol Symbol

data TemplateSymbol
  = TLitSort  Sort
  | TOptSort Sort
  | TListSort Sort String LMode
  | TLitSym  String
  | TSeqence TemplateSymbol TemplateSymbol

-- | A sort is some string, and can be made optional.
data Sort = SortLit String
          | Layout

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

