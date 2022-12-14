module Test.Fun where

import SDF3.Spec

import qualified Data.Set as Set

-- Module: lex

module_lex :: Spec
module_lex = Spec "lex" Set.empty $ Set.fromList
  [
    LexSorts       lexsorts,
    LexSyntax      lexsyn,
    LexRestriction lexRestrictions,
    CFRestriction  lexCFRestrictions
  ]
  
lexsorts :: Set.Set Sort
lexsorts = Set.fromList [SortLit "ID",
                         SortLit "INT",
                         SortLit "AST",
                         SortLit "COM",
                         SortLit "EOF"]
           
lexsyn :: Set.Set (Production Sort)
lexsyn = Set.fromList [lexProd (SortLit "ID")  idProd Set.empty,
                       lexProd (SortLit "INT") intProd Set.empty,
                       lexProd Layout whitespace Set.empty,
                       lexProd Layout (SortSym . SortLit $ "COM") Set.empty,
                       lexProd (SortLit "COM") comProd Set.empty,
                       lexProd (SortLit "AST") astProd Set.empty,
                       lexProd Layout layoutProd Set.empty,
                       lexProd (SortLit "EOF") (CCSym EmptyClass) Set.empty]
  where
    azAZ       = CCSym cc_azAZ
    azAZ09     = CCSym cc_azAZ09
    cc09       = CCSym cc_09
    whitespace = CCSym cc_ws    
    idProd     = Sequence azAZ (ListSym azAZ09 ZeroManyList)
    intProd    = Sequence (OptionalSym . CCSym . charCC $ "-")
                          (ListSym cc09 OneManyList)
    comProd    = (CCSym $ Concat (charCC "/") (charCC "*")) `Sequence`
                 (ListSym
                  ((CCSym . Complement . charCC $ "*") `Alternative`
                   (SortSym . SortLit $ "AST")        `Alternative`
                   (SortSym . SortLit $ "COM"))
                   ZeroManyList) `Sequence`
                 (CCSym $ Concat (charCC "*") (charCC "/"))
    astProd = CCSym . charCC $ "*"
    layoutProd =
      (CCSym $ (charCC "/") `Concat` (charCC "/")) `Sequence`
      (ListSym (CCSym . Complement $ cc_newlines) ZeroManyList) `Sequence`
      ((CCSym cc_newlines) `Alternative` (SortSym . SortLit $ "EOF"))

lexRestrictions :: Set.Set (Restriction Sort)
lexRestrictions = Set.fromList [Restrict (SortSym . SortLit $ "ID") (Set.singleton cc_azAZ09),
                                Restrict (SortSym . SortLit $ "INT") (Set.singleton cc_09),
                                Restrict (SortSym . SortLit $ "AST") (Set.singleton (charCC "/")),
                                Restrict (SortSym . SortLit $ "EOF") (Set.singleton (Complement EmptyClass))]

lexCFRestrictions :: Set.Set (Restriction Sort)
lexCFRestrictions = Set.fromList [Restrict (CCSym . charCC $ "-") (Set.singleton cc_09),
                                  Restrict (OptionalSym (SortSym Layout)) (Set.singleton cc_ws),
                                  Restrict (OptionalSym (SortSym Layout)) (Set.singleton $ (Concat (charCC "/") (charCC "/"))),
                                  Restrict (OptionalSym (SortSym Layout)) (Set.singleton $ (Concat (charCC "/") (charCC "*")))]

-- Module: fun

module_fun :: Spec
module_fun = Spec "fun" (Set.fromList [module_lex]) $ Set.fromList
  [
    CFSorts         cfsorts,
    CFStartSymbols  cfStartSymbols,
    CFSyntax        cfSyntax,
    CFPriorities    cfPriorities,
    TemplateOptions templateOptions
  ]

cfsorts :: Set.Set Sort
cfsorts = Set.fromList [SortLit "Exp",
                         SortLit "Case",
                         SortLit "Bnd",
                         SortLit "Pat"]
  
cfStartSymbols :: Set.Set Sort
cfStartSymbols = Set.fromList [SortLit "Exp"]                 

cfSyntax :: Set.Set (Production Sort)
cfSyntax = Set.fromList [  
  -- Exp.Int = INT
  Prod (SortLit "Exp") "Int"  (SortSym . SortLit $ "INT") Set.empty,
  -- Exp.Var = ID
  Prod (SortLit "Exp") "Var"  (SortSym . SortLit $ "ID") Set.empty,
  -- Pat.PVar = ID
  Prod (SortLit "Pat") "PVar" (SortSym . SortLit $ "ID") Set.empty,
  -- Exp = <(<Exp>)> {bracket}
  TemplateProd (SortLit "Exp") "" ((TLitSym "(") `TSequence`
                                   (TLitSort . SortLit $ "Exp") `TSequence`
                                   (TLitSym ")")) (Set.singleton Bracket),
  -- Exp = [(<Exp>)]
  TemplateProd (SortLit "Exp") "" ((TLitSym "-") `TSequence`
                                   (TLitSort . SortLit $ "Exp")) (Set.singleton Bracket),
  -- Exp.Add = [<Exp> + <Exp>]
  TemplateProd (SortLit "Exp") "Add" ((TLitSort . SortLit $ "Exp") `TSequence`
                                      (TLitSym "+") `TSequence`
                                      (TLitSort . SortLit $ "Exp")) (Set.singleton SDF3.Spec.Left),
  -- Exp.Sub = [<Exp> - <Exp>]
  TemplateProd (SortLit "Exp") "Sub" ((TLitSort . SortLit $ "Exp") `TSequence`
                                      (TLitSym "-") `TSequence`
                                      (TLitSort . SortLit $ "Exp")) (Set.singleton SDF3.Spec.Left),
  -- Exp.Add = [<Exp> == <Exp>]
  TemplateProd (SortLit "Exp") "Eq" ((TLitSort . SortLit $ "Exp") `TSequence`
                                     (TLitSym "==") `TSequence`
                                     (TLitSort . SortLit $ "Exp")) (Set.singleton SDF3.Spec.Left),
  -- Exp.Fun = [fun <ID*> -> <Exp>]
  TemplateProd (SortLit "Exp") "Fun" ((TLitSym "fun") `TSequence`
                                     (TListSym (TLitSort . SortLit $ "ID") "" ZeroManyList) `TSequence`
                                     (TLitSym "->") `TSequence`
                                     (TLitSort . SortLit $ "Exp")) Set.empty,
  -- Exp.App = [<Exp> <Exp>]
  TemplateProd (SortLit "Exp") "App" ((TLitSort . SortLit $ "Exp") `TSequence` (TLitSort . SortLit $ "Exp")) Set.empty,

  -- Exp.Let = [let <(Bnd "\n\n")+> in <Exp>]
  TemplateProd (SortLit "Exp") "Let" ((TLitSym "let") `TSequence`
                                      (TListSym (TLitSort . SortLit $ "Bnd") "\n\n" OneManyList) `TSequence`
                                      (TLitSym "in") `TSequence`
                                      (TLitSort . SortLit $ "Exp")) Set.empty,

  -- Exp.If = [if <Exp> then <Exp> else <Exp>]
  TemplateProd (SortLit "Exp") "If" ((TLitSym "if") `TSequence`
                                      (TLitSort . SortLit $ "Exp") `TSequence`
                                      (TLitSym "then") `TSequence`
                                      (TLitSort . SortLit $ "Exp") `TSequence`
                                      (TLitSym "else") `TSequence`
                                      (TLitSort . SortLit $ "Exp")) Set.empty,

  -- Exp.Match = [match <Exp> with <(Case "\n")+>]
  TemplateProd (SortLit "Exp") "Match" ((TLitSym "match") `TSequence`
                                        (TLitSort . SortLit $ "Exp") `TSequence`
                                        (TLitSym "with") `TSequence`
                                        (TListSym (TLitSort . SortLit $ "Case") "\n" OneManyList)) (Set.singleton LongestMatch),

  -- Bnd.Bnd = [<ID> = <Exp>]
  TemplateProd (SortLit "Bnd") "Bnd" ((TLitSort . SortLit $ "ID") `TSequence`
                                      (TLitSym "=") `TSequence`
                                      (TLitSort . SortLit $ "Exp")) Set.empty,

  -- Case.Case = [| <Pat> -> <Exp>]
  TemplateProd (SortLit "Case") "Case" ((TLitSym "|") `TSequence`
                                        (TLitSort . SortLit $ "Pat") `TSequence`
                                        (TLitSym "->") `TSequence`
                                        (TLitSort . SortLit $ "Exp")) Set.empty,

  -- Pat.PApp = [<Pat> <Pat>] {left}
  TemplateProd (SortLit "Pat") "PApp" ((TLitSort . SortLit $ "Pat") `TSequence` (TLitSort . SortLit $ "Pat")) (Set.singleton SDF3.Spec.Left),

  -- Pat = [(<Pat>)] {bracket}
  TemplateProd (SortLit "Pat") "" ((TLitSym "(") `TSequence`
                                   (TLitSort . SortLit $ "Pat") `TSequence`
                                   (TLitSym ")")) (Set.singleton Bracket)
  ]

cfPriorities :: Set.Set (Priority Sort)
cfPriorities = Set.fromList $ [TransP $ ProdRef (SortLit "Exp") "Min" `ContinueTP`
                                        (ProdRef (SortLit "Exp") "App" `ElementTP`
                                         ProdRef (SortLit "Exp") "Add"),
                                
                               TransP $ ProdRef (SortLit "Exp") "Min" `ElementTP`
                                        ProdRef (SortLit "Exp") "Sub",
                                
                               TransP $ ProdRef (SortLit "Exp") "App" `ElementTP`
                                        ProdRef (SortLit "Exp") "Sub",
                                
                               TransP $  ProdRef (SortLit "Exp") "Sub"   `ContinueTP`
                                        (ProdRef (SortLit "Exp") "Eq"    `ContinueTP`
                                        (ProdRef (SortLit "Exp") "IfE"   `ContinueTP`
                                        (ProdRef (SortLit "Exp") "IfT"   `ContinueTP`
                                        (ProdRef (SortLit "Exp") "Match" `ContinueTP`
                                        (ProdRef (SortLit "Exp") "Fun"   `ElementTP`
                                         ProdRef (SortLit "Exp") "Let"))))),

                               TransP $ ProdRef (SortLit "Exp") "Add" `ElementTP`
                                        ProdRef (SortLit "Exp") "Eq",
                                
                               TransP $ ProdRef (SortLit "Exp") "Add" `ElementTP`
                                        ProdRef (SortLit "Exp") "IfE",

                               TransP $ ProdRef (SortLit "Exp") "Add" `ElementTP`
                                        ProdRef (SortLit "Exp") "IfT",

                               TransP $ ProdRef (SortLit "Exp") "Add" `ElementTP`
                                        ProdRef (SortLit "Exp") "Match",

                               TransP $ ProdRef (SortLit "Exp") "Add" `ElementTP`
                                        ProdRef (SortLit "Exp") "Fun",

                               TransP $ ProdRef (SortLit "Exp") "Add" `ElementTP`
                                        ProdRef (SortLit "Exp") "Let",

                               IndexedTransP $ ElementITP (ProdRef (SortLit "Exp") "App") 1
                                                          (ProdRef (SortLit "Exp") "Min")
                               ]                                      

templateOptions :: Set.Set (TemplateOption Sort)
templateOptions = Set.fromList $ [KeywordReject (SortSym . SortLit $ "ID") Reject,
                                  Keyword . Set.singleton $ cc_azAZ09]
