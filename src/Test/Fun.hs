module Test.Fun where

import SDF3.Spec

import qualified Data.Set as Set

lexsorts :: Set.Set Sort
lexsorts = Set.fromList $ [SortLit "ID",
                           SortLit "INT",
                           SortLit "AST",
                           SortLit "COM",
                           SortLit "EOF"]
           
lexsyn :: Set.Set (Production Sort)
lexsyn = Set.fromList $ [lexProd (SortLit "ID")  idProd Set.empty,
                         lexProd (SortLit "INT") intProd Set.empty,
                         lexProd Layout whitespace Set.empty,
                         lexProd Layout (SortSym . SortLit $ "COM") Set.empty,
                         lexProd (SortLit "COM") comProd Set.empty,
                         lexProd (SortLit "AST") astProd Set.empty,
                         lexProd Layout layoutProd Set.empty,
                         lexProd (SortLit "EOF") (CCSym . Class $ []) Set.empty]
  where
    azAZ       = CCSym cc_azAZ
    azAZ09     = CCSym cc_azAZ09
    cc09       = CCSym cc_09
    whitespace = CCSym cc_ws    
    idProd     = Sequence azAZ (ListSym azAZ09 ZeroManyList)
    intProd    = Sequence (OptionalSym . CCSym . Class $ "-")
                          (ListSym cc09 OneManyList)
    comProd    = (CCSym . Class $ "/*") `Sequence`
                 (ListSym
                  ((CCSym . Complement . Class $ "*") `Alternative`
                   (SortSym . SortLit $ "AST")        `Alternative`
                   (SortSym . SortLit $ "COM"))
                   ZeroManyList) `Sequence`
                 (CCSym . Class $ "*/")
    astProd = CCSym . Class $ "*"
    layoutProd =
      (CCSym . Class $ "//") `Sequence`
      (ListSym (CCSym . Complement $ cc_newlines) ZeroManyList) `Sequence`
      ((CCSym cc_newlines) `Alternative` (SortSym . SortLit $ "EOF"))

lexRestrictions :: Set.Set (Restriction Sort)
lexRestrictions = Set.fromList $ [Restrict (SortLit "ID") (Set.singleton cc_azAZ09),
                                  Restrict (SortLit "INT") (Set.singleton cc_09),
                                  Restrict (SortLit "AST") (Set.singleton (Class "/")),
                                  Restrict (SortLit "EOF") (Set.singleton (Complement . Class $ []))]

cfsorts :: Set.Set Sort
cfsorts = Set.fromList $ [SortLit "Exp",
                          SortLit "Case",
                          SortLit "Bnd",
                          SortLit "Pat"]

