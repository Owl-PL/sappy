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
    azAZ       = CCSym . Class $ ['a'..'z']++['A'..'Z']
    azAZ09     = CCSym . Class $ ['a'..'z']++['A'..'Z']++['0'..'9']
    cc09       = CCSym . Class $ ['0'..'9']
    whitespace = CCSym . Class $ [' ', '\t', '\n', '\r']
    newlinesCC = Class $ "\n\r"
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
      (ListSym (CCSym . Complement $ newlinesCC) ZeroManyList) `Sequence`
      ((CCSym newlinesCC) `Alternative` (SortSym . SortLit $ "EOF"))

cfsorts :: Set.Set Sort
cfsorts = Set.fromList $ [SortLit "Exp",
                          SortLit "Case",
                          SortLit "Bnd",
                          SortLit "Pat"]

