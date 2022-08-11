module Test.Fun where

import SDF3.Spec

import qualified Data.Set as Set

lexsorts :: Set.Set Sort
lexsorts = Set.fromList $ [SortLit "ID",
                          SortLit "INT",
                          SortLit "AST",
                          SortLit "EOF"]
lexsyn :: Set.Set (Production Sort)
lexsyn = Set.fromList $ [lexProd (SortLit "ID")  idProd Set.empty,
                         lexProd (SortLit "INT") intProd Set.empty,
                         lexProd Layout whitespace Set.empty,
                         lexProd Layout ]
  where
    azAZ       = CCSym . Class $ ['a'..'z']++['A'..'Z']
    azAZ09     = CCSym . Class $ ['a'..'z']++['A'..'Z']++['0'..'9']
    cc09       = CCSym . Class $ ['0'..'9']
    whitespace = CCSym . Class $ [' ', '\t', '\n', '\r']
    idProd     = Sequence azAZ (ListSym azAZ09 ZeroManyList)
    intProd    = Sequence (OptionalSym . CCSym . Class $ "-")
                          (ListSym cc09 OneManyList)

cfsorts :: Set.Set Sort
cfsorts = Set.fromList $ [SortLit "Exp",
                          SortLit "Case",
                          SortLit "Bnd",
                          SortLit "Pat"]

