concrete UserConstantsSwe of UserConstants = BaseConstantsSwe **

-- generated by MkConstants.hs

open
  UtilitiesSwe,
  SyntaxSwe,
  ParadigmsSwe,
  SymbolicSwe,
  (L=BaseConstantsLatex),
  Formal

in {
lin area_Fun = mkFun "area" ;
lin radius_Fun = mkFun "radie" ;
lin circle_Noun = mkNoun (mkN "cirkel" "cirklar") ;
lin pi_Const = mkConst "\\pi" (mkNP the_Det (mkCN tal_N (symb "\\(\\pi\\)"))) ;
lin legendre_symbol_Oper = mkOper L.legendre_symbol_Oper (mkN "Legendresymbol" "Legendresymboler") ;
lin square_Oper = mkOper L.square_Oper (mkN "kvadrat" "kvadrater") ;
lin resultant_Oper = mkOper L.plus_Oper (mkN "resultant" "resultanter") ;
lin perpendicular_Compar = mkCompar L.perpendicular_Compar "vinkelrät" "mot" ;
lin length_Oper = mkOper L.length_Oper (mkN "norm" "normer") ;
lin vector_Noun = mkNoun (mkN "vektor" "vektorer") ;
lin denumerable_Adj = mkAdj "upräknelig" ;
lin cardinality_Fun = mkFun (mkN "kardinalitet" "kardinaliteter") ;
lin is_root_Relnoun = mkN2 (mkN "rot" "rötter") ;
lin degree_Fun = mkFun (mkN "grad" "grader") ;
lin polynomial_Noun = mkNoun (mkN "polynom" "polynom") ;
lin irrational_Adj = mkAdj "irrationell" ;
lin rational_Adj = mkAdj "rationell" ;
}
