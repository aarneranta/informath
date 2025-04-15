concrete UserConstantsFre of UserConstants = BaseConstantsFre **

-- generated by MkConstants.hs

open
  UtilitiesFre,
  SyntaxFre,
  ParadigmsFre,
  SymbolicFre,
  (L=BaseConstantsLatex),
  Formal

in {
lin area_Fun = mkFun "aire" ;
lin radius_Fun = mkFun "rayon" ;
lin circle_Noun = mkNoun (mkN "cercle" masculine) ;
lin pi_Const = mkConst "\\pi" (mkNP the_Det (mkCN nombre_N (symb "\\(\\pi\\)"))) ;
lin legendre_symbol_Oper = mkOper L.legendre_symbol_Oper (mkCN (mkN "symbole" masculine) (SyntaxFre.mkAdv genitive (mkNP (mkPN "Legendre")))) ;
lin square_Oper = mkOper L.square_Oper (mkN "carré" masculine) ;
lin resultant_Oper = mkOper L.plus_Oper (mkN "addition") ;
lin perpendicular_Compar = mkCompar L.perpendicular_Compar (mkA "perpendiculaire") dative ;
lin length_Oper = mkOper L.length_Oper "norme" ;
lin vector_Noun = mkNoun "vecteur" ;
lin denumerable_Adj = mkAdj "dénombrable" ;
lin cardinality_Oper = mkOper L.absolute_value_Oper "cardinalité" ;
lin is_root_Relnoun = mkN2 (mkN "racine") genitive ;
lin degree_Fun = mkFun (mkN "degré" masculine) ;
lin polynomial_Noun = mkNoun (mkN "polynôme" masculine) ;
lin irrational_Adj = mkAdj "irrationnel" ;
lin rational_Adj = mkAdj "rationnel" ;
}
