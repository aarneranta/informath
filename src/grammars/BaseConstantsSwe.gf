concrete BaseConstantsSwe of BaseConstants =

open
  UtilitiesSwe,
  SyntaxSwe,
  ParadigmsSwe,
  SymbolicSwe,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {

lincat
  Noun = CN ;
  Fam = FamilyT ;
  Set = SetT ;
  Adj = AP ;
  Verb = VP ;
  Reladj = RelationT ;
  Relverb = V2 ;
  Relnoun = N2 ;
  Name = NP ;
  Fun = FunctionT ;
  Label = LabelT ;
  Const = ConstantT ;
  Oper = OperatorT ;
  Compar = ComparisonT ;
  Comparnoun = ComparnounT ;

lin
  type_Noun = mkNoun (mkN "typ" "typer") ;
  set_Noun = mkNoun (mkN "mängd" "mängder") ;
  proposition_Noun = mkNoun "påstående" ;

  elements_Fun = mkFun "elementtyp" ;
  proofs_Fun = mkFun  "bevistyp" ;

  absurdity_Name = mkName (mkN "kontradiktion" "kontradiktioner") ;
  conjunction_Fun = mkFun (mkN "konjunktion" "konjunktioner") ;
  disjunction_Fun = mkFun (mkN "disjunktion" "disjunktioner") ;
  implication_Fun = mkFun (mkN "implikation" "inmplikationer") ;
  universal_Fun = mkFun "universal" "kvantifikation" ; ---- plural
  existential_Fun = mkFun "existentiell" "kvantifikation" ;
  negation_Fun = mkFun (mkN "negation" "negationer") ;
  equivalence_Fun = mkFun (mkN "ekvivalens" "ekvivalenser") ;

  number_Noun = mkNoun tal_N ;
  boolean_Noun = mkNoun "sanningsvärde" ;
  list_Fam = mkFam "lista" ;
  natural_Set = mkSet L.natural_Set "naturlig" tal_N ;
  integer_Set = mkSet L.integer_Set (mkN "hel" tal_N) ;
  rational_Set = mkSet L.rational_Set "rationell" tal_N ;
  real_Set = mkSet L.real_Set "reell" tal_N ;
  complex_Set = mkSet L.complex_Set "komplex" tal_N ;

  Eq_Compar = mkCompar L.Eq_Compar "lika" "med" ;
  Lt_Compar = mkCompar L.Lt_Compar "mindre" "än" ; 
  Gt_Compar = mkCompar L.Gt_Compar "större" "än" ; 
  Neq_Compar = mkCompar L.Neq_Compar "inte lika" "med" ; ---- 
  Leq_Compar = mkCompar L.Leq_Compar "mindre än eller lika" "med" ; 
  Geq_Compar =  mkCompar L.Geq_Compar "större än eller lika" "med" ;

  positive_Adj = mkAdj "positiv" ;
  negative_Adj = mkAdj "negativ" ;

  converge_Verb = mkVP (mkV "konvergera") ;
  divide_Relverb = mkV2 "dela" ;
  member_Relnoun = mkN2 (mkN "element" "element") ;
  divisor_Relnoun = mkN2 (mkN "delare") ;

  plus_Oper = mkOper L.plus_Oper "summa" ;
  minus_Oper = mkOper L.minus_Oper (mkN "skillnad" "skillnader") (mkPrep "mellan") ;
  times_Oper = mkOper L.times_Oper "produkt" ;
  div_Oper = mkOper L.div_Oper "kvot" ;
  pow_Oper = mkOper L.pow_Oper "potens" ; ----
  neg_Oper = mkOper L.neg_Oper "negation" ;
  logarithm_Oper = mkOper L.logarithm_Oper "logaritm" ;
  square_root_Oper = mkOper L.square_root_Oper "kvadratrot" ;

  successor_Fun = mkFun (mkN "efterföljare" neutrum) ;
  absolute_value_Oper = mkOper L.absolute_value_Oper (mkN "absolutbelopp" neutrum) ;
  factorial_Oper = mkOper L.factorial_Oper "fakultet" ;
  gcd_Fun = mkFun "störst" "gemensam" "delare" ;

  even_Adj = mkAdj "jämn" ;
  odd_Adj = mkAdj "udda" ;
  divisible_Reladj = mkRel "delbar" "med" ;
  prime_Adj = mkAdj "prim" ;

  function_Fam = mkFam (mkCN (mkN "funktion" "funktioner")) from_Prep to_Prep ;
  union_Oper = mkOper L.union_Oper "union" ;
  intersection_Oper = mkOper L.intersection_Oper (mkN "snitt" "snittet") ;
  complement_Oper = mkOper L.complement_Oper (mkN "komplement" "komplement") ;
  cartesian_Oper = mkOper L.cartesian_Oper (mkCN (mkA "kartesisk") (mkN "produkt")) ;
  difference_Oper = mkOper L.difference_Oper (mkN "differens") (mkPrep "mellan") ;
  powerset_Oper = mkOper L.powerset_Oper "potensmängd" ;

  subset_Comparnoun = mkComparnoun L.subset_Comparnoun (mkCN (mkA "äkta") (mkN "del" mängd_N)) ;  
  subseteq_Comparnoun = mkComparnoun L.subseteq_Comparnoun (mkN "del" mängd_N) ;  
  superset_Comparnoun = mkComparnoun L.superset_Comparnoun (mkCN (mkA "äkta") (mkN "över" mängd_N)) ;  
  superseteq_Comparnoun = mkComparnoun L.superseteq_Comparnoun (mkN "över" mängd_N) ;  
  equalset_Compar = mkCompar L.equalset_Compar "lika" "med" ;
  notequalset_Compar = mkCompar L.notequalset_Compar "inte lika" "med" ; ----
  element_Comparnoun = mkComparnoun L.element_Comparnoun element_N ;
  notelement_Comparnoun = mkComparnoun L.notelement_Comparnoun (mkN "icke-" element_N) ; ----

  emptyset_Const = mkConst L.emptyset_Const (mkNP the_Det (mkCN (mkA "tom" "tomt" "tomma" "tommare" "tommast") mängd_N)) ;
  universeset_Const = mkConst L.universeset_Const (mkNP the_Det (mkCN (mkA "universell") mängd_N)) ;


}