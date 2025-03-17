resource BaseConstantsLatex = 

open
  Formal,
  Prelude

in {

oper
  natural_Set = "N" ;
  integer_Set = "Z";
  rational_Set = "Q";
  real_Set = "R";
  complex_Set = "C";

  Eq_Compar = "=" ;
  Lt_Compar = "<" ;
  Gt_Compar = ">" ;
  Neq_Compar = "\\neq" ;
  Leq_Compar = "\\leq" ;
  Geq_Compar = "\\geq" ;
  
  plus_Oper : OperT = mkOper "+" <1 : Prec> ;
  minus_Oper : OperT = mkOper "-" <1 : Prec> ; 
  times_Oper : OperT = mkOper "\\times" <2 : Prec> ;
  div_Oper : OperT = mkOper "\\div" <2 : Prec> ; ---
  pow_Oper : OperT = mkOper "" "^{" "}" <2 : Prec> ; ---
  neg_Oper : OperT = mkOper "\\negated" ;
  logarithm_Oper : OperT = mkOper "\\log_" "{" "}" <3 : Prec> ;
  square_root_Oper : OperT = mkOper "\\sqrt{" "" "}" <4 : Prec> ;
  factorial_Oper : OperT = mkOper "" "" "\\^{!}" <3 : Prec> ;
  absolute_value_Oper : OperT = mkOper "\\abs{" "" "}" <4 : Prec> ;

  function_Oper : OperT = mkOper "\\rightarrow" ; ---
  union_Oper : OperT = mkOper "\\cup" ;
  intersection_Oper : OperT = mkOper "\\cap" ;
  cartesian_Oper : OperT = mkOper "\\times" ;
  difference_Oper : OperT = mkOper "\\setminus" ;
  complement_Oper : OperT = mkOper "\\complement" ;
  powerset_Oper : OperT = mkOper "\\wp" ;

  subset_Comparnoun = "\\subset" ;  
  subseteq_Comparnoun = "\\subseteq" ;
  superset_Comparnoun = "\\superset" ;  
  superseteq_Comparnoun = "\\superseteq" ;  
  equalset_Compar = "=" ;
  notequalset_Compar = "\\neq" ;
  element_Comparnoun = "\\in" ;  
  notelement_Comparnoun = "\\notin" ;

  positivePart : OperT = mkOper "" "" "\\^{+}" <4 : Prec> ;
  negativePart : OperT = mkOper "" "" "\\^{-}" <4 : Prec> ;

oper
  OperT : Type = {begin, op, end : Str ; p : Prec} ;
  
  mkOper = overload {
    mkOper : Str -> OperT
      = \c -> {begin, end = "" ; op = c ; p = 0} ; -- lowest Prec
    mkOper : Str -> Prec -> OperT
      = \c, p -> {begin, end = "" ; op = c ; p = p} ;
    mkOper : (beg, op, end : Str) -> Prec -> OperT
      = \beg, op, end , p -> {begin = beg ;  end = end ; op = op ; p = p}
    } ;

}