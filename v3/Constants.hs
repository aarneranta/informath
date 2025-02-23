module Constants where

import UserConstants

constants = [
  ("Set", "Noun", "set_Noun"), 
  ("Type", "Noun", "type_Noun"), 
  ("Prop", "Noun", "proposition_Noun"),
  ("Elem", "Fun", "elements_Fun"),
  ("Proof", "Fun", "proofs_Fun"),
  ("false", "Name", "absurdity_Name"),
  ("and", "Fun", "conjunction_Fun"),
  ("or", "Fun", "disjunction_Fun"),
  ("if", "Fun", "implication_Fun"),
  ("forall", "Fun", "universal_Fun"),
  ("exists", "Fun", "existential_Fun"),
  ("not", "Fun", "negation_Fun"),
  ("iff", "Fun", "equivalence_Fun"),
  ("Number", "Noun", "number_Noun"), 
  ("Nat", "Set", "natural_Set"), 
  ("Int", "Set", "integer_Set"), 
  ("Rat", "Set", "rational_Set"), 
  ("Real", "Set", "real_Set"), 
  ("Complex", "Set", "complex_Set"),
  ("Eq", "Compar", "Eq_Compar"), 
  ("Lt", "Compar", "Lt_Compar"), 
  ("Gt", "Compar", "Gt_Compar"), 
  ("Neq", "Compar", "Neq_Compar"), 
  ("Leq", "Compar", "Leq_Compar"), 
  ("Geq", "Compar", "Geq_Compar"), 
  ("positive", "Adj", "positive_Adj"), 
  ("negative", "Adj", "negative_Adj"),  
  ("plus", "Oper", "plus_Oper"), 
  ("minus", "Oper", "minus_Oper"), 
  ("times", "Oper", "times_Oper"), 
  ("div", "Oper", "div_Oper"), 
  ("pow", "Oper", "pow_Oper"), 
  ("neg", "Oper", "neg_Oper"), 
  ("abs", "Fun", "absolute_value_Fun"), 
  ("factorial", "Fun", "factorial_Fun"), 
  ("gcd", "Fun", "gcd_Fun"), 
  ("even", "Adj", "even_Adj"), 
  ("odd", "Adj", "odd_Adj"), 
  ("prime", "Adj", "prime_Adj"),
  ("Div", "Rel", "divisible_Rel"),
  ("function", "Oper", "function_Oper"),
  ("union", "Oper", "union_Oper"),
  ("intersection", "Oper", "intersection_Oper"),
  ("difference", "Oper", "difference_Oper"),
  ("powerset", "Oper", "powerset_Oper")
  ]
  ++ userConstants
