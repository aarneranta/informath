-- mathematical terms as they appear in "normal" mathematical text

abstract Terms = {

cat
  Formula ;
  Equation ;
  Eqsign ;
  Term ;
  [Term] {1} ;
  Ident ;
  Function ;

fun
  FEquation : Equation -> Formula ;
  FElem : [Term] -> Term -> Formula ;

  EChain : Eqsign -> Term -> Equation -> Equation ;
  EBinary : Eqsign -> Term -> Term -> Equation ;

  TParenth : Term -> Term ; -- extra parentheses

  TTimes : Term -> Term -> Term ;
  TNeg : Term -> Term ;
  TApp : Function -> [Term] -> Term ;

  TIdent : Ident -> Term ;
  TNumber : Int -> Term ; --- was float

  FIdent : Ident -> Function ;
  FDerivative : Function -> Function ;

  StrIdent : String -> Ident ;
  
  TFrac : Term -> Term -> Term ;
  TComprehension : Term -> Term -> Formula -> Term ;
  TPositive : Term -> Term ; -- R^+
  TNegative : Term -> Term ;

  TLog : Term -> Term -> Term ;
  
  TextbfTerm : Term -> Term ;
}
