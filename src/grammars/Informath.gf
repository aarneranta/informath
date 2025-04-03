abstract Informath =
  MathCore
  ** {

flags startcat=Jmt ;

cat
  [Adj] {2} ;
  [Exp] {2} ;

fun
  FormulaProp : Formula -> Prop ;

  SetTerm : Set -> Term ;
  ConstTerm : Const -> Term ;
  ComparEqsign : Compar -> Eqsign ;
  ComparnounEqsign : Comparnoun -> Eqsign ;
  AppOperTerm : Oper -> Term -> Term -> Term ;
  AppOperOneTerm : Oper -> Term -> Term ;

-- to remove parentheses around complex propositions
  SimpleAndProp : [Prop] -> Prop ;
  SimpleOrProp : [Prop] -> Prop ;
  SimpleIfProp : Prop -> Prop -> Prop ;
  SimpleIffProp : Prop -> Prop -> Prop ;

  AndAdj : [Adj] -> Adj ;
  OrAdj : [Adj] -> Adj ;

  AndExp : [Exp] -> Exp ;
  OrExp : [Exp] -> Exp ;

-- for indexed parsing (terms in $...$ stored in a dictionary)

  IndexedTermExp : Int -> Exp ;
  IndexedFormulaProp : Int -> Prop ;
  IndexedLetFormulaHypo : Int -> Hypo ;

-- for Pathak's examples

  LetFormulaHypo : Formula -> Hypo ;
  PostQuantProp : Prop -> Exp -> Prop ;

  DefinedAdjJmt : Label -> [Hypo] -> Exp -> Adj -> Prop -> Jmt ;
  WeDefineAdjJmt : Label -> [Hypo] -> Exp -> Adj -> Prop -> Jmt ;

  AdjKind : Adj -> Kind -> Kind ;
  KindProp : Exp -> Kind -> Prop ;

  AllKindExp : Kind -> Exp ;
  AllIdentsKindExp : [Ident] -> Kind -> Exp ;
  EveryKindExp : Kind -> Exp ;
  EveryIdentKindExp : Ident -> Kind -> Exp ;
  
  SomeKindExp : Kind -> Exp ;
  SomeIdentsKindExp : [Ident] -> Kind -> Exp ;
  IndefKindExp : Kind -> Exp ;
  IndefIdentKindExp : Ident -> Kind -> Exp ;
  
  NoIdentsKindExp : [Ident] -> Kind -> Exp ;
  NoKindExp : Kind -> Exp ;

}