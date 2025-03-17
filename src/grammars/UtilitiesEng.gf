resource UtilitiesEng =

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {
oper
  RelationT : Type = {ap : AP ; prep : Prep} ;
  FunctionT : Type = {cn : CN ; prep : Prep} ;
  ConstantT : Type = {np : NP ; c : Str} ;
  OperatorT : Type = L.OperT ** {f : FunctionT} ; 
  ComparisonT : Type = {rel : RelationT ; op :  Str} ;
  SetT : Type = {cn : CN ; c : Str} ;
  LabelT = {np : NP ; isEmpty : Bool} ;
  ComparnounT = {cn : CN ; prep : Prep ; op : Str} ;

  mkNoun = overload {
    mkNoun : Str -> CN
      = \s -> mkCN (mkN s) ;
    mkNoun : Str -> Str -> CN
      = \a, n -> mkCN (mkA a) (mkN n) ;
    } ;
    
  mkSet = overload {
    mkSet : Str -> Str -> SetT
      = \c, s -> {cn = mkCN (mkN s) ; c = c} ;
    mkSet : Str -> Str -> Str -> SetT
      = \c, a, n -> {cn = mkCN (mkA a) (mkN n) ; c = c} ;
    } ;
    
  mkFun = overload {
    mkFun : Str -> FunctionT
      = \s -> {cn = mkCN (mkN s) ; prep = possess_Prep} ;
    mkFun : (a, n : Str) -> FunctionT
      = \a, n -> {cn = mkCN (mkA a) (mkN n) ; prep = possess_Prep} ;
    mkFun2 : (a, b, n : Str) -> FunctionT
      = \a, b, n -> {cn = mkCN (mkA a) (mkCN (mkA b) (mkN n)) ; prep = possess_Prep} ;
    } ;
    
  mkAdj = overload {
    mkAdj : Str -> AP
      = \s -> mkAP (mkA s) ;
    } ;
    
  mkRel = overload {
    mkRel : Str -> Str -> {ap : AP ; prep : Prep}
      = \s, p -> {ap = mkAP (mkA s) ; prep = mkPrep p}
    } ;
    
  mkName = overload {
    mkName : Str -> NP
      = \s -> mkNP (mkPN s)
    } ;

  mkLabel = overload {
    mkLabel : Str -> LabelT
      = \s -> {np = mkNP (mkPN s) ; isEmpty = False}
    } ;

  mkConst = overload {
    mkConst : Str -> Str -> ConstantT
      = \c, w -> {np = mkName w ; c = c} ;
    mkConst : Str -> NP -> ConstantT
      = \c, np -> {np = np ; c = c} ;
    } ;
    
  mkOper = overload {
    mkOper : L.OperT -> Str -> OperatorT
      = \op, w -> op ** {f = mkFun w} ; -- lowest Prec
    mkOper : Str -> Str -> OperatorT
      = \w, c -> L.mkOper c ** {f = mkFun w} ; -- lowest Prec
    mkOper : Str -> Str -> Prec -> OperatorT
      = \w, c, p -> L.mkOper c p ** {f = mkFun w}
    } ;

  mkCompar = overload {
    mkCompar : Str -> Str -> Str -> ComparisonT
      = \op, s, p -> {rel = mkRel s p ; op = op} ;
    } ;

  mkComparnoun = overload {
    mkComparnoun : Str -> Str -> ComparnounT
      = \op, s -> {cn = mkCN (mkN s) ; prep = possess_Prep ; op = op} ;
    mkComparnoun : Str -> CN -> ComparnounT
      = \op, cn -> {cn = cn ; prep = possess_Prep ; op = op} ;
    mkComparnoun : Str -> CN -> Prep -> ComparnounT
      = \op, cn, prep -> {cn = cn ; prep = prep ; op = op} ;
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;
}