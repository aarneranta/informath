concrete InformathSwe of Informath =
  MathCoreSwe **
  InformathFunctor - [postAdvS] with
    (Syntax = SyntaxSwe),
    (Symbolic = SymbolicSwe),
    (Grammar = GrammarSwe),
    (Extend = ExtendSwe)
  ** open
    Formal,
    Prelude,
    BaseConstantsLatex
in {

oper
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;

}