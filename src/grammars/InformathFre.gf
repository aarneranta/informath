concrete InformathFre of Informath =
  MathCoreFre **
  InformathFunctor - [postAdvS] with
    (Syntax = SyntaxFre),
    (Symbolic = SymbolicFre),
    (Grammar = GrammarFre)
  ** open
    Formal,
    Prelude,
    BaseConstantsLatex
in {

oper
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;

}