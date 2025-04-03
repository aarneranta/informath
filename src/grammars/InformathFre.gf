concrete InformathFre of Informath =
  MathCoreFre **
  InformathFunctor - [
    AllKindExp, AllIdentsKindExp,
    postAdvS
  ] with
    (Syntax = SyntaxFre),
    (Symbolic = SymbolicFre),
    (Grammar = GrammarFre)
  ** open
    Formal,
    Prelude,
    BaseConstantsLatex
in {

lin
  AllKindExp kind = mkNP all_Predet (mkNP thePl_Det (useKind kind)) ;
  AllIdentsKindExp idents kind = mkNP all_Predet (mkNP thePl_Det (mkCN (mkCN kind.cn idents.np) kind.adv)) ;

oper
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;

}