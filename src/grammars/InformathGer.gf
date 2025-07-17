--# -path=.:present

concrete InformathGer of Informath =
  MathCoreGer **
  InformathFunctor - [postAdvS] with
    (Syntax = SyntaxGer),
    (Symbolic = SymbolicGer),
    (Grammar = GrammarGer),
    (Extend = ExtendGer)
  ** open
    ParadigmsGer,
    Formal,
    Prelude,
    BaseConstantsLatex
in {

oper
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;
  imply_V2 : V2 = mkV2 (mkV "implizieren") ;
  only_if_Subj : Subj = mkSubj "nur dann , wenn" ;

}
