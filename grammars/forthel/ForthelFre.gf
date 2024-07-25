-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelFre of Forthel =

  ForthelTermsAscii,
  LatexTermsTex **
  ForthelFunctor - [
    pluralNP, AllTerm,
    PredicateSynonym, NamesAssumption, StatementAssumption
    ] with
    (Syntax=SyntaxFre),
    (Symbolic=SymbolicFre),
    (Extend=ExtendFre),
    (Grammar=GrammarFre)
---    (Markup=MarkupFre)

** open

  ParadigmsFre,
  (P=ParadigmsFre),
  (M=MakeStructuralFre),
  (R=ResFre),
  (I=IrregFre),
  Prelude
  
in {

-- functor exceptions
lin
  AllTerm notion = mkNP all_Predet (mkNP thePl_Det notion.cn) ;

  NamesAssumption names classnoun =
    mkPhr 
      (lets_Utt (mkVP assume_VS
         (mkS (mkCl (namesNP names) (mkCN classnoun.cn classnoun.adv))))) ;
      
  StatementAssumption stat =
    mkPhr 
      (lets_Utt (mkVP assume_VS stat)) ;
oper
  pluralNP : NP -> NP = \np -> np ** {n=plural} ;

-- words etc

  denote_V2 : V2 =
    mkV2 (mkV "signifier") ;

  any_Quant : Quant =
     let niq = "n'importe quel" in
     M.mkQuant niq (niq + "le") (niq + "s") (niq + "les") ;
  each_Det = every_Det ; 
  such_that_Subj = M.mkSubj "tel que" ; --- tel/tels/telle/telles

  iff_Conj = M.mkConj [] "si et seulement si" singular ;

  equal_A2 : A2 = mkA2 (mkA "égal") dative ;

  assume_VS : VS = mkVS (mkV "supposer") ;

  then_Adv : Adv = P.mkAdv "alors" ;

  let_Str : Str = "soit" ; --- seien ?
  
}
