-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelPor of Forthel =

  ForthelTermsAscii,
  LatexTermsTex **
  ForthelFunctor - [
    pluralNP, AllTerm, ThereIsNoStatement,
    PredicateSynonym, NamesAssumption, StatementAssumption
    ] with
    (Syntax=SyntaxPor),
    (Symbolic=SymbolicPor),
    (Extend=ExtendPor),
    (Grammar=GrammarPor)
---    (Markup=MarkupPor)

** open

  ParadigmsPor,
  (P=ParadigmsPor),
  (M=MakeStructuralPor),
  (R=ResPor),
  (I=IrregPor),
  (X=ExtraPor),
  Prelude
  
in {

-- functor exceptions
lin
  AllTerm notion = mkNP all_Predet (mkNP thePl_Det notion.cn) ;

  ThereIsNoStatement notion = mkS negativePol (Extend.ExistsNP (mkNP no_Quant notion.cn)) ;

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
    mkV2 (mkV "denotar") ;

  any_Quant : Quant = a_Quant ; ----
  each_Det = every_Det ; 
  such_that_Subj = M.mkSubjSubj "tal que" ; --- tel/tels/telle/telles

  iff_Conj = M.mkConj [] "se e só se" singular ;

  equal_A2 : A2 = mkA2 (mkA "igual") dative ;

  assume_VS : VS = mkVS (mkV "admitar") ; --- assumir ?

  then_Adv : Adv = P.mkAdv "então" ;

  let_Str : Str = "seja" ; --- pl ?
  
}
