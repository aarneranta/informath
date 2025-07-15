concrete MathCoreGer of MathCore =
  TermsLatex, 
  UserConstantsGer **
  MathCoreFunctor - [Pred3Adj] --- take a long time
    with
    (Syntax=SyntaxGer),
    (Grammar=GrammarGer),
    (Markup=MarkupGer),
    (Extend=ExtendGer),
    (Symbolic=SymbolicGer)
  ** open
    UtilitiesGer,
    Prelude,
    ParadigmsGer,
    (M=MakeStructuralGer),
    (I=IrregGer)

in {

oper

  define_V2 : V2 = mkV2 (mkV "definieren") ;
  assume_VS : VS = mkVS (mkV "an" I.nehmen_V) ;
  type_CN : CN = mkCN (mkN "typ") ;
  case_N : N = mkN "fall" ;
  contradiction_N : N = mkN "kontradiction" ;
  then_Adv : Adv = ParadigmsGer.mkAdv "så" ;
  thenText_Adv : Adv = ParadigmsGer.mkAdv "då" ;
  such_that_Subj : Subj = mkSubj "så att" ;
  applied_to_Prep : Prep = mkPrep "applicerat på" dative ;
  defined_as_Prep : Prep = mkPrep "definierat som" dative ;
  function_N : N = mkN "funktion" ;
  basic_type_CN : CN = mkCN (mkN "grundtyp") ;
  map_V3 = mkV3 (mkV "ab" (mkV "bilden")) (mkPrep accusative) as_Prep ;
  say_VS = mkVS (mkV "sagen") ;
  hold_V2 = mkV2 I.halten_V for_Prep ;
  arbitrary_A = mkA "godtycklig" ;
  set_N = mkN "mängd" ;

  iff_Subj : Subj = mkSubj "om och endast om" ;
  commaConj : Conj = M.mkConj "" "," plural ;

  basic_concept_Str = "grundbegrepp" ;
  by_cases_Str = "med fallanalys:" ;
  proof_Str = "bevis" ;
  axiom_Str = "axiom" ;
  theorem_Str = "teorem" ;
  definition_Str = "definition" ;

  instance_N = mkN "instans" ;
  prove_VS = mkVS (mkV "beweisen") ;
  
  as_Prep : Prep = mkPrep "som" dative ;

  let_Str : Bool => Str = \\_ => "låt" ;
  assuming_Str = "under följande antaganden:" ;

  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; ---- should be in RGL

}