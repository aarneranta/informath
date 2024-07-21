-- based on http://nevidal.org/download/forthel.pdf

abstract Forthel = {

cat
  Statement ;
  Predicate ;
  Term ;
  Notion ;

  PrimaryStatement ;
  Terms ; -- subject of primary statement
  Predicates ;
  Notions ; -- complement of there exist(s)
  
  ClassNoun ;
  DefiniteNoun ;
  QuantifiedNotion ;
  QuantifiedNotions ;
  PlainTerm ;  -- no quantifiers
  Adjective ;  --- these two are one cat in 1.3.1
  Verb ;
  Operator ; -- symbolic
  Relation ;

  SymbTerm ;
  Name ;
  [Name] ; 

fun
-- 1.3.2
  set_Notion : [Name] -> Notion ; -- set (A, B, C)
  element_Notion : [Name] -> Term -> Notion ;
  function_Notion : [Name] -> Term -> Term -> Notion ;

  NameSymbTerm : Name -> SymbTerm ;
  StringName : String -> Name ;


-- 1.3.3
  EveryTerm : Notion -> QuantifiedNotion ;  --- can only take Sg
  EachTerm  : Notion -> QuantifiedNotion ;

  AllTerm   : Notion -> QuantifiedNotion ;  -- can take both Sg and Pl
  AnyTerm   : Notion -> QuantifiedNotion ;
  SomeTerm  : Notion -> QuantifiedNotion ;
  NoTerm    : Notion -> QuantifiedNotion ;

  OneQuantifiedNotions : QuantifiedNotion -> QuantifiedNotions ;
  AddQuantifiedNotions : QuantifiedNotion -> QuantifiedNotions -> QuantifiedNotions ;
  
  QuantifiedTerm : QuantifiedNotions -> Term ;

  PlainTermTerm : PlainTerm -> Term ; -- no quantifiers
  
  SymbPlainTerm : SymbTerm -> PlainTerm ;

  DefiniteSgNounTerm : DefiniteNoun -> PlainTerm ;
  DefinitePlNounTerm : DefiniteNoun -> PlainTerm ;

  zero_DefiniteNoun : DefiniteNoun ;
  order_DefiniteNoun : PlainTerm -> DefiniteNoun ;

-- 1.3.4
-- reading nonterminals doesPredicate etc as funs, not cats

  DoesPredicate : Verb -> Predicate ;
  IsPredicate : Adjective -> Predicate ;
  IsaPredicate : Notion -> Predicate ; --- not notion in doc 
  HasPredicate : Notion -> Predicate ; --- hence we may overgenerate
  HasNoPredicate : Notion -> Predicate ;

  converge_Verb : Verb ;
  divide_Verb : Term -> Verb ;   --- V2
  belong_Verb : Term -> Verb ;
  join_Verb : Term -> Term -> Verb ; --- V3

  prime_Adjective : Adjective ;
  dividing_Adjective : Term -> Adjective ; -- A2
  equal_Adjective : Term -> Adjective ;
  less_Adjective : Term -> Adjective ;

-- 1.3.5
--- intervening categories as functions again

  SimpleStatement : Terms -> Predicates -> PrimaryStatement ;

  ThereIsStatement : Notions -> PrimaryStatement ;
  ThereIsNoStatement : Notion -> PrimaryStatement ;

  PosOnePredicates : Predicate -> Predicates ;
  NegOnePredicates : Predicate -> Predicates ;
  PosAddPredicates : Predicate -> Predicates -> Predicates ;
  NegAddPredicates : Predicate -> Predicates -> Predicates ;

  OneTerms : Term -> Terms ;
  AddTerms : Term -> Terms -> Terms ;

  OneNotions : Notion -> Notions ;
  AddNotions : Notion -> Notions -> Notions ;

---- symbolic statements TODO

  PrimaryStatementStatement : PrimaryStatement -> Statement ;
  ForStatement : QuantifiedNotions -> Statement -> Statement ;

--- simplicied from spec, which uses many levels
--- to resolve ambiguities: that can be misleading to an innocent
--- reader (and the use of primaryStatement seems to compromise this)

  AndStatement : Statement -> Statement -> Statement ;
  OrStatement : Statement -> Statement -> Statement ;
  IfStatement : Statement -> Statement -> Statement ;
  IffStatement : Statement -> Statement -> Statement ;


}