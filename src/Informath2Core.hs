{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Informath2Core where

import Informath

data SEnv = SEnv {varlist :: [String]}
initSEnv = SEnv {varlist = []}

semantics :: Tree a -> Tree a
semantics = addCoercions . addParenth . sem initSEnv . removeFonts

addCoercions :: Tree a -> Tree a
addCoercions t = case t of
  GAxiomJmt label hypos prop -> GAxiomJmt label (addCoercions hypos) (proofProp prop)
  {-
  AxiomPropJmt : Label -> ListHypo -> Prop -> Jmt ;
  DefPropJmt : Label -> ListHypo -> Prop -> Prop -> Jmt ;
  SuchThatKind : Ident -> Kind -> Prop -> Kind ;
  ThmJmt : Label -> ListHypo -> Prop -> Proof -> Jmt ;
  AxiomExpJmt : Label -> ListHypo -> Exp -> Kind -> Jmt ;
  AxiomKindJmt : Label -> ListHypo -> Kind -> Jmt ;
  DefExpJmt : Label -> ListHypo -> Exp -> Kind -> Exp -> Jmt ;
  DefKindJmt : Label -> ListHypo -> Kind -> Kind -> Jmt ;
  ElemKind : Kind -> Kind ;
  IdentsArgKind : Kind -> ListIdent -> ArgKind ;
  KindArgKind : Kind -> ArgKind ;
  TypedExp : Exp -> Kind -> Exp ;
  -}
  GVarsHypo idents kind -> GVarsHypo idents (GElemKind kind)
  _ -> composOp addCoercions t
 where
   proofProp prop = case prop of
     GProofProp _ -> prop
     _ -> GProofProp prop

removeFonts :: Tree a -> Tree a
removeFonts t = case t of
  GTextbfTerm term -> removeFonts term
  _ -> composOp removeFonts t

addParenth :: Tree a -> Tree a
addParenth t = case t of
  GSimpleAndProp (GListProp props) -> GAndProp (GListProp (map addParenth props))
  GSimpleOrProp (GListProp props) -> GOrProp (GListProp (map addParenth props))
  GSimpleIfProp a b -> GIfProp (addParenth a) (addParenth b)
  GSimpleIffProp a b -> GIffProp (addParenth a) (addParenth b)
  _ -> composOp addParenth t
  
sem :: SEnv -> Tree a -> Tree a
sem env t = case t of

  GLetFormulaHypo formula -> case (sem env formula) of
    GFElem (GListTerm terms) (GSetTerm set) ->
      GVarsHypo (GListIdent [x | GTIdent x <- terms]) (GSetKind set) ---- TODO: check that all terms are idents

    _ -> GPropHypo (sem env (GFormulaProp (sem env formula)))

  GSimpleIfProp cond@(GFormulaProp (GFElem (GListTerm terms) (GSetTerm set))) prop ->
    case getJustVarsFromTerms env terms of
      Just xs -> sem env (GAllProp (GListArgKind [GIdentsArgKind (GSetKind set) (GListIdent xs)]) prop)
      _ ->  GSimpleIfProp (sem env cond) (sem env prop)
      
  GSimpleIfProp cond@(GKindProp exp kind) prop -> case getJustVars env exp of
    Just xs -> sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent xs)]) prop)
    _ -> GSimpleIfProp (sem env cond) (sem env prop)
    
  GSimpleIfProp cond prop -> case getAndProps cond of
    Just props -> sem env (foldr (\a b -> GSimpleIfProp a b) prop props)
    _ -> GSimpleIfProp (sem env cond) (sem env prop)

  GPostQuantProp prop exp -> case exp of
    GEveryIdentKindExp ident kind ->
      sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [ident])]) prop)
    GIndefIdentKindExp ident kind ->
      sem env (GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent [ident])]) prop)
    GSomeArgKindExp argkind ->
      sem env (GExistProp (GListArgKind [argkind]) prop)
    GAllArgKindExp argkind ->
      sem env (GAllProp (GListArgKind [argkind]) prop)

  GAllProp argkinds prop -> case argkinds of
    GListArgKind [GIdentsArgKind (GAdjKind adj kind) vars@(GListIdent xs)] ->
      GAllProp
        (GListArgKind [GIdentsArgKind kind vars])
        (GSimpleIfProp
	  (sem env (mkAndProp [GAdjProp adj (GTermExp (GTIdent x)) | x <- xs]))
	  (sem env prop))
    akinds -> GAllProp akinds (sem env prop)
    
  GKindProp exp (GAdjKind adj kind_) ->
    sem env (GAdjProp adj exp) --- ignoring kind, if not in hypothesis position

  GAdjKind adj kind ->
    let (var, nenv) = newVar env
    in GSuchThatKind var (sem nenv kind) (sem nenv (GAdjProp adj (GTermExp (GTIdent var))))

  GAdjProp (GAndAdj (GListAdj adjs)) x ->
    let sx = sem env x
    in GAndProp (GListProp [GAdjProp adj sx | adj <- adjs])
  GAdjProp (GOrAdj (GListAdj adjs)) x ->
    let sx = sem env x
    in GOrProp (GListProp [GAdjProp adj sx | adj <- adjs])
  GAdjProp a (GAndExp (GListExp exps)) ->
    let sa = sem env a
    in GAndProp (GListProp [GAdjProp sa exp | exp <- exps])
  GAdjProp a (GOrExp (GListExp exps)) ->
    let sa = sem env a
    in GOrProp (GListProp [GAdjProp sa exp | exp <- exps])
  GNotAdjProp adj exp -> GNotProp (sem env (GAdjProp adj exp))

  GFormulaProp (GFEquation (GEBinary (GComparEqsign compar) term1 term2)) ->
    GAdjProp (GComparAdj compar (sem env (GTermExp term2))) (sem env (GTermExp term1))

  GFormulaProp (GFEquation equation@(GEChain _ _ _)) -> case chainedEquations equation of
    triples -> GAndProp (GListProp
      [sem env (GFormulaProp (GFEquation (GEBinary eqsign x y))) | (eqsign, x, y) <- triples])
    
  GTermExp (GConstTerm const) -> GConstExp const
  GTermExp (GAppOperTerm oper x y) ->
    GOperListExp oper (GAddExps (sem env (GTermExp x)) (GOneExps (sem env (GTermExp y))))
  GTermExp (GAppOperOneTerm (LexOper "minus_Oper") x) ->  ---- should not be needed
    GOperListExp (LexOper "neg_Oper") (GOneExps (sem env (GTermExp x)))
  GTermExp (GAppOperOneTerm oper x) ->
    GOperListExp oper (GOneExps (sem env (GTermExp x)))
  GTermExp (GTTimes x y) -> sem env (GTermExp (GAppOperTerm (LexOper "times_Oper") x y))
  GTermExp (GTFrac x y) -> sem env (GTermExp (GAppOperTerm (LexOper "div_Oper") x y))
  GTermExp (GTNeg x) ->  sem env (GTermExp (GAppOperOneTerm (LexOper "neg_Oper") x))
  GTParenth term -> sem env term
      
  _ -> composOp (sem env) t

chainedEquations :: GEquation -> [(GEqsign, GTerm, GTerm)]
chainedEquations equation = case equation of
  GEChain eqsign term equ ->
    let triples@((_, x, _):_) = chainedEquations equ
    in (eqsign, term, x) : triples
  GEBinary eqsign term1 term2 ->
    [(eqsign, term1, term2)]

{-
ifs2hypos :: [GHypo] -> GProp -> ([GHypo], GProp)
ifs2hypos hs prop = case prop of
  GSimpleIfProp p q -> 
-}


hypoVars :: GHypo -> [GIdent]
hypoVars hypo = case hypo of
  GVarsHypo (GListIdent idents) _ -> idents
  _ -> []

newVar :: SEnv -> (GIdent, SEnv)
newVar env = (GStrIdent (GString "h_"), env) ---- TODO fresh variables

-- identify exp lists that are just variable lists, possibly bindings
getJustVars :: SEnv -> GExp -> Maybe [GIdent]
getJustVars env exp = case exp of
  GTermExp (GTIdent x) -> Just [x]
  GAndExp (GListExp exps) -> do
    xss <- mapM (getJustVars env) exps
    return (concat xss)
  _ -> Nothing

getJustVarsFromTerms :: SEnv -> [GTerm] -> Maybe [GIdent]
getJustVarsFromTerms env terms = case terms of
  GTIdent x : ts ->  do
    xs <- getJustVarsFromTerms env ts
    return (x : xs)
  [] -> return []
  _ -> Nothing

mkAndProp :: [GProp] -> GProp
mkAndProp props = case props of
  [prop] -> prop
  _ -> GSimpleAndProp (GListProp props)


getAndProps :: GProp -> Maybe [GProp]
getAndProps prop = case prop of
  GSimpleAndProp (GListProp props) -> Just props
  _ -> Nothing


