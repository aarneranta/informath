{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Informath where

import Informath

import Data.List (nub, sortOn)
import Data.Char (isDigit)

type Opts = [String]

nlg :: Opts -> Tree a -> [Tree a]
nlg opts tree = case opts of
   _ | elem "-variations" opts ->
         nub $ concatMap variations [t, ut, ft, aft, iaft, viaft]
   _ -> [viaft]
 where
   t = unparenth tree
   ut = uncoerce t
   ft = formalize ut
   aft = aggregate (flatten ft)
   iaft = insitu aft
   viaft = varless iaft

unparenth :: Tree a -> Tree a
unparenth t = case t of
  GAndProp (GListProp props) -> GSimpleAndProp (GListProp (map unparenth props))
  GOrProp (GListProp props) -> GSimpleOrProp (GListProp (map unparenth props))
  GIfProp a b -> GSimpleIfProp (unparenth a) (unparenth b)
  GIffProp a b -> GSimpleIffProp (unparenth a) (unparenth b)
  _ -> composOp unparenth t

uncoerce :: Tree a -> Tree a
uncoerce t = case t of
  GProofProp prop -> uncoerce prop
  GElemKind kind -> uncoerce kind
  GCoercionExp coercion_ exp -> uncoerce exp
  _ -> composOp uncoerce t

formalize :: Tree a -> Tree a
formalize t = case t of
  GAdjProp (GComparAdj compar y) x -> case (getTerm x, getTerm y) of
    (Just tx, Just ty) ->
      GFormulaProp (GFEquation (GEBinary (GComparEqsign compar) tx ty))
    _ -> GAdjProp (GComparAdj compar (formalize y)) (formalize x)
  GComparnounProp compar x y -> case (getTerm x, getTerm y) of
    (Just tx, Just ty) ->
      GFormulaProp (GFEquation (GEBinary (GComparnounEqsign compar) tx ty))
  GOperListExp oper xy@(GAddExps x (GOneExps y)) -> case (getTerm x, getTerm y) of
    (Just tx, Just ty) -> GTermExp (GAppOperTerm oper tx ty)
    _ -> GOperListExp oper (formalize xy)
  GConstExp const -> GTermExp (GConstTerm const)
  GFunListExp f exps -> maybe t GTermExp (getTerm t) 
  GOperListExp f exps -> maybe t GTermExp (getTerm t) 
  GAppExp f exps -> maybe t GTermExp (getTerm t) 
  _ -> composOp formalize t

getTerm :: Tree a -> Maybe GTerm
getTerm t = case t of
  GConstExp const -> return (GConstTerm const)
----  GFunListExp (LexFun "successor_Fun") (GOneExps x) -> tryComputeSuccessor t
  GFunListExp fun (GOneExps x) -> do
    tx <- getTerm x
    case fun of
      _ -> Nothing
  GOperListExp oper (GOneExps x) -> do
    tx <- getTerm x
    case oper of
      LexOper "neg_Oper" -> return (GTNeg tx)
      _ -> return (GAppOperOneTerm oper tx)
  GOperListExp oper (GAddExps x (GOneExps y)) -> do
    tx <- getTerm x
    ty <- getTerm y
    case oper of
      LexOper "times_Oper" -> return (GTTimes tx ty)
      _ -> return (GAppOperTerm oper tx ty)
  GAppExp t@(GTermExp (GTIdent f)) exps -> case mapM getTerm (exps2list exps) of
    Just xs -> return (GTApp (GFIdent f) (GListTerm xs))
    _ -> Nothing
  GTermExp term -> return term
  _ -> Nothing

aggregate :: Tree a -> Tree a
aggregate t = case t of
  GNotProp prop -> case aggregate prop of
    GAdjProp adj x -> GNotAdjProp adj x
    aprop -> GNotProp aprop
  GSimpleAndProp (GListProp pp@(GAdjProp a x : props)) -> case getAdjs props x of
    Just adjs -> GAdjProp (GAndAdj (GListAdj (a:adjs))) x
    _ -> case getAdjArgs props a of
      Just exps -> GAdjProp a (GAndExp (GListExp (x:exps)))
      _ -> GSimpleAndProp (GListProp (map aggregate pp))
  GSimpleOrProp (GListProp pp@(GAdjProp a x : props)) -> case getAdjs props x of
    Just adjs -> GAdjProp (GOrAdj (GListAdj (a:adjs))) x
    _ -> case getAdjArgs props a of
      Just exps -> GAdjProp a (GOrExp (GListExp (x:exps)))
      _ -> GSimpleOrProp (GListProp (map aggregate pp))
  GListHypo hypos -> GListHypo (aggregateHypos hypos)
  _ -> composOp aggregate t
 where
   aggregateHypos hypos = case hypos of
     GPropHypo a : GPropHypo b : hs ->
       GPropHypo (aggregate (GSimpleAndProp (GListProp [a, b]))) : aggregateHypos hs
     h : hs -> aggregate h : aggregateHypos hs
     _ -> hypos

getAdjs :: [GProp] -> GExp -> Maybe [GAdj]
getAdjs props x = case props of
  GAdjProp adj y : pp | x == y -> do
    adjs <- getAdjs pp x
    return (adj : adjs)
  prop : _ -> Nothing
  _ -> return []

getAdjArgs :: [GProp] -> GAdj -> Maybe [GExp]
getAdjArgs props a = case props of
  GAdjProp b y : pp | a == b -> do
    exps <- getAdjArgs pp a
    return (y : exps)
  prop : _ -> Nothing
  _ -> return []

flatten :: Tree a -> Tree a
flatten t = case t of
  GSimpleAndProp (GListProp props) -> case getAndProps props of
    Just ps -> GSimpleAndProp (GListProp ps)
    _ -> GSimpleAndProp (GListProp (map flatten props))
  GSimpleOrProp (GListProp props) -> case getOrProps props of
    Just ps -> GSimpleOrProp (GListProp ps)
    _ -> GSimpleOrProp (GListProp (map flatten props))
  _ -> composOp flatten t

getAndProps :: [GProp] -> Maybe [GProp]
getAndProps props = case props of
  GSimpleAndProp (GListProp ps):qs -> do
    pss <- getAndProps ps
    qss <- getAndProps qs
    return (pss ++ qss)
  prop : qs -> do
    qss <- getAndProps qs
    return (prop : qss)
  _ -> return []

getOrProps :: [GProp] -> Maybe [GProp]
getOrProps props = case props of
  GSimpleOrProp (GListProp ps):qs -> do
    pss <- getOrProps ps
    qss <- getOrProps qs
    return (pss ++ qss)
  prop : qs -> do
    qss <- getOrProps qs
    return (prop : qss)
  _ -> return []


variations :: Tree a -> [Tree a]
variations tree = case tree of
  GAxiomJmt label (GListHypo hypos) prop -> 
    let splits = [splitAt i hypos | i <- [0..length hypos]]
    in [GAxiomJmt label (GListHypo hypos11) (hypoProp hypos2 prop2) |
          (hypos1, hypos2) <- splits,
	  hypos11 <- sequence (map variations hypos1),
	  prop2 <- variations prop]
  GVarsHypo (GListIdent xs) (GSetKind set) ->
    [tree, GLetFormulaHypo (GFElem (GListTerm [GTIdent x | x <- xs]) (GSetTerm set))]
  GAllProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- allExpVariations argkind]
  GExistProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- existExpVariations argkind]
  _ -> composOpM variations tree

allExpVariations :: GArgKind -> [GExp]
allExpVariations argkind = case argkind of
  GIdentsArgKind kind (GListIdent [x]) -> [GEveryIdentKindExp x kind , GAllArgKindExp argkind]
  _ -> [GAllArgKindExp argkind]

existExpVariations :: GArgKind -> [GExp]
existExpVariations argkind = case argkind of
----  GIdentsArgKind kind (GListIdent [x]) -> [GEveryIdentKindExp x kind, GSomeArgKindExp argkind]
  _ -> [GSomeArgKindExp argkind]

hypoProp :: [GHypo] -> GProp -> GProp
hypoProp hypos prop = case hypos of
  GPropHypo p : hs -> GSimpleIfProp p (hypoProp hs prop)
  GVarsHypo xs k : hs -> GAllProp (GListArgKind [GIdentsArgKind k xs]) (hypoProp hs prop)
  _ -> prop


---- a very simple special case of in situ so far
insitu :: Tree a -> Tree a
insitu t = case t of
  GAllProp (GListArgKind [argkind]) (GAdjProp adj exp) -> case subst argkind exp of
    True -> GAdjProp adj (GAllArgKindExp argkind)
    _ -> t
  GAllProp (GListArgKind [argkind]) (GNotAdjProp adj exp) -> case subst argkind exp of
    True -> GAdjProp adj (GNoArgKindExp argkind)
    _ -> t
  _ -> composOp insitu t

subst :: GArgKind -> GExp -> Bool
subst argkind exp = case (argkind, exp) of
  (GIdentsArgKind _ (GListIdent [x]), GTermExp (GTIdent y)) -> x == y
  _ -> False

varless :: Tree a -> Tree a
varless t = case t of
  GAllArgKindExp (GIdentsArgKind kind (GListIdent [_])) -> GEveryKindExp kind
  GNoArgKindExp (GIdentsArgKind kind (GListIdent [_])) -> GNoKindExp kind
  _ -> composOp varless t

exps2list :: GExps -> [GExp]
exps2list exps = case exps of
  GOneExps e -> [e]
  GAddExps e es -> e : exps2list es

