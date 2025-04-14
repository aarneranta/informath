{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module SpecialDeduktiConversions where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import CommonConcepts
import ConstantData
import qualified Data.Map as M
import DeduktiOperations

specialDeduktiConversions = M.fromList [
  ("lean_encpi", piToFunction)
  ]


piToFunction :: Tree a -> Tree a
piToFunction t = case t of
  EApp _ _ -> case splitApp t of
    (EIdent (QIdent "enc.Pi"), [_, _, _, a, EAbs bind b]) -> EFun (mkHypo bind (piToFunction a)) (piToFunction b)
    (f, xs) -> foldl EApp (piToFunction f) (map piToFunction xs)
  _ -> composOp piToFunction t
 where
   mkHypo :: Bind -> Exp -> Hypo
   mkHypo bind a = case bind of
     BVar x -> HVarExp x a
     BTyped x _ -> HVarExp x a --- what if it differs from binding? shouldn't it be convertible?

