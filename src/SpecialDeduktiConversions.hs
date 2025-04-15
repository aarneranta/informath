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
  ("initlean", convInitLean)
  ]


convInitLean :: Tree a -> Tree a
convInitLean t = case t of
  EApp _ _ -> case splitApp t of
    (EIdent (QIdent "enc.Sort"), _) -> EIdent (QIdent "Set")
    (EIdent (QIdent "enc.El"), [_, x]) -> convInitLean x
    (f@(EIdent (QIdent "Eq")), [_, _, x, y]) -> foldl EApp f (map convInitLean [x, y])
    (EIdent (QIdent "enc.Pi"), [_, _, _, a, EAbs bind b]) -> EFun (mkHypo bind (convInitLean a)) (convInitLean b)
    (f, xs) -> foldl EApp (convInitLean f) (map convInitLean xs)
  EFun (HVarExp _ (EIdent (QIdent "lvl.Lvl"))) body -> convInitLean body
  _ -> composOp convInitLean t
 where
   mkHypo :: Bind -> Exp -> Hypo
   mkHypo bind a = case bind of
     BVar x -> HVarExp x a
     BTyped x _ -> HVarExp x a --- what if it differs from binding? shouldn't it be convertible?

