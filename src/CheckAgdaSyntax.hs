{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- check syntax errors in Agda judgements line by line
-- usage: runghc CheckAgdaSyntax.hs <exx.agda | grep ERROR

module CheckAgdaSyntax where

import Agda.AbsAgda
import Agda.ParAgda
import Agda.PrintAgda
import Agda.ErrM

main = interact (unlines . map parseAgdaJmt . filter (not . null) . lines)

parseAgdaJmt :: String -> String
parseAgdaJmt s = do
  case pJmt (myLexer (s ++ " ;")) of
    Bad e -> "ERROR: " ++ s
    Ok mo -> "OK: " ++ s ++ "\n" ++ normalize mo ++ " -- NORMALIZED"

normalize :: Jmt -> String
normalize = printTree . norm where

  norm :: Tree a -> Tree a
  norm t = case t of
    ECrossU x y -> app "times" (norm x) (norm y)
    ECrossI x y -> app "times" (norm x) (norm y)
    ELeqI x y -> app "Leq" (norm x) (norm y)
    EEqI x y -> app "Eq" (norm x) (norm y)
    EEEqI x y -> app "Eq" (norm x) (norm y)
    EAndI x y -> app "and" (norm x) (norm y)
    EOrI x y -> app "or" (norm x) (norm y)
    _ -> composOp norm t

  app s x y = EApp (EApp (EIdent (AIdent s)) x) y  

