module Ranking (rankTreesAndStrings) where

import PGF
import Lexing
import Data.List (sortOn)

-- ranking trees and their linearizations with a set of scores

data Scores = Scores {
  tree_length :: Int,
  tree_depth :: Int,
  characters :: Int,
  tokens :: Int,
  subsequent_dollars :: Int,
  initial_dollars :: Int,
  parses :: Int
  } deriving Show

scoreString :: String -> Scores
scoreString s = Scores {
  tree_length = 0, -- computed separately
  tree_depth = 0,  -- 
  characters = length s,
  tokens = length toks,
  subsequent_dollars = subdollars toks,
  initial_dollars = initdollars toks + if head toks == "$" then 1 else 0,
  parses = 1 ---- TODO
  }
 where
   toks = words (lextex s)
   subdollars ts = case ts of
     "$":"$":tt -> 1 + subdollars tt
     "$":",":"$":tt -> 1 + subdollars tt
     "$":".":"$":tt -> 1 + subdollars tt
     _:tt -> subdollars tt
     _ -> 0
   initdollars ts = case ts of
     ".":"$":tt -> 1 + initdollars tt
     _:tt -> initdollars tt
     _ -> 0

treeLength :: Expr -> Int
treeLength t = case unApp t of
  Just (f, ts@(_:_)) -> 1 + sum (map treeLength ts)
  _ -> 1

treeDepth :: Expr -> Int
treeDepth t = case unApp t of
  Just (f, ts@(_:_)) -> 1 + maximum (map treeDepth ts)
  _ -> 1

-- returns all scores and their sum
scoreTreeAndString :: (Expr, String) -> (Scores, Int)
scoreTreeAndString (t, s) =
  let scores = (scoreString s){tree_length = treeLength t, tree_depth = treeDepth t}
  in (scores,
      sum (map (\f -> f scores)
               [tree_length, tree_depth, characters, tokens,
                subsequent_dollars, initial_dollars, parses]))

-- sorts trees from lowest to highest total score
rankTreesAndStrings :: [(Expr, String)] -> [((Expr, String), (Scores, Int))] 
rankTreesAndStrings tss = sortOn (snd . snd) [(ts, scoreTreeAndString ts) | ts <- tss]


