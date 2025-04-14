module ConstantData where

import qualified Data.Map as M

-- storing data about constants in a separate file, to be read by RunInformath
-- each line is: constant, info items separated by spaces
-- the Project label is used so that all data can be stored globally and filtered if requested

type ConstantData = M.Map DkId ConstantInfo

type GFCat = String
type GFFun = String
type DkId = String
type Project = String

data Combination =
    ComALL
  | ComNONE
  | ComONLY [Int] -- which arguments to keep
 deriving Show
 
data ConstantInfo =
    BASE GFCat GFFun                      -- constant in BaseConstants.dk
  | ALIAS Project DkId Combination        -- alias for a BASE constant in Project
  | NEW Project GFCat GFFun Combination   -- new constant in Project
  | COERCION Project                      -- coercion, to be peeled away with all args except last
  | CONV DkId  -- just convert, e.g. from Dk to Lean
 deriving Show

convInfo other = CONV other

extractTargetConversions :: [[String]] -> [(String, String, String)]
extractTargetConversions ds = [(tgt, dk, t) | "#CONV":tgt:dk:t:_ <- ds]

extractConstantData :: Maybe [String] -> [[String]] -> M.Map String ConstantInfo
extractConstantData mprojs =
  M.fromList .
  filter (isFor mprojs) .
  map (mkConstantInfo) .  -- after =, a possible GF rule
  filter ((/='#') . head . head) 

isFor mprojs info = case mprojs of
  Just projs -> case info of
    (_, ALIAS p _ _) -> elem p projs
    (_, NEW p _ _ _) -> elem p projs
    (_, COERCION p) -> elem p projs
    _ -> True
  _ -> True

coercionFunctions cd = [s | (s, COERCION _) <- M.toList cd] 

mkConstantInfo words = case words of
  c : "BASE" : cat : fun : _ -> (c, BASE cat fun)
  c : "ALIAS" : proj : dkid : ws -> (c, ALIAS proj dkid (mkCombination ws))
  c : "NEW" : proj : cat : fun : ws -> (c, NEW proj cat fun (mkCombination ws))
  c : "COERCION" : proj : _ -> (c, COERCION proj)
  _ -> error $ "ill-formed constant data: " ++ unwords words

mkCombination ws = case ws of
  [] -> ComALL
  ["NONE"] -> ComNONE
  _ -> ComONLY (map read ws)

applyCombination com xs = case com of
  ComALL -> xs
  ComNONE -> []
  ComONLY ks -> [xs !! k | k <- ks]

lookBackConstantData = M.fromList . concatMap lookback . M.toList
  where
    lookback (c, info) = case info of
      BASE _ fun -> [(fun, c)] ---- (c, ComALL))
---      ALIAS _ fun com -> (fun, c) ---- (c, com)) -- not to be included
      NEW _ _ fun com -> [(fun, c)] ---- (c, com))
      COERCION _ -> [(c, c)]
      _ -> []


    