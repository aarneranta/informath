module MkConstants where

-- build a lexicon extension from a configuration file
-- that annotates Dedukti constants with information for GF
-- Usage:
--
--  runghc MkConstants <file>.dkgf
--  make Core.pgf
--
-- the format of an annotation is
--
--   <id> <cat> <words>
--   Odd Adj odd
--   Nat Noun natural number
--   Eq Rel equal to
--   Succ Fun successor
--
-- from which it generates lines in three files
--
--   Constants.gf         :  fun Dk_<id> : <cat> ;
--   Core/ConstantsEng.gf :  lin Dk_<id> = mk<cat> "word"* ;
--   Core/Constants.hs    :  ("<id>", "<cat>"),
--
-- In this format, the overloaded mk<cat> opers in ConstantBaseEng.gf are assumed.
-- If more expressive power is needed, one can also write
--
--   <id> <cat> = <gf-expression>
--
-- in which case <gf-expression> is used verbatim in ConstantsEng.gf
--
-- Annotations are written in a file such as nat.dkgf.
-- It is a good idea to save these files, from which the Constants* files can be
-- rebuilt at need.


import PGF

import Data.List (intersperse)
import System.Environment (getArgs)


mkConstants :: FilePath -> IO ()
mkConstants file = do
  annots <- readFile file >>= return . map words . filter (not . null) . lines
  let dkannots = [annot | annot@(_:_:mk:_) <- annots, mk /= "->"]
  writeAndReport "grammars/Constants.gf" $ mkConstantsGF dkannots
  writeAndReport "grammars/ConstantsEng.gf" $ mkConstantsEngGF dkannots
  writeAndReport "Constants.hs" $ mkConstantsHS annots


mkConstantsGF annots = unlines $ [
  "abstract UserConstants = BaseConstants ** {",
  "",
  "-- generated by MkConstants.hs",
  ""
  ] ++
  map mkConstant annots ++
  ["}"]
 where
   mkConstant (fun:cat:_) =
     unwords(["fun", dk fun, ":", cat, ";"])


mkConstantsEngGF annots = unlines $ [
  "concrete UserConstantsEng of UserConstants = BaseConstantsEng **",
  "",
  "-- generated by MkConstants.hs",
  "",
  "open",
  "  SyntaxEng,",
  "  ParadigmsEng,",
  "  SymbolicEng,",
  "  NotationsEng,",
  "  Formal",
  "",
  "in {"
  ] ++
  map mkConstant annots ++
  ["}"]
 where
   mkConstant (fun:cat:ws) = 
     unwords(["lin", dk fun, "=", lin cat ws, ";"])
   lin cat ws = case ws of
     "=":ww -> unwords ww
     _ -> unwords (("mk"++cat) : map quote ws)

mkConstantsHS annots = unlines $ [
  "module UserConstants where",
  "",
  "constants = ["
  ] ++
  [concat (intersperse ", \n" (map mkConstant annots))] ++
  ["  ]"]
 where
   mkConstant (fun:cat:mk:ws) =
       concat(["  (", quote fun, ", ", quote cat, ", ", quote lin, ")"])
     where
       lin = case mk of
         "->" -> unwords ws
         _ -> dk fun


writeAndReport :: FilePath -> String -> IO ()
writeAndReport file s = do
  writeFile file s
  putStrLn $ "wrote " ++ file


quote :: String -> String
quote s = "\"" ++ s ++ "\"" --- we don't expect to need escapes in s


dk :: String -> String  --- should be the same as in CommonConcepts
dk s = showCId (mkCId ("Dk_" ++ s)) -- mkCId takes care of escapes and quotes


main = do
  file:_ <- getArgs
  mkConstants file
