{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Core2Dedukti (jmt2dedukti)
import Dedukti2Core
import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Dedukti.ErrM
import DeduktiOperations (
  identsInTypes, dropDefinitions, stripQualifiers, identTypes, ignoreCoercions,
  ignoreFirstArguments, peano2int, applyConstantData)
import ConstantData (
  ConstantData, extractConstantData, lookBackConstantData, extractTargetConversions, convInfo, coercionFunctions)
import Informath -- superset of Core
import Core2Informath (nlg)
import Informath2Core (semantics)
import ParseInformath (parseJmt)
import Lexing
import MkConstants (mkConstants)
import qualified Dedukti2Agda as DA
import qualified Dedukti2Coq as DC
import qualified Dedukti2Lean as DL

import PGF

import Data.List (partition, isSuffixOf, isPrefixOf, intersperse, sortOn)
----import System.Random
import System.Environment (getArgs)
import System.IO
import qualified Data.Map as M

helpMsg = unlines [
  "usage: RunInformath <flag>* <file>?",
  "without arguments or flags, start interactive session",
  "",
  "  ? <string>  translate from natural language to Dedukti",
  "  <string>    translate from Dedukti to natural language",
  "",
  "with file argument: depending on file suffix,",
  "  .dk    read Dedukti file and convert to natural language of Agda",
  "  .dkgf  create UserConstants files to map Dedukti identifiers",
  "  .txt   (or any other) parse as natural language, convert to Dedukti",
  "flags:",
  "",
  "  -help           print this message",
  "  -data=<files>   constant data additional to base_constant_data.dkgf",
  "  -projects=<names> project names used for filtering additional constant data",
  "  -to-agda        convert to Agda (with <file>.dk as argument)",
  "  -to-coq         convert to Coq (with <file>.dk as argument)",
  "  -to-lean        convert to Lean (with <file>.dk as argument)",
  "  -to-dedukti     to Dedukti code (typically after changes in <file.dk>)",
  "  -lang=<lang>    natural language to be targeted; Eng (default), Swe, Fre,...",
  "  -gfname=<ident> GF module generated from .dkgf, default UserConstants", 
  "  -to-latex-file  generate a LaTeX file (used with natural language output)",
  "  -parallel       generate a jsonl list with all languages and variations",
  "  -v              verbose output, e.g. syntax trees and intermediate results",
  "  -variations     when producing natural language, show all variations",
  "  -idents         show frequency list of idents in a Dedukti file",
  "  -dropdefs       drop definition parts of Dedukti code",
  "  -dropqualifs    strip qualifiers of idents",
  "  -dropcoercions  strip named coercions, only leaving their last arguments",
  "  -dropfirstargs  drop first k arguments of given functions (usually type arguments)",
  "  -peano2int      convert succ/0 natural numbers to sequences of digits",
  "  -idents         print a frequency table of identifiers in a .dk file",
  "  -idtypes        print the types identifiers in a .dk file",
  "",
  "Output is to stdout and can be redirected to a file to check with",
  "Dedukti or Agda or Coq or Lean when producing one of these."
  ]

informathPrefix = "Informath"
informathPGFFile = "grammars/" ++ informathPrefix ++ ".pgf"
baseConstantDataFile = "base_constant_data.dkgf"
Just jmt = readType "Jmt"

data Env = Env {
 flags :: [String],
 constantData :: ConstantData,
 lookBackData :: M.Map String String,  -- from GFFun to DkId ---- and to more info?
 convToAgdaData :: ConstantData,
 convToCoqData :: ConstantData,
 convToLeanData :: ConstantData,
 cpgf :: PGF,
 lang :: Language,
 termindex :: [String] -- list of terms replaced by \INDEXEDTERM{ i }
 }

ifFlag x env = elem x (flags env)
ifv env act = if (ifFlag "-v" env) then act else return ()

flagValue flag dfault ff = case [f | f <- ff, isPrefixOf flag (tail f)] of
  f:_ -> drop (length flag + 2) f   -- -<flag>=<value>
  _ -> dfault

commaSep s = words (map (\c -> if c==',' then ' ' else c) s)

allLanguages env = languages (cpgf env)

main = do
  xx <- getArgs
  let (ff, yy) = partition ((== '-') . head) xx
  corepgf <- readPGF informathPGFFile
  let otherdatafiles = commaSep (flagValue "data" "" ff)
  datafiles <- mapM readFile (baseConstantDataFile : otherdatafiles) >>= return . concatMap lines
  let datalines = filter (not . null) (map words datafiles)
  let mprojects = let ps = commaSep (flagValue "projects" "" ff)
                  in if null ps then Nothing else Just ps
  let constantdata = extractConstantData mprojects datalines
  let targetdata = extractTargetConversions datalines
  let lookbackdata = lookBackConstantData constantdata 
  let Just lan = readLanguage (informathPrefix ++ (flagValue "lang" "Eng" ff))
  let env = Env{
        flags = ff,
	constantData = constantdata,
	lookBackData = lookbackdata,
	convToAgdaData = M.fromList [(dk, convInfo t) | ("Agda", dk, t) <- targetdata],
	convToCoqData = M.fromList [(dk, convInfo t) | ("Coq", dk, t) <- targetdata],
	convToLeanData = M.fromList [(dk, convInfo t) | ("Lean", dk, t) <- targetdata],
	cpgf = corepgf,
	lang=lan,
	termindex = []}
  case yy of
    _ | ifFlag "-help" env -> do
      putStrLn helpMsg
    filename:_ | isSuffixOf ".dkgf" filename -> do
      let gfname = flagValue "gfname" "UserConstants" ff
      mkConstants filename gfname
    filename:_ | isSuffixOf ".dk" filename -> do
      s <- readFile filename
      mo@(MJmts jmts) <- parseDeduktiModule env s
      case s of
        _ | ifFlag "-to-agda" env ->
	  DA.processDeduktiModule (applyConstantData (convToAgdaData env) mo)
        _ | ifFlag "-to-coq" env ->
	  DC.processDeduktiModule (applyConstantData (convToCoqData env) mo)
        _ | ifFlag "-to-lean" env -> 
	  DL.processDeduktiModule (applyConstantData (convToLeanData env) mo)
	_ | ifFlag "-to-dedukti" env -> mapM_ putStrLn [printTree j | j <- jmts] -- when modifying dedukti
	_ | ifFlag "-parallel" env -> parallelJSONL env{flags = "-variations":flags env} mo
	_ | ifFlag "-idents" env -> printFrequencyTable (identsInTypes mo)
	_ | ifFlag "-idtypes" env ->
	      mapM_ putStrLn [printTree (JStatic c t) | (c, t) <- M.toList (identTypes mo)]
	_ -> processDeduktiModule env mo
    filename:_  -> do
      s <- readFile filename
      ss0 <- mapM (processInformathJmt env) (filter (not . null) (lines s))
      let ss = renameLabels ss0 -- quick hack to rename labels
      mo <- parseDeduktiModule env (unlines ss)
      case s of
        _ | ifFlag "-to-agda" env ->
	  DA.processDeduktiModule (applyConstantData (convToAgdaData env) mo)
        _ | ifFlag "-to-coq" env ->
	  DC.processDeduktiModule (applyConstantData (convToCoqData env) mo)
        _ | ifFlag "-to-lean" env ->
	  DL.processDeduktiModule (applyConstantData (convToLeanData env) mo)
	_ -> mapM_ putStrLn ss
    _ -> do
      loop env

loop :: Env -> IO ()
loop env = do
  putStr "> "
  hFlush stdout
  ss <- getLine
  case ss of
    '?':s -> processInformathJmt env s >>= putStrLn
    '=':s -> roundtripDeduktiJmt env s >> return ()
    _     -> parseDeduktiModule env ss >>= processDeduktiModule env
  loop env

parseDeduktiModule :: Env -> String -> IO Module
parseDeduktiModule env s = do
  case pModule (myLexer s) of
    Bad e -> error ("parse error: " ++ e)
    Ok mo -> return $ foldr ($) mo (deduktiOpers env)


deduktiOpers :: Env -> [Module -> Module]
deduktiOpers env =
  [peano2int | ifFlag "-peano2int" env] ++
  [ignoreCoercions envCoercions | ifFlag "-dropcoercions" env] ++
  [applyConstantData (constantData env) | not (noConstantData env)] ++ 
  [stripQualifiers | ifFlag "-dropqualifs" env] ++ 
  [dropDefinitions | ifFlag "-dropdefs" env] 
 where
  envCoercions = map QIdent (coercionFunctions (constantData env))
  noConstantData env =
    or [ifFlag f env | f <- words "-to-agda -to-coq -to-dedukti -to-lean -rawconstantdata -parallel"]

-- example: ./RunInformath -idtypes -dropdefs -dropqualifs -dropcoercions test/matita-all.dk

processDeduktiModule :: Env -> Module -> IO ()
processDeduktiModule env mo@(MJmts jmts) = 
  if ifFlag "-to-latex-file" env
    then do
      putStrLn latexPreamble
      flip mapM_ jmts $ processDeduktiJmtTree env --(\j -> processDeduktiJmtTree env j >> putStrLn "")
      putStrLn "\\end{document}"
  else
    flip mapM_ jmts $ processDeduktiJmtTree env

roundtripDeduktiJmt :: Env -> String -> IO ()
roundtripDeduktiJmt env cs = do
  let gr = cpgf env
  case pJmt (myLexer cs) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok t -> do
      ifv env $ putStrLn $ "## Dedukti: " ++ show t
      let gft = gf $ jmt2core t
      ifv env $ putStrLn $ "## MathCore: " ++ showExpr [] gft
      let lin = unlextex $ linearize gr (lang env) gft
      putStrLn lin
      processCoreJmt env lin

processDeduktiJmtTree :: Env -> Jmt -> IO ()
processDeduktiJmtTree env t = do
  let gr = cpgf env
  ifv env $ putStrLn $ "#Dedukti: " ++ show t
  let ct = jmt2core t
  let gft = gf ct
  ifv env $ putStrLn $ "## MathCore: " ++ showExpr [] gft
  ifv env $ putStrLn $ "# MathCoreEng: " ++ unlextex (linearize gr (lang env) gft)
  convertCoreToInformath env ct

convertCoreToInformath :: Env -> GJmt -> IO ()
convertCoreToInformath env ct = do
  let fgr = cpgf env
  let fts = nlg (flags env) ct
  let gffts = map gf fts
  flip mapM_ gffts $ \gfft -> do
    ifv env $ putStrLn $ "## Informath: " ++ showExpr [] gfft
    putStrLn $ unlextex $ linearize fgr (lang env) gfft
    if (ifFlag "-to-latex-file" env) then (putStrLn "") else return ()

processCoreJmt :: Env -> String -> IO ()
processCoreJmt env s = do
  let gr = cpgf env
  let ls = lextex s
  ifv env $ putStrLn ls
  let (mts, msg) = parseJmt gr (lang env) jmt ls
  ifv env $ putStrLn msg
  case mts of
    Just ts@(_:_) -> do
      flip mapM_ ts $ processCoreJmtTree env
    _ -> putStrLn ("NO PARSE: " ++ ls)


processCoreJmtTree :: Env -> Expr -> IO ()
processCoreJmtTree env t = do
  let gr = cpgf env
  ifv env $ putStrLn $ "## Informath: " ++ showExpr [] t
  ifv env $ putStrLn $ "# InformathEng: " ++ unlextex (linearize gr (lang env) t)
  let tr = fg t
  let str = semantics tr
  let st = gf str
  ifv env $ putStrLn $ "## MathCore: " ++ showExpr [] st
  ifv env $ putStrLn $ "# MathCoreEng: " ++ unlextex (linearize gr (lang env) st)
  let d = jmt2dedukti (lookBackData env) str
  putStrLn $ printTree d
---  convertCoreToInformath env str

processInformathJmt :: Env -> String -> IO String
processInformathJmt env s = do
  let gr = cpgf env
  let ls = lextex s
  ifv env $ putStrLn $ "## LEXED: " ++ ls
  let (ils, tindex) = indexTex ls
  ifv env $ putStrLn $ "## INDEXED: " ++ ils ++ show tindex
  let (mts, msg) = parseJmt gr (lang env) jmt ils
  ifv env $ putStrLn msg
  case mts of
    Just ts@(t:_) -> do
      let env1 = env{termindex = tindex}
      s:_ <- flip mapM ts $ processInformathJmtTree env1
      return s
    _ -> do
      ifv env $ putStrLn ("# NO PARSE: " ++ ils)
      return ""

processInformathJmtTree :: Env -> Expr -> IO String
processInformathJmtTree env t0 = do
  let gr = cpgf env
  ifv env $ putStrLn $ "## Informath: " ++ showExpr [] t0
  let t = unindexJmt env t0
  ifv env $ putStrLn $ "## Informath: " ++ showExpr [] t
  let tr = fg t
  let str = semantics tr
  let st = gf str
  ifv env $ putStrLn $ "## Core     : " ++ showExpr [] st
  ifv env $ putStrLn $ unlextex (linearize gr (lang env) st)
  let d = jmt2dedukti (lookBackData env) str
  let dt = printTree d
  ifv env $ putStrLn $ dt
  return dt

parallelJSONL :: Env -> Module -> IO ()
parallelJSONL env mo@(MJmts jmts) = do
  let gr = cpgf env
  let MJmts cjmts = applyConstantData (constantData env) mo
  flip mapM_ (zip jmts cjmts) $ \ (jmt, cjmt) -> do
      let tree = jmt2core cjmt
      let gft = gf tree
      let json = concat $ intersperse ", " $ [
            mkJSONField "dedukti" (printTree jmt),
            mkJSONField "agda" (DA.printAgdaJmts (DA.transJmt (applyConstantData (convToAgdaData env) jmt))),
            mkJSONField "coq" (DC.printCoqJmt (DC.transJmt (applyConstantData (convToCoqData env) jmt))),
            mkJSONField "lean" (DL.printLeanJmt (DL.transJmt (applyConstantData (convToLeanData env) jmt)))
	    ] ++ [
	      mkJSONListField (showCId lang)
	        [unlextex (linearize gr lang (gf t)) | t <- nlg (flags env) tree]
		  | lang <- allLanguages env
	    ]
      putStr "{"
      putStr json
      putStrLn "}"

mkJSONField :: String -> String -> String
mkJSONField key value = show key ++ ": " ++ stringJSON value

mkJSONListField :: String -> [String] -> String
mkJSONListField key values =
  show key ++ ": " ++ "[" ++ concat (intersperse ", " (map stringJSON values)) ++ "]"


stringJSON = quote . escape where
  quote s = "\"" ++ s ++ "\""
  escape s = case s of
    c:cs | elem c "\"\\" -> '\\':c:escape cs
    '\n':cs -> '\\':'n':escape cs
    c:cs -> c:escape cs
    _ -> s


renameLabels :: [String] -> [String]
renameLabels ss = [rename i s | (i, s) <- zip [1..] ss] where
  rename i s = case words s of
    "noLabel":ws -> unwords (("noLabel" ++ "_" ++ show i):ws)
    _ -> s


unindexJmt :: Env -> Expr -> Expr
unindexJmt env expr = maybe expr id (unind  expr) where
  unind expr = case unApp expr of
    Just (f, [x]) -> case unInt x of
      Just i -> case showCId f of
        "IndexedTermExp" -> parsed "Exp" (look i)
        "IndexedFormulaProp" -> parsed "Prop" (look i)
        "IndexedLetFormulaHypo" -> do
	   formula <- parsed "Formula" (filter (/='$') (look i))
	   return $ mkApp (mkCId "LetFormulaHypo") [formula]
        _ -> return expr
      _ -> do
        ux <- unind x
        return $ mkApp f [ux]
    Just (f, xs) -> do
       uxs <- mapM unind xs
       return $ mkApp f uxs
    _ -> return expr


  look i = termindex env !! i
  parsed c s = do
    cat <- readType c
    let (mts, msg) = parseJmt (cpgf env) (lang env) cat s
    case mts of
      Just (t:ts) -> return t ---- todo: ambiguity if ts
      _ -> Nothing


printFrequencyTable :: M.Map QIdent Int -> IO ()
printFrequencyTable m = do
  let list = sortOn (\ (_, i) -> -i) $ M.toList m
  mapM_ putStrLn ["(" ++ show n ++ ")\t" ++ printTree x | (x, n) <- list]

latexPreamble = unlines [
  "\\documentclass{article}",
  "\\usepackage{amsfonts}",
  "\\usepackage{amssymb}",
  "\\setlength\\parindent{0pt}",
  "\\setlength\\parskip{8pt}",
  "\\begin{document}"
  ]