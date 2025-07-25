OPEN=open  # pdf viewer command

all: Informath.pgf Dedukti Agda Coq Lean RunInformath

.PHONY: all Informath.pgf Dedukti Agda Lean Coq RunInformath demo

Informath.pgf:
	cd grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Fam,Adj,Rel,Fun,Label,Const,Oper,Compar,Set,Coercion,Relverb,Relnoun,Reladj,Comparnoun,Verb,Pred3 --probs=Informath.probs InformathEng.gf InformathFre.gf InformathSwe.gf InformathGer.gf
	sed -i '' 's/Monad\.Identity/Monad\.Identity\nimport Control\.Monad/' grammars/Informath.hs
	ln -sf grammars/Informath.hs

Dedukti:
	cd typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make
	ln -sf typetheory/Dedukti

Agda:
	cd typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make
	ln -sf typetheory/Agda

Lean:
	cd typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make
	ln -sf typetheory/Lean

Coq:
	cd typetheory ; bnfc -m -p Coq --haskell-gadt Coq.bnf ; make
	ln -sf typetheory/Coq

RunInformath: 
	ghc -package gf RunInformath.hs

demo:
	./RunInformath -lang=Eng test/exx.dk
	./RunInformath -lang=Fre test/exx.dk
	./RunInformath -lang=Ger test/exx.dk
	./RunInformath -lang=Swe test/exx.dk
	./RunInformath -lang=Eng test/exx.dk >exx.txt
	./RunInformath -lang=Eng exx.txt
	./RunInformath -lang=Eng test/gflean-data.txt
	cat BaseConstants.dk test/exx.dk >bexx.dk
	dk check bexx.dk
	./RunInformath -to-agda test/exx.dk >exx.agda
	agda --prop exx.agda
	./RunInformath -to-coq test/exx.dk >exx.v
	cat BaseConstants.v exx.v >bexx.v   
	coqc bexx.v
	./RunInformath -to-lean test/exx.dk >exx.lean
	cat BaseConstants.lean exx.lean >bexx.lean   
	lean bexx.lean
	cat BaseConstants.dk test/top100.dk >out/texx.dk 
	dk check out/texx.dk
	cat BaseConstants.dk test/sets.dk >out/sexx.dk 
	dk check out/sexx.dk
	./RunInformath -to-latex-file -variations test/top100.dk >out/top100.tex
	echo "consider pdflatex out/top100.tex"
	./RunInformath -to-latex-file -variations test/sets.dk >out/sets.tex
	echo "consider pdflatex out/sets.tex"

top100:
	./RunInformath -to-latex-file -variations test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat BaseConstants.dk test/top100.dk >out/texx.dk 
	dk check out/texx.dk

top100fre:
	./RunInformath -to-latex-file -lang=Fre -variations -ranking test/top100.dk >out/top100fre.tex
	cd out ; pdflatex top100fre.tex ; $(OPEN) top100fre.pdf

top100ger:
	./RunInformath -to-latex-file -lang=Ger -variations test/top100.dk >out/top100ger.tex
	cd out ; pdflatex top100ger.tex ; $(OPEN) top100ger.pdf

top100single:
	./RunInformath -to-latex-file test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat BaseConstants.dk test/top100.dk >out/texx.dk 
	dk check out/texx.dk

baseconstants:
#	tail -150 BaseConstants.dk >tmp/baseconstants.dk
	cat BaseConstants.dk >tmp/baseconstants.dk

	./RunInformath -to-latex-file -variations tmp/baseconstants.dk >out/baseconstants.tex
	cd out ; pdflatex baseconstants.tex ; $(OPEN) baseconstants.pdf

parallel:
	tail -150 BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	cat test/top100.dk >>tmp/parallel.dk
	./RunInformath -parallel tmp/parallel.dk >tmp/parallel-informath.jsonl

parallel-def:
	tail -150 BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	./RunInformath -parallel -no-unlex -dedukti-tokens tmp/parallel.dk >tmp/parallel-def-train.jsonl



matita:
	./RunInformath test/mini-matita.dk

gflean:
	./RunInformath test/gflean-data.txt
