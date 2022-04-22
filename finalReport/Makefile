all: roadmap.pdf

roadmap.pdf: roadmap.tex
	latexmk -xelatex -silent $<

roadmap.tex: roadmap.lagda
	agda --latex-dir=. --latex $<

.PHONY: clean
clean:
	rm -f roadmap.tex *.out *.pdf *.xdv *.ptb *.fdb_latexmk *.fls *.aux *.blg *.log *.agdai *.bbl
