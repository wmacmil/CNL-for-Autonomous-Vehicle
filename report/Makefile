all: report.pdf

report.pdf: report.tex
	latexmk -xelatex -silent $<

report.tex: report.lagda
	agda --latex-dir=. --latex $<

.PHONY: clean
clean:
	rm -f roadmap.tex *.out *.pdf *.xdv *.ptb *.fdb_latexmk *.fls *.aux *.blg *.log *.agdai *.bbl
