all: project.pdf

project.pdf: project.tex
	latexmk -xelatex -silent $<

.PHONY: clean
clean:
	rm -f *.out *.xdv *.ptb *.fdb_latexmk *.fls *.aux *.blg *.log *.agdai *.bbl
