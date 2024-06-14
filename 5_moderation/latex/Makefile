# -*- Makefile -*-

TARGET=moderation
DEST=../

all: $(TARGET).pdf
	
%.pdf: %.tex
	latexmk -pdf -f $^

%.tex: %.Rnw
	Rscript -e "knitr::knit('$^')"

.PHONY: nuke clean tidy

nuke: clean
	rm -f cache/* figure/* $(TARGET).pdf

tidy: clean
	mv $(TARGET).pdf $(DEST)

clean:
	rm -f *.tex *.aux *.log *.out *.nav *.snm *.toc *.vrb *.bbl *.blg *.fdb_latexmk *.fls
