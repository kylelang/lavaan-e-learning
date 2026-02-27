# -*- Makefile -*-

TARGET = sem_mediation

all: ${TARGET}

%: %.tex
	latexmk -pdf $@.tex

%.tex: %.Rnw
	Rscript -e "library(knitr); knit('$^')"

tidy: clean
	cp ${TARGET}.pdf ../

nuke: uncache clean
	rm -f ${TARGET}.pdf ${TARGET}.tex

clean:
	find ./${TARGET}* -maxdepth 1 -type f | grep -vE "\.pdf|\.Rnw|\.tex|\.bib" | xargs rm -f

uncache:
	rm -f cache/* figure/*

.PHONY: tidy clean nuke uncache
