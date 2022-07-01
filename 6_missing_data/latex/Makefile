# -*- Makefile -*-

### Assumptions:
### 1. If you run `make all`, you want to compile every RNW file in the current
###    directory
### 2. After compilation, you want to move every PDF file to the parent
###    directory

## Find all RNW files beginning:
RNWS := $(wildcard *.Rnw)

## Strip the file extension off the located RNW files:
NAMES := $(RNWS:%.Rnw=%)

all: ${NAMES}

%: %.tex
	latexmk -pdf -f $@.tex

%.tex: %.Rnw
	Rscript -e "library(knitr); knit('$^')"

clean:
	mv *.pdf ../
	rm -f *.tex *.aux *.log *.out *.nav *.snm *.toc *.vrb *.bbl *.blg *.fdb_latexmk *.fls
