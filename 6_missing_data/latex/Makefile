# -*- Makefile -*-

TARGET = missing_data
DEST = ../

all: $(TARGET)
	
%: %.tex
	latexmk -pdf $^

%.tex: %.Rnw
	Rscript -e "knitr::knit('$^')"

.PHONY: tidy nuke clean uncache

tidy: clean
	cp $(TARGET).pdf $(DEST)

nuke: uncache clean
	rm -f $(TARGET).pdf $(TARGET).tex

clean:
	find ./$(TARGET)* -maxdepth 1 -type f | grep -vE "\.pdf|\.Rnw|\.tex|\.bib" | xargs rm -f

uncache:
	rm -f cache/* figure/*
