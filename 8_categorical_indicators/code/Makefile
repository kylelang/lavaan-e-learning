# -*- Makefile -*-

NAME = demo8

all: ${NAME}

## Rule to compile both the RMD files with and without solutions: 
%: %.Rmd
	Rscript -e 'rmarkdown::render("$<")'

.phony: uncache 

uncache:
	rm -rf ${NAME}_cache/* ${NAME}_files/* ${NAME}_solutions_files/*
