# -*- Makefile -*-

NAME = lab6

all: ${NAME}

## Rule to compile both the RMD files with and without solutions: 
%: %.Rmd
	Rscript render.R $@ TRUE
	Rscript render.R $@ FALSE

.phony: uncache 

uncache:
	rm -rf ${NAME}_cache/* ${NAME}_files/* ${NAME}_solutions_files/*
