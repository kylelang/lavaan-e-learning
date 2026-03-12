# -*- Makefile -*-

all: lab5

## Rule to compile both the RMD files with and without solutions: 
%: %.Rmd
	Rscript render.R $@ TRUE
	Rscript render.R $@ FALSE
