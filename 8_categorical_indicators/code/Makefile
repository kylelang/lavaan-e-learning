# -*- Makefile -*-

all: demo8 lab8

## Rule to compile both the RMD files with and without solutions: 
demo8: demo8.Rmd ../figures/basic_cfa.png ../figures/latent_response_cfa_tau.png
	Rscript -e 'rmarkdown::render("$<")'

## Rule to compile both the RMD files with and without solutions: 
lab8: lab8.Rmd
	Rscript render.R $@ TRUE
	Rscript render.R $@ FALSE

.phony: uncache 

uncache:
	rm -rf ${NAME}_cache/* ${NAME}_files/* ${NAME}_solutions_files/*
