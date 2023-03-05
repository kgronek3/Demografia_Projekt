.SHELL := /bin/bash

.PHONY: compile clean delete clean_all

all: compile

compile:
	@echo "Compiling LaTeX file..."
	pdflatex praca_lic
	biber praca_lic 
	pdflatex praca_lic
	pdflatex praca_lic

clean:
	@echo "Cleaning log files..."
	@rm -f *.aux *.log *.bbl *.blg *.bcf *.run.xml *.out *.fdb_latexmk *.synctex.gz *.fls *.toc *.lof *.lot

delete:
	@echo "Deleting pdf file."
	@rm -f *.pdf

clean_all:
	@echo "Cleaning pdf and log files..."
	@rm -f *.aux *.log *.bbl *.blg *.bcf *.run.xml *.out *.fdb_latexmk *.synctex.gz *.fls *.pdf *.toc *.lof *.lot
	
