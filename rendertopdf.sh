#!/bin/bash

if [ -n $1 ]; then
#  Rscript -e "library(rmarkdown);render('$1.Rmd')"; latexmk $1.tex
  Rscript -e "library(rmarkdown);render('$1.Rmd')"
fi
