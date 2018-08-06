
To compile the markdown file use one of these:


```
pandoc -s --to latex --from markdown assignment3.md --output assignment3.pdf

pandoc -s  assignment3.md --to latex --from markdown --output assignment3.tex --natbib ; latexmk -pdflatex='xelatex --shell-escape' -pdf assignment3.tex
```
