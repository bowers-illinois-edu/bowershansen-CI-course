# bowershansen-CI-course

Source files for Jake and Ben's causal inference course materials

## Compiling `.Rnw` files

### From within RStudio:

- open file in RStudio
- If you have multiple files open, make sure this one's at front
- Press `Compile PDF` button above file edit window
- (You may get warnings and or failure messages in the output window even though a PDF was produced.)
- (If the file contains cross-references, you may have to hit `Compile PDF` twice.)


## From within R and at the Command Line

```
library(knitr)
knit("foo.Rnw")
```

Separately (outside of R), use pdflatex to compile `foo.tex`.  E.g., from the command line:
```
$ pdflatex foo.tex
$ pdflatex foo.tex
```

(Yep, `pdflatex` is run twice on the same file.)

Alternatively you can use the ```knitandtex.sh``` shell command file which
uses ```latexmk``` to automagically run latex and bibtex as many times as
necessary. For example,

```
$ ./knitandtex.sh unit07-Rex
```

## Compiling `unitXX-YYY.tex` files

You'll need a LaTeX installation equipped with the beamer package and its dependencies.

Near the top of the .tex file, comment out all but one of these lines, depending on what format you want to produce:
```
%\input{slidesonly}
%\input{handout}
\input{handout+mynotes}

```

Latex should be run twice.  Or, for a file with `\usepackage{tikz}` in
the preamble, three times.

# About this work

## Improving these hints

Please don't hesitate to [fork](https://help.github.com/articles/fork-a-repo/) this repository, improve these hints (or any other aspect) and then shoot us a [pull request](https://help.github.com/articles/using-pull-requests/) explaining your fix.

## License

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/3.0/us/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/3.0/us/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/3.0/us/">Creative Commons Attribution-NonCommercial-ShareAlike 3.0 United States License</a>.
