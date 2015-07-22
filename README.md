# bowershansen-CI-course

Source files for Jake and Ben's causal inference course materials

## Compiling unitXX-YYY.tex files

You'll need a LaTeX installation equipped with the beamer package and its dependencies. 

Near the top of the .tex file, comment out all but one of these lines, depending on what format you want to produce:
```
%\input{slidesonly}
%\input{handout}
\input{handout+mynotes}

```

## Compiling *.Rnw files

From the command line:
```
$ R CMD foo.Rnw
$ pdflatex foo.tex
```

From within R:
```
library(Sweave)
Sweave("foo.Rnw")
```
Separately (outside of R), use pdflatex to compile `foo.tex`.

