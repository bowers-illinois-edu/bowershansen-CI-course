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

From within RStudio:   

- open file in RStudio
- If you have multiple files open, make sure this one's at front
- Press `Compile PDF` button above file edit window
- (You may get warnings and or failure messages in the output window even though a PDF was produced.)
- (If the file contains cross-references, you may have to hit `Compile PDF` twice.)


From the command line:
```
$ R CMD foo.Rnw
$ pdflatex foo.tex
$ pdflatex foo.tex
```

(Yep, `pdflatex` is run twice on the same file.)

From within R:
```
library(Sweave)
Sweave("foo.Rnw")
```
Separately (outside of R), use pdflatex to compile `foo.tex`.

# Improving these hints

Please don't hesitate to [fork](https://help.github.com/articles/fork-a-repo/) this repository, improve these hints (or any other aspect) and then shoot us a [pull request](https://help.github.com/articles/using-pull-requests/) explaining your fix.
