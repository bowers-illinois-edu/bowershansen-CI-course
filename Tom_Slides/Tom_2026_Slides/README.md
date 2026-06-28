# Building Tom's 2026 lecture slides

Each dated folder holds one Beamer source `<slide>.tex` and one `<slide>.R`.
The R script writes the figures the slides include into a `figures/`
subfolder; the slides are then compiled with LuaLaTeX (the `metropolis`
theme needs `lualatex` or `xelatex`, not `pdflatex`). The `Makefile`
automates both steps.

## Requirements

- R (the slides were last built under R 4.6.1) with the packages listed
  under "R package requirements" below. On a fresh machine, run `make deps`
  once to install them.
- A LaTeX distribution (TeX Live) providing `lualatex` and `latexmk`, with
  the `metropolis` Beamer theme and the `fontawesome5` package (used by the
  estimator diagram). A full TeX Live install includes all of these.

Run every command below from this directory (`Tom_Slides/Tom_2026_Slides`).

## Quick start

    make deps       # first time only: install the R packages (see below)
    make            # build every day's PDF
    make 2026-06-17 # build one day (use the FOLDER name)
    make -k         # build everything that can; skip days whose deps are missing

Each `<slide>.pdf` lands next to its source in the dated folder.

The single-day target is the folder name exactly as it is on disk: the June
15 and 16 decks use underscores (`make 2026_06_16`); every other day uses
dashes (`make 2026-06-25`).

## What the Makefile does, per day

1. Runs the R script with `saveplots_ <- TRUE` so it (re)writes its figures
   into `figures/`. (The scripts default to `saveplots_ <- FALSE` and skip
   writing, so they are safe to source interactively without overwriting.)
2. Compiles the slides with
   `latexmk -lualatex -shell-escape -interaction=nonstopmode`.

Two figures are not produced by any R script and the Makefile builds them
as standalone TikZ documents:

- `2026-06-17/figures/estimator.pdf` from `figures/Estimation_Diagram/estimation.tex`.
- `2026-06-24` and `2026-06-26` use `figures/matching_pipeline_flowchart.pdf`,
  compiled from `../matching-guide/guide/fig/version_2/` and copied in. To use
  the version_1 flowchart instead, edit `FLOWCHART_DIR`/`FLOWCHART_SRC`/`FLOWCHART_OUT`
  near the top of the `Makefile`.

## Other targets

    make figures    # only (re)generate figures, no LaTeX
    make deps       # install the R packages the scripts need (see below)
    make clean      # remove LaTeX aux files (keep PDFs and figures)
    make distclean  # also remove built PDFs and generated/copied figure PDFs
    make help       # print usage

## R package requirements

The scripts load: blkvar, blockTools, dplyr, estimatr, ggplot2, nbpMatching,
optmatch, plot3D, PSweight, randomizr, RItools, sandwich, senstrat, viridis.

`make deps` installs any that are missing: the CRAN ones via
`install.packages()`, and `blkvar` (GitHub only) via
`remotes::install_github("lmiratrix/blkvar")`.

The builds run R from each dated folder, where there is no `.Rprofile`, so
they use your personal R library rather than the repo-root renv project. If
you later want the slides built through renv, point the R step at the project
(for example, run R from a folder that activates renv, or set the project via
`RENV_PROJECT`).
