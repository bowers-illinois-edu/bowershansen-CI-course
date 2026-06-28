# =============================================================================
# Makefile -- build Tom's 2026 ICPSR Causal Inference lecture slides
# =============================================================================
#
# Each dated folder holds one Beamer source <slide>.tex and one <slide>.R.
# The R script writes the figures the slides include into a figures/ subfolder
# (only when saveplots_ is TRUE, which this Makefile sets). The slides are then
# compiled with LuaLaTeX (the metropolis theme needs lualatex/xelatex, not
# pdflatex). So each day's PDF is built in two steps:
#
#     1. Rscript (saveplots_ <- TRUE)  ->  figures/*.pdf and *.png
#     2. latexmk -lualatex             ->  <slide>.pdf
#
# Two days also need a standalone TikZ figure that no R script produces:
#     2026-06-17 needs figures/estimator.pdf
#                  (compiled from figures/Estimation_Diagram/estimation.tex)
#     2026-06-24 and 2026-06-26 need figures/matching_pipeline_flowchart.pdf
#                  (compiled from ../matching-guide/.../version_2, then copied in)
#
# -----------------------------------------------------------------------------
# Usage
# -----------------------------------------------------------------------------
#   make                 build every day's PDF (same as `make all`)
#   make -k              build everything that can build; skip days that fail
#   make 2026-06-17      build a single day (use the FOLDER name)
#   make figures         only (re)generate figures (R + the two standalones)
#   make deps            install the R packages the scripts need
#   make clean           remove LaTeX aux files (keep PDFs and figures)
#   make distclean       also remove built PDFs and the copied-in figures
#   make help            print this list
#
# Note: days 2026-06-22, 2026-06-23, and 2026-06-26 need extra R packages
# (blockTools, nbpMatching, plot3D, PSweight, blkvar). Run `make deps` first,
# or use `make -k` to build the other days and skip these until deps are in.
# =============================================================================

SHELL := /bin/bash

# Folders that hold a day's slides. Two use underscores, the rest use dashes;
# the .tex/.R basename inside each always uses dashes (folder 2026_06_15 holds
# 2026-06-15.tex), so we map folder -> basename with $(subst _,-,...).
DAYS := 2026_06_15 2026_06_16 2026-06-17 2026-06-18 2026-06-22 \
        2026-06-23 2026-06-24 2026-06-25 2026-06-26

# folder -> its slide PDF, e.g. 2026_06_15 -> 2026_06_15/2026-06-15.pdf
PDFS := $(foreach d,$(DAYS),$(d)/$(subst _,-,$(d)).pdf)

# Standalone TikZ figure built from the matching guide and copied into the
# two days that include it. Paths are relative to this Makefile's folder.
FLOWCHART_DIR := ../matching-guide/guide/fig/version_2
FLOWCHART_SRC := $(FLOWCHART_DIR)/matching_pipeline_flowchart_V2.tex
FLOWCHART_OUT := $(FLOWCHART_DIR)/matching_pipeline_flowchart_V2.pdf

LATEXMK := latexmk -lualatex -shell-escape -interaction=nonstopmode -halt-on-error

.PHONY: all pdfs figures deps clean distclean help $(DAYS)

all: pdfs
pdfs: $(PDFS)

# -----------------------------------------------------------------------------
# Per-folder shortcut: `make 2026-06-17` builds that day's PDF.
# -----------------------------------------------------------------------------
$(DAYS):
	$(MAKE) $@/$(subst _,-,$@).pdf

# -----------------------------------------------------------------------------
# Generic rule: a day's PDF depends on its .tex and .R. Regenerate the figures
# (R) and then compile the slides (LuaLaTeX), both with the folder as cwd.
# -----------------------------------------------------------------------------
%.pdf: %.tex %.R
	@echo "==> $(@D): generating figures (R)"
	cd $(@D) && mkdir -p figures && \
	  Rscript -e 'saveplots_ <- TRUE; source("$(notdir $*).R")'
	@echo "==> $(@D): compiling slides (lualatex)"
	cd $(@D) && $(LATEXMK) $(notdir $*).tex

# -----------------------------------------------------------------------------
# Extra prerequisites: the two standalone figures the R scripts do not produce.
# -----------------------------------------------------------------------------
2026-06-17/2026-06-17.pdf: 2026-06-17/figures/estimator.pdf
2026-06-24/2026-06-24.pdf: 2026-06-24/figures/matching_pipeline_flowchart.pdf
2026-06-26/2026-06-26.pdf: 2026-06-26/figures/matching_pipeline_flowchart.pdf

# estimator.pdf <- compile the standalone, then copy it next to the slides.
2026-06-17/figures/estimator.pdf: 2026-06-17/figures/Estimation_Diagram/estimation.tex
	@echo "==> building standalone figure: estimator.pdf"
	cd $(@D)/Estimation_Diagram && \
	  latexmk -lualatex -interaction=nonstopmode -halt-on-error estimation.tex
	mkdir -p $(@D)
	cp $(@D)/Estimation_Diagram/estimation.pdf $@

# matching_pipeline_flowchart.pdf <- build once from the matching guide, copy in.
%/figures/matching_pipeline_flowchart.pdf: $(FLOWCHART_OUT)
	mkdir -p $(@D)
	cp $(FLOWCHART_OUT) $@

$(FLOWCHART_OUT): $(FLOWCHART_SRC)
	@echo "==> building standalone figure: matching_pipeline_flowchart.pdf"
	cd $(FLOWCHART_DIR) && \
	  latexmk -lualatex -interaction=nonstopmode -halt-on-error \
	  matching_pipeline_flowchart_V2.tex

# -----------------------------------------------------------------------------
# Figures only -- no slide compilation.
# -----------------------------------------------------------------------------
figures:
	@for d in $(DAYS); do \
	  echo "==> figures: $$d"; \
	  ( cd $$d && mkdir -p figures && \
	    Rscript -e 'saveplots_ <- TRUE; source(Sys.glob("20*.R"))' ) || exit 1; \
	done
	$(MAKE) 2026-06-17/figures/estimator.pdf
	$(MAKE) 2026-06-24/figures/matching_pipeline_flowchart.pdf
	$(MAKE) 2026-06-26/figures/matching_pipeline_flowchart.pdf

# -----------------------------------------------------------------------------
# Install the R packages the scripts load. Four are on CRAN; blkvar is on
# GitHub only (lmiratrix/blkvar), installed via remotes.
# -----------------------------------------------------------------------------
deps:
	Rscript -e 'cran <- c("blockTools","nbpMatching","plot3D","PSweight","dplyr","estimatr","ggplot2","optmatch","randomizr","RItools","sandwich","senstrat","viridis"); \
	  miss <- cran[!cran %in% rownames(installed.packages())]; \
	  if (length(miss)) install.packages(miss, repos="https://cloud.r-project.org"); \
	  if (!requireNamespace("blkvar", quietly=TRUE)) { \
	    if (!requireNamespace("remotes", quietly=TRUE)) install.packages("remotes", repos="https://cloud.r-project.org"); \
	    remotes::install_github("lmiratrix/blkvar") }'

# -----------------------------------------------------------------------------
# Cleanup.
# -----------------------------------------------------------------------------
clean:
	@for d in $(DAYS); do ( cd $$d && latexmk -c 20*.tex >/dev/null 2>&1 ); done
	-cd $(FLOWCHART_DIR) && latexmk -c >/dev/null 2>&1
	-cd 2026-06-17/figures/Estimation_Diagram && latexmk -c >/dev/null 2>&1
	@echo "removed LaTeX aux files (PDFs and figures kept)"

distclean: clean
	@for d in $(DAYS); do \
	  rm -f $$d/$$(echo $$d | tr '_' '-').pdf; \
	  rm -f $$d/figures/*.pdf; \
	done
	-rm -f $(FLOWCHART_OUT) 2026-06-17/figures/Estimation_Diagram/estimation.pdf
	@echo "removed built PDFs and generated/copied figure PDFs (photos kept)"

help:
	@sed -n '1,46p' $(firstword $(MAKEFILE_LIST))
