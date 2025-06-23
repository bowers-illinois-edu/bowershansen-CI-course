# Standard Makefile boilerplate
SHELL = /bin/sh

RSCRIPT = Rscript

# Circumvent whatever's in .Rprofile (eg renv setup)
RDO = $(RSCRIPT) --no-init-file -e 

.PHONY: day_%-all.pdf

day_%.md: day_%.Rmd
	$(RDO) 'handout_ <- FALSE; rmarkdown::render("$<", "md_document")'

daily-announcement.md:
	touch daily-announcement.md

old-daily-announcements.md: daily-announcement.md
	echo '\n ' >> old-daily-announcements.md && cat daily-announcement.md >> old-daily-announcements.md

recap.tex:
	touch recap.tex

styles/daily-announcement.tex: daily-announcement.md recap.tex
	pandoc -o styles/daily-announcement.tex daily-announcement.md && echo '\n ' >> styles/daily-announcement.tex && cat recap.tex >> styles/daily-announcement.tex

day_%-wmn.pdf: day_%.Rmd styles/daily-announcement.tex styles/wmn_output_yaml styles/wmn-preamble.tex styles/auth+date.yaml
	cp styles/wmn_output_yaml _output.yaml && $(RDO) 'handout_ <- FALSE; solutions_ <- TRUE; rmarkdown::render("$<", output_file="$@")' $< && rm _output.yaml  

day_%-handout.pdf: day_%.Rmd styles/handout_output_yaml styles/handout-preamble.tex styles/auth+date.yaml
	cp styles/handout_output_yaml _output.yaml && $(RDO) 'handout_ <- TRUE; solutions_ <- FALSE; rmarkdown::render("$<", output_file="$@")' && rm _output.yaml

day_%-slides.pdf: day_%.Rmd recap.tex styles/beamer_output_yaml styles/beamer-preamble.tex styles/auth+nodate.yaml
	cp styles/beamer_output_yaml _output.yaml && $(RDO) 'handout_ <- FALSE; solutions_ <- FALSE; rmarkdown::render("$<", output_file="$@")' && rm _output.yaml

day_5-slides.pdf: day5_noncompliance_attrition.Rmd child_files_/ivmodel_and_weak_instruments.Rmd styles/icpsr_beamer_output_yaml styles/icpsr-beamer-template defs-all.sty
	cp styles/icpsr_beamer_output_yaml _output.yaml && $(RSCRIPT) -e 'handout_ <- FALSE; solutions_ <- FALSE; rmarkdown::render("$<", output_file="$@")' && rm _output.yaml

day_5-wmn.pdf: day5_noncompliance_attrition.Rmd child_files_/ivmodel_and_weak_instruments.Rmd styles/daily-announcement.tex styles/icpsr_wmn_output_yaml styles/wmn-preamble.tex defs-all.sty
	cp styles/icpsr_wmn_output_yaml _output.yaml && $(RSCRIPT) -e 'handout_ <- FALSE; solutions_ <- TRUE; rmarkdown::render("$<", output_file="$@")' $< && rm _output.yaml  

day_5-handout.pdf: day5_noncompliance_attrition.Rmd child_files_/ivmodel_and_weak_instruments.Rmd styles/icpsr_handout_output_yaml styles/handout-preamble.tex defs-all.sty
	cp styles/icpsr_handout_output_yaml _output.yaml && $(RSCRIPT) -e 'handout_ <- TRUE; solutions_ <- FALSE; rmarkdown::render("$<", output_file="$@")' && rm _output.yaml
