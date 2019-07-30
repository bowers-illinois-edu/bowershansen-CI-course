---
title: Day by Day Plans for ICPSR Causal Inference
date: 29 July 2019
author: |
  | ICPSR 2019 Session 2
  | Jake Bowers, Ben Hansen, Tom Leavitt
bibliography:
 - refs.bib
 - BIB/master.bib
 - BIB/misc.bib
 - ci.bib
fontsize: 10pt
geometry: margin=1in
graphics: yes
biblio-style: authoryear-comp
output:
  pdf_document:
    keep_tex: true
    latex_engine: xelatex
    citation_package: biblatex
    includes:
        in_header:
           - defs-all.sty

---

<!-- /usr/local/bin/pandoc +RTS -K512m -RTS day-plan.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output day-plan.tex --template /Library/Frameworks/R.framework/Versions/3.6/Resources/library/rmarkdown/rmd/latex/default-1.17.0.2.tex --highlight-style tango --pdf-engine xelatex --biblatex --include-in-header defs-all.sty --variable graphics=yes --variable 'compact-title:yes'  ; latexmk day-plan.tex -->

This document sketches out how topics and readings and assignments map to days
of the class. The syllabus itself has more readings associated with any given
topic. In regards readings --- we have chosen material that we think is
relatively easy to read over more technical pieces. And we have not tried to be
comprehensive, but rather have chosen a few articles or chapters that, we
think, introduce major conceptual approaches in ways that social scientists
might  find particularly useful.

## Day 1 and Day 2: (Unit 1) (Ben)
 - The idea of a sharp null test.
    + Fisher's hypothesis of absolutely no effect
    + Appraising evidence vs a sharp null by comparing realized value to its distribution
    + For large sample RCTs, these distributions characterized by CLT + probability reasoning, stat'l formulas
    + For small sample RCTs, distributions can be characterized by probability calculations and/or simulation
 - Potential outcomes
 - Coffee Experiment
 - Comparative studies timeline

### Readings about Experiments and Causal Inference:

 - Section~1.2, ``Experimentation defined,''  of @kinder1993experimental.  (Particularly pp. 5--10.)

 - @holland:1986a, Sections~1--4. (The article that brought the ``Rubin Causal Model'' to  statisticians' attention.)

 - @rosenbaum10book, Chapter 1


## Day 3: P-values. (Unit 2) (Tom)
 - Review of Expectations, Probability
 - Probability distribution on Omega (assignments) forms the basis of p-values
 - Gesture at estimation.
 - Calculate exact p-values and simulated p-values and then normal approximations under the sharp null.
 - Why is area in the tail evidence against the null?
 - Effect increasing test statistic.
 - Why are p-values a good measure of evidential support?

### Readings on the testing approach to causal inference:

 - @fisher:1935, Chapter 2
 - @rosenbaum10book, Chapter 2

## Day 4: Unbiased Estimation (Unit 3) (Tom)
 - Expected value of the estimates (the 8 choose 4 Omega)
 - Proof of unbiasedness.

### Reading for discussions about the estimation approach  to causal inference:
 - @gerbergreen2012, Chapter 1--3.
 - Endnote spanning pages A-32 and 33,
  @freedman:purv:pisa:1998.  (This can be read as a precis
  of: @neyman:1990.)

## Day 5: Consistency and Precision of the Difference-Of-Means estimator of the ATE (Tom)
 - Why we care about standard errors. (And estimator of the variance).
 - Conservative variance estimation.
 - And unbiased estimators of $var(y_t)$ and $var(y_c)$.
 - Normality assumption (the 1.96 finite population CLT). (70 different estimated intervals).
 - Hypothesis testing: posit weak null and assume estimator / test statistic is normal. (estimator as test statistic).
 - Fisher versus Neyman (sharp null vs weak null): for Fisher we know E(t) and Var(t), for Neyman we Know E(t), but V(t) we estimate and rely on a normal approximation.
 - Relationship between finite population CLT.
 - **Assignment is due next Thursday**


## Day 6: Monday (Unit 5)
 - Context for last week.
 - By now, we have a good sense of the statistics for causal quantities in
   randomized experiments: when the desired relationship is between treatment
assignment  and outcome.
 - Next step away from the randomized experiments: Instrumental variables (Intro)

### Readings about Instruments for Causal Inference:
  - @angrist1996 including the @rosenbaum1996 discussion
  - @gerbergreen2012, Chapter 5.
  - @rosenbaum10book, Chapter 5.3 (on "Instruments")
  - Section 2.3 of @rosenbaum:2002a.
  - Chapter~1, ``Introduction,'' of @fisher:1935.

## Day 7: IV, Blooms Estimator (Ben and Tom).
  - Give out assignment 2 (`unit02-Ex`)
  - Instrumental Variables
  - Attrition (notice this strategy).
  - Start propensity scores and matching.

## Day 8: IV and IV Testing
  - Recap IV estimation of the CACE (with 1 sided non-compliance)
  - Testing the sharp null of no effect on the compliers under the IV assumptions (with 2-sided non-compliance)

## Day 9: Statistical Adjustment in Observational Studies
  - The problem of using a linear model (like least squares) for adjustment
  - The idea of assessing the randomization in a randomized experiment
  - The idea of assessing balance or "as-if-randomized" claims in an observational study
  - The basics of matching --- on a single variable.

### Readings about the principled search for designs with ignorable assignment
 - Chapter 1 of @rosenbaum10book
 - Chapter 6 of @dunning2012natural
 - @hansenSales2015cochran
 - @hansen:bowers:2008 (On Balance Testing)

### Readings about  the problems of adjustment using the linear model
 - @gelman2006dau, Section 9.5 (on interpolation and extrapolation)
 - @simonsohn2015specification and @ho:etal:07 on model dependence when using the linear model for adjustment

### Readings about multivariate matching
 - @rosenbaum10book, Chapters 8--9, 13
 - @hans:04 (on Full, Optimal Matching)
 - @gelman2008weakly  (on problems of using the logistic regression to create propensity scores)

### Readings on estimation and testing given matched designs
 - @gerbergreen2012 on Block-Randomized Designs (for estimation)
 - @rosenbaum10book Chapters 8--9, 13

## Day 10:
  - The problem of using a linear model (like least squares) for adjustment
  - Introduce matching on multiple variables: Mahalanobis distance

## Day 11: Monday
  - Basic forms of matching: greedy,optimal, replacement, full.
  - Matching on more than one variable, approaches: mahalanobis distance and propensity scores.

## Day 12:
  - Hand out last assignment on matching. (assignment 3)
  - Exact matching; Calipers; Combining matrices;

## Day 13:
   - Effective sample size; SIUP for balance testing; searching for a good
     match using balance, effective sample size, as well as max dist on
important covariate.
   - Estimation and Testing after matching; Plus more on  Navigating
     implementation tradeoffs in matching: Information and blocked designs
(some blocking have more information about treatment effects than others);
Balance tests and the Sequential Intersection Union Principle

## Day 14:
 - Non-bipartite matching (cross-sectional)

### Reading on matching with more than one group
 - Chap 11 \& 12, of @rosenbaum2010design
 - @lu2011optimal
 - @wong2012jop (for an example)

## Day 15: Monday
 - (Regression) Discontinuity Designs

###  Readings about (Regression) Discontinuity Designs

 - @lee08
 - @mccrary2008manipulation
 - @causek11
 - @cattaneo2014randomization
 - @saleshansen2015riforrd


## Day 16:
 - Sensitivity analysis I (Rosenbaum Style)

### Readings on sensitivity analysis
 - @rosenbaum10book, Chap 3
 - @hhh2010
 - @rosenbaumtwo

## Day 17:
 - Sensitivity Analysis II (Calibrated Style)

## Day 18:
 - Maybe Interference

### Readings about causal inferences when units interact and causal effects flow across networks
 - @bowersetal2013; @bowers2016research
 - @gerbergreen2012, Chapter 8 
 - @ichino2012deterring (for an example) 
 - @aronow2013estimating
 - @liu2014large


## Day 19:
 - Free Discussion or project work

# References
