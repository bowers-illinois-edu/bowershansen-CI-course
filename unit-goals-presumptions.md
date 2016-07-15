## Unit 1, Documenting causation

Learning goals

- Experiments are well suited to documenting causation
    - direct & direct evidence
    - if you find something, supports argument of form _either_ treatment caused the effect _or_ ...
- Roles of some key tools of experiment design in statistics
    - control groups
    - random assignment
- Essentials of the conceptual framework
    - timeline of comparative studies & associated distinctions (covariates vs "baseline" vs outcomes variables)
    - potential outcomes notation and framework
    - Fundamental Problem of Causal Inference (**2016**)
    - ACEs
- permutation-based p-values
     - what are they?
     - basic examples
     - what they do & don't assume
- Expected value and variance

Statistical assumptions made throughout this unit:

- The assignment of units of study to treatment conditions is modeled as a random process


Secondary learning goals:

- Three trends in quantiative analysis
    - social physics (19th c)
    - sampling of populations (20th c)
    - modern causal inference (   )
- Identify key issues that modern causal inference sees differently than earlier traditions in quantitative analysis:
    - "the race effect was..."
    - "the intervention effect was..."
    - omitted variable bias
    - generalizing from sample to population
    - non-compliance (with a designated intervention)
    - non-independence of outcomes (we'll develop some new concepts for thinking about this)
    - what quantitative analysis does and doesn't need theory for
    - what theory does and doesn't need quantitative analysis for

## Unit 2


Learning goals

- Relevant mechanics of random assignment
    - common properties of randomized designs: complete, simple, balanced
    - paired comparisons
    - unit of assignment vs unit of measurement; clustering
    - To get the SE right, make sure you're dividing by $n$, num clusters, not $m$, a number of elements. <!-- In winter 2016 I learned that this isn't widely appreciated among stats PhD students (BH) -->

- Exclusion and intention-to-treat
    - Intention-to-treat vs effect on treated
    - procedures to ensure exclusion
    - T>=0 issues, as opposed to omitted covariate problem in obs studies

- Expected value
- Unbiasedness
-  Conditional probability and conditional expectation
- ACEs and FACEs
- Intention-to-treat vs per-protocol
- Unbiased estimators may or may not have Normal sampling dist'ns
- Unlearn $\mathrm{s.e.}(\bar p) = \bar p(1-\bar p)/n$, since $s^2/n$ gives the same answer when it's valid and otherwise generalizes better.


Statistical assumptions made throughout this unit:

- The assignment of units of study to treatment conditions is
      - random
      - governed by known probabilities of assignment
- Non-interference, between units of assignment


## Unit 3, Fisherian randomization inference & models of effects

Basic learning goals
- rerandomization to generate reference distributions, and thus p-values for arbitrary test statistics
- Confidence intervals by inversion
- rerandomization lets you associate p-values with arbitrary test statistics
- "robust" testing and its importance for power
-  p-values from simulation in R

special topics & by-products:
- What the problem of weak instruments is, and why it's so much easier to deal with from the Fisherian perspective
- non-interference is presumed by Y_t/Y_c notation, but not necessarily by an incomplete response schedule
- interference $\neq$ non-independence.  (Lots of non-independence among outcomes can be wrapped up into baseline differences, and doesn't require us to permit presence of interference.)
-  Maybe $ \bar p(1-\bar p)/n$ has a usage after all: error of approximation of p-value simulation).

## Unit 04, baseline comparability and blocking

(Interesting unit template possibility: examples, assumptions, issues.)


- Baseline comparability
  - Randomization tests
	  + Multiplicity issue
	  + Mahal dist-based check of Hansen and Bowers 2008
	  + Unstratified vs pair matched examples
	  + for designs w/ blocks, shuffle w/in strata
  - Stratified and post-stratified experiments and natural experiments
  
  - NAs in a covariate (reference DOS ch 9)
	    + missingness as data
	    + (MAR vs MCAR)
- Blocking and poststratification
	-  pairs, blocks as a generalization of pairs

- Searching for natural experiments
	- Stepwise intersection-union principle and balance checking

- NOT HERE BUT LATER:
- Methods of combining effect estimates across poststrata:
  - "fixed effects"
  - standardization with a standard population
  - Inverse probability weighting
  - Variant of IPW to target the effect of treatment on treated