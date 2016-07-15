## Unit 1, Documenting causation

### To build on or extend from prior courses

N/A, but course prereqs:

- formal logic, enough to identify valid/not valid math proofs/deductive arguments 
-  Conditional probability and conditional expectation (you can fill this in, but it'll take additional self-study)
- experience using a command-based computer program (as opposed to a menu-driven one)
- willingness to learn R  (if it's new to you, consider taking the course)
- willingness to learn markdown (you can do this on your own)


### To be introduced in this unit
#### examples

- Fisher tea-tasting
- Salk vaccine trials
- dolphins(?)
- Arceneaux et al 2010 experiment/matched study combo

#### assumptions

- The assignment of units of study to treatment conditions is (considered to be) a random process

#### techniques 

- permutation-based p-values
     - what are they?
     - basic examples
     - what they do & don't assume
- reading and re-running R code from a script
- **ITT analysis? or is this later?

#### technical concepts

- Random assignment
- Comparative studies timeline
- Hypothesis of absolutely no effect
 - timeline of comparative studies & associated distinctions (covariates vs "baseline" vs outcomes variables)
 

#### broader concepts

- Causal relativity: Documenting causation calls for comparison.  Whether the comparison of two groups is legitimate, apples-to-apples, is only partly a function of how similar the two groups are; it's also contextual:
- 	- random assignment 
	- how much is known about baseline characteristics
	- sample size
	- ...
- Widely agreed that RCTs suffice to establish causation in social and medical science.  Less agreement re whether an RCT is necessary, importance of merely establishing causation.

### If it fits in....

- Three trends in quantitative analysis
    - social physics (19th c)
    - sampling of populations (20th c)
    - modern causal inference (   )
- A modern causal perspective may change your views on:
    - "the race effect was..."
    - "the intervention effect was..."
    - omitted variable bias
    - generalizing from sample to population
    - non-compliance (with a designated intervention)
    - non-independence of outcomes (we'll develop some new concepts for thinking about this)
    - what quantitative analysis does and doesn't need theory for
    - what theory does and doesn't need quantitative analysis for
    
## Unit 2
## To build on/extend from prior units

salk trial NFIP study (CACE estimation)

### To be introduced in this unit

#### examples
acorn

#### assumptions
- complete random assignment
- no spillovers between units
- exclusion (where appropriate)

#### techniques
- Expected value and variance
- Aggregate by cluster
- calculate expected values from a response schedule
- calculate variances from a response schedule

### technical concepts
- structural characteristics of experiments & random assignment: 
    - common properties of randomized designs: complete, simple, balanced
    - paired comparisons (? or next unit?)
    - unit of assignment vs unit of measurement; clustering
 -   potential outcomes schedules
- Fundamental Problem of Causal Inference (**2016**)
- With clusters, relevant "sample size" is the number of clusters, not the number of elements.
- Exclusion
    - procedures to ensure exclusion
    - T>=0 issues, as opposed to omitted covariate problem in obs studies

#### broader concepts
 - ACEs vs FACEs
 - SATEs vs PATEs
 - ITT vs per-protocol
 - ITT vs CACE
 -   If there are clusters, proper SEs will have to attend carefully to them. Either aggregate data, or make sure your formula/computer program is attending to clustering
 -  
### If it fits in




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


Statistical assumptions made throughout this unit:

- The assignment of units of study to treatment conditions is
      - random
      - governed by known probabilities of assignment


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

## Unit Template

### To build on/extend from prior units

### To be introduced in this unit

#### examples
#### assumptions
#### techniques
#### broader concepts

### If it fits in
