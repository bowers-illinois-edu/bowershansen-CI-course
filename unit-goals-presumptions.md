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

- Exclusion and intention-to-treat
    - Intention-to-treat vs effect on treated
    - procedures to ensure exclusion
    - T>=0 issues, as opposed to omitted covariate problem in obs studies

Statistical assumptions made throughout this unit:

- The assignment of units of study to treatment conditions is
      - random
      - governed by known probabilities of assignment
- Non-interference, between units of assignment


## Unit 3, Fisherian randomization inference & models of effects

Learning goals

- Blocks as a generalization of pairs!!
- Baseline comparability/Mahal dist!!
- Causal vs descriptive parameters
-   Most statistical models mainly serve to estimate descriptive parameters. I.e., distinction between "effect" in sense of an estimated regression parameter and "effect" in sense of what would have happened had vaccine been withheld.
- Y_t, Y_c
- non-interference is presumed by Y_t/Y_c notation
- Expected value
- Unbiasedness
-  Conditional probability and conditional expectation
- ACEs and FACEs
- Intention-to-treat vs per-protocol
- Unbiased estimators may or may not have Normal sampling dist'ns
- Unbiased estimators may or may not have constant variances
-  p-values from simulation in R
-  error of approximation of p-value simulation
