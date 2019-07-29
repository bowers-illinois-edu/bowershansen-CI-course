These are the day by day plans for the course. See unit-goals-presumptions.md for broader visions.

# 2019

## Day 1 and Day 2: (Unit 1)
 - The idea of a sharp null test.
    + Fisher's hypothesis of absolutely no effect
    + Appraising evidence vs a sharp null by comparing realized value to its distribution
    + For large sample RCTs, these distributions characterized by CLT + probability reasoning, stat'l formulas
    + For small sample RCTs, distributions can be characterized by probability calculations and/or simulation 
 - Potential outcomes
 - Coffee Experiment
 - Comparative studies timeline 

## Day 3: P-values. (Unit 2)
 - Review of Expectations, Probability
 - Probability distribution on Omega (assignments) forms the basis of p-values
 - Gesture at estimation.
 - Calculate exact p-values and simulated p-values and then normal approximations under the sharp null.
 - Why is area in the tail evidence against the null?
 - Effect increasing test statistic.
 - Why are p-values a good measure of evidential support?

## Day 4: Unbiased Estimation (Unit 3)
 - Expected value of the estimates (the 8 choose 4 Omega)
 - Proof of unbiasedness.
 
## Day 5: Consistency and Precision of the Difference-Of-Means estimator of the ATE
 - Why we care about standard errors. (And estimator of the variance).
 - Conservative variance estimation.
 - And unbaised estimators of var(y_t) and var(y_c).
 - Normality assumption (the 1.96 finite population CLT). (70 different estimated intervals).
 - Hypothesis testing: posit weak null and assume estimator / test statistic is normal. (estimator as test statistic).
 - Fisher versus Neyman (sharp null vs weak null): for Fisher we know E(t) and Var(t), for Neyman we Know E(t), but V(t) we estimate and rely on a normal approximation.
 - Relationship between finite population CLT.

 - **Assignment 1** is due Tuesday.

## Day 6: (Unit 5)
 - Context for last week.
  - What does it mean to make a counterfactual causal inference?
  - When you selected this course, what did you expect that we would
    cover/discuss?
  - Why do you think we are talking about randomized studies? Or about
    $p$-values and estimators?
 - Some time on the assignment.
 - By now, we have a good sense of the statistics for causal quantities in
   randomized experiments: when the desired relationship is between treatment
assignment  and outcome. There is more to learn, especially about block
randomized experiments, but we will come back to the statistics for now and instead begin to move toward the question of creating and justifying interpretable comparisons from creating a justifying statistical procedures to help us formalize intuitions about causal inferences.
 - Next step away from experiments: Instrumental variables (Intro)

## Day 7: IV, Blooms Estimator
  - Give out assignment 2 (unit02-Ex)
  - Instrumental Variables
  - Attrition  (??) (notice this strategy).
  - Start propensity scores and matching.


## Day 8: IV and IV Testing
  - Recap IV estimation of the CACE (with 1 sided non-compliance)
  - Testing the sharp null of no effect on the compliers under the IV assumptions (with 2-sided non-compliance)


## Day 9: Statistical Adjustment in Observational Studies
  - The problem of using a linear model (like least squares) for adjustment
  - The idea of assessing the randomization in a randomized experiment
  - The idea of assessing balance or "as-if-randomized" claims in an observational study
  - The basics of matching --- on a single variable.

## Day 10:
  - The problem of using a linear model (like least squares) for adjustment
  - Introduce matching on multiple variables: Mahalanobis distance

## Day 11:
  - Recap of last week.
  - Maybe sometime on Assignment 2
  - Basic forms of matching: greedy,optimal, replacement, full.
  - Matching on more than one variable, approaches: mahalanobis distance and propensity scores.

## Day 12:
  - Hand out last assignment on matching. (assignment 3)
  - Exact matching; Calipers; Combining matrices;

## Day 13:
   - Effective sample size; SIUP for balance testing; searching for a good match using balance, effective sample size, as well as max dist on important covariate.

   - Estimation and Testing after matching; Plus more on  Navigating implementation tradeoffs in matching: Information and blocked designs (some blockings have more information about treatment effects than others); Balance tests and the Sequential Intersection Union Principle

## Day 14:
 - Non-bipartite matching (cross-sectional)

## Day 15: Monday
 - Recap of last week
 - Questions arising from the assignment
 - RDD

## Day 16:
 - Sensitivity analysis I (Rosenbaum Style)

## Day 17:
 - Sensitivity Analysis II

## Day 18:
 - Interference

