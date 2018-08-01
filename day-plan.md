These are the day by day plans for the second half of the course. See unit-goals-presumptions.md for broader visions of the first half of the course.

# Day 1 and Day 2: (Unit 1)
 - The idea of a sharp null test.
 - Potential outcomes
 - Coffee Experiment

# Day 3: P-values. (Unit 2)
 - Review of Expectations, Probability
 - Probability distribution on Omega (assignments) forms the basis of p-values
 - Gesture at estimation.
 - Calculate exact p-values and simulated p-values and then normal approximations under the sharp null.
 - Why is area in the tail evidence against the null?
 - Effect increasing test statistic.
 - Why are p-values a good measure of evidential support? 

# Day 4: Unbiased Estimation (Unit 3)
 - Expected value of the estimates (the 8 choose 4 Omega)
 - Proof of unbiasedness.

# Day 5: Standard Error Estimation (Unit 4)
 - Why we care about standard errors. (And estimator of the variance).
 - Conservative variance estimation. 
 - And unbaised estimators of var(y_t) and var(y_c).
 - Interval estimation. Normality assumption (the 1.96 finite population CLT). (70 different estimated intervals).
 - Coverage probability.
 - Hypothesis testing: posit weak null and assume estimator / test statistic is normal. (estimator as test statistic).
 - Fisher versus Neyman (sharp null vs weak null): for Fisher we know E(t) and Var(t), for Neyman we now E(t), but V(t) we estimate and rely on a normal approximation.
 - Relationship between finite population CLT.
 - Mention that Observational Studies: same but unknown reference distributions for the assignment.
 - No fisher style confidence sets or testing other hypotheses other than the sharp null.
 
 - Assignment is due Tuesday.

# Day 6: (Unit 5)
 - Context for last week. 
 - Some time on the assignment.
   - Admissability: fisher is admissable but not chi.square. 
   - Question 5: 
 - By now, we have a good sense of the statistics for causal quantities in randomized experiments: when the desired relationship is between treatment assignment  and outcome.
 - Next step: Instrumental variables


# Day 7: 
  - Give out assignment 2 (unit02-Ex)
  - Instrumental Variables
  - Attrition  (??) (notice this strategy).
  - Start propensity scores and matching. 


# Day 8: Start Matching by this day.
  - 


# Day 9:

# Day 10:


# Day 11:
 - Assignment 2

# Day 11: 7-8-2017

Weak instruments; Precision from gain scores in experiments; Balance testing in experiments; Overview of rest of the course.

# Day 12: 8-8-2017

The problem of covariance adjustment to reduce "bias"/ confounding. How can we answer the question about whether we have adjusted enough. A simple approach: stratification on one categorical variable (and interaction effects). More complex: find sets that are as similar as possible in terms of a continuous variable (bipartite matching). Balance assessment after stratification.

Reading: Gelman and Hill \S~9.5 on overlap/common support.

# Day 13: 9-8-2017

Basic forms of matching: greedy,optimal, replacement, full.
Matching on more than one variable, approaches: mahalanobis distance and propensity scores.

Agenda: Matching on one covariate using optmatch; Matching on more than one covariate: mahalnobis distances, propensity scores using optmatch. Practice. Skipping discussion of fullmatching vs fixed ratio matching (like pairs), and optimal vs greedy.


# Day 14: 10-8-2017
Agenda: Exact matching; Calipers; Combining matrices; Discussion of with and without-replacement matching and optimal vs greedy matching; Maybe use those tools to improve your best matched design.


# Day 15: 11-8-2017

Effective sample size; SIUP for balance testing; searching for a good match using balance, effective sample size, as well as max dist on important covariate.

Estimation and Testing after matching; Plus more on  Navigating implementation tradeoffs in matching: Information and blocked designs (some blockings have more information about treatment effects than others); Balance tests and the Sequential Intersection Union Principle

# Day 16: 14-8-2017
Non-bipartite matching (cross-sectional)

# Day 17: 15-8-2017
? Sensitivity analysis I (Rosenbaum Style)?

# Day 18: 16-8-2017
?Sensitivity Analysis? ?RDD? (Ben takes this class)

# Day 19: 17-8-2017
Interference?


