# Mid-class review quizzes -- Week 3 (June 29 - July 2, 2026)

Short, easy review quizzes to run mid-session (Tom notes people tire in the
middle). Each set is 3-4 multiple-choice questions drawn from Tom's lectures of
the previous two weeks (June 15-26). A student who attended and did some reading
should get them right. Allow about 1 minute per question; works on Mentimeter or
straight off a slide.

Format: each question lists options A-D; the correct answer(s) and a one-line
explanation follow in a `>` blockquote. To project the questions without the
answers, delete the blockquote lines (every line starting with `>`).

Coverage map:

- Monday    -- randomized experiments, Fisher's exact test, potential outcomes (Jun 15-16)
- Tuesday   -- average effects, difference-in-means, unbiasedness, variance, weak-null inference (Jun 17-18)
- Wednesday -- covariate adjustment; noncompliance and the CACE; attrition (Jun 22-23)
- Thursday  -- observational studies, matching, balance, design-to-inference (Jun 24-26)

---

## Quiz 1 -- Monday: Randomization, Fisher's exact test, potential outcomes

**Q1. What does random assignment buy us? (Jun 15)**

- A. Known, objective probabilities, fixed by the design rather than by assumption
- B. A guarantee that the treated and control groups are identical
- C. A larger sample size
- D. Proof that the treatment has an effect

> **Answer: A.** Randomization's value is "a reasoned basis for inference": the
> chance of a wrong conclusion is known, and can be made small by design.

**Q2. In Fisher's test of the Lady Tasting Tea, what varies and what is held fixed? (Jun 15)**

- A. The assignment varies; the judgments (outcomes) are held fixed
- B. The judgments vary; the assignment is held fixed
- C. Both vary at once
- D. Both are fixed; only the test statistic changes

> **Answer: A.** Fisher's move: vary the assignment over all the ways it could
> have come out, holding the observed outcomes fixed. The "what if" is in the
> assignment, not the outcomes.

**Q3. The sharp null of "no effect" says that for every unit i: (Jun 15-16)**

- A. y_i(1) = y_i(0)
- B. the average of the y_i(1) equals the average of the y_i(0)
- C. y_i(1) - y_i(0) = tau, the same nonzero constant for all i
- D. the treatment effect is positive

> **Answer: A.** Sharp = pins down BOTH potential outcomes for EACH unit, here by
> setting them equal unit-by-unit. (B is the weak/average null; C is the
> constant-effect null.)

**Q4. Fisher mixes 8 cups, 4 milk-first and 4 tea-first, in random order. How many equally likely assignments are there, and what is the chance the taster gets all 4 right by luck alone? (Jun 15)**

- A. 70 assignments; chance = 1/70 (about 0.014)
- B. 8 assignments; chance = 1/8
- C. 256 assignments; chance = 1/256
- D. 16 assignments; chance = 1/16

> **Answer: A.** choose(8,4) = 70 equally likely assignments; only 1 gives all
> four correct, so the one-sided p-value is 1/70 ~ 0.014.

---

## Quiz 2 -- Tuesday: Average effects, difference-in-means, inference

**Q1. The estimand (the target) for this part of the course is the average treatment effect. It is written as: (Jun 17)**

- A. bar-tau = bar-y(1) - bar-y(0), the average of the individual effects
- B. the difference between the treated and control sample means
- C. y_i(1) - y_i(0) for a single unit i
- D. the p-value of the test

> **Answer: A.** Keep estimand vs. estimator distinct: the ATE is the target,
> defined in potential outcomes over the fixed finite population; the
> difference-in-means (B) is the estimator we use to guess it.

**Q2. Under complete random assignment, the difference-in-means estimator is ___ for the ATE: (Jun 17)**

- A. unbiased -- whatever the potential outcomes, E[tau_hat] = bar-tau
- B. biased upward
- C. consistent but never unbiased
- D. undefined unless effects are constant

> **Answer: A.** Unbiasedness comes from the assignment mechanism alone
> (E[Z_i] = n_1/N), for ANY potential outcomes.

**Q3. Neyman's variance estimator is called "conservative." Why? (Jun 18)**

- A. It drops the one term it cannot estimate (S^2_tau / N), which can only make the estimate too big, never too small
- B. It deliberately underestimates the variance
- C. It assumes the effect is the same for everyone
- D. It throws away the control group

> **Answer: A.** No unit reveals both potential outcomes, so S^2_tau is not
> estimable; dropping it gives an upper bound. Overstating the variance widens
> intervals and makes rejection harder -- never too liberal. (It is exact when
> effects are constant, conservative otherwise.)

**Q4. How do we build a 95% confidence interval for the ATE? (Jun 18)**

- A. Collect the null values tau_0 that the test does NOT reject (invert the test)
- B. Take the range of the observed outcomes
- C. Use the 2.5th and 97.5th percentiles of the data
- D. Report the treated mean plus or minus the control mean

> **Answer: A.** A confidence interval is the set of nulls we do not reject;
> inverting the Normal-approximation test yields the familiar
> tau_hat +/- 1.96 * se as a consequence, not a definition.

---

## Quiz 3 -- Wednesday: Covariate adjustment; noncompliance and the CACE

**Q1. In a randomized experiment, adjusting for a baseline (pre-treatment) covariate mainly: (Jun 22)**

- A. Cuts the estimator's variance (more power, tighter CIs) without removing bias
- B. Removes confounding bias from the estimate
- C. Requires the outcome model to be exactly correct
- D. Changes the estimand from the ATE to something else

> **Answer: A.** Randomization already delivers unbiasedness; a baseline
> covariate (blocking, a gain score, or Lin's regression adjustment) only
> tightens the randomization distribution. Inference stays design-based -- no
> outcome model is assumed correct. (ACORN: se 0.024 -> 0.020, CI now excludes 0.)

**Q2. In a GOTV experiment, Z = assigned a call, D = actually reached, Y = voted. The Complier Average Causal Effect (CACE) is estimated by: (Jun 23)**

- A. The Wald ratio: ITT_Y / ITT_D (reduced form over first stage)
- B. Comparing those reached vs. not reached (the per-protocol contrast)
- C. The difference in means of Y by assignment, ignoring D
- D. A regression of D on Z

> **Answer: A.** Both ITTs are whole-sample difference-in-means on the randomized
> Z, so the ratio recovers the Complier effect without ever labeling a Complier.
> The per-protocol contrast (B) is confounded because receipt D is not
> randomized. (Adams & Smith: 0.058 / 0.717 ~ 0.081.) This needs the exclusion
> restriction: Z affects Y only through D.

**Q3. Under one-sided noncompliance (a control unit cannot get the dose, d_i(0) = 0 for all i), which compliance types can be present? (Jun 23)**

- A. Only Compliers and Never-Takers
- B. All four types
- C. Only Always-Takers and Defiers
- D. Only Compliers

> **Answer: A.** With d_i(0) = 0 there are no Always-Takers and no Defiers by
> construction (monotonicity holds automatically), leaving Compliers (reached
> when called) and Never-Takers (never reached).

**Q4. With attrition (some outcomes missing), the "analyze as you randomized" estimator -- the difference-in-means among units that report -- is unbiased for the ATE among ___, provided reporting does not depend on assignment (r_i(1) = r_i(0)): (Jun 23)**

- A. The Always-Reporters
- B. The whole sample
- C. The Compliers
- D. The Never-Reporters

> **Answer: A.** Under no assignment-dependent attrition, reporting is a fixed
> pre-treatment attribute (like a covariate), so the reporters form one clean
> group -- the Always-Reporters -- and randomization within them identifies their
> ATE. A testable check: is the reporting rate equal across arms?

---

## Quiz 4 -- Thursday: Observational studies, matching, balance, inference

**Q1. In an observational study, "as-if randomization" means: (Jun 24, 26)**

- A. Among units with the same covariates, each has the same probability of treatment -- so within a stratum, assignment looks completely randomized
- B. The treatment was, in fact, randomized
- C. The outcome is generated at random
- D. The sample was randomly drawn from a population

> **Answer: A.** We never learn the true treatment probabilities; we only assume
> they are equal for units sharing the same covariates. Conditioning on the
> treated count then gives a uniform distribution over assignments within the
> stratum/matched set.

**Q2. The propensity score is defined as: (Jun 24)**

- A. lambda(x_i) = Pr(Z_i = 1 | x_i), the probability of treatment given the covariates
- B. The probability that the outcome equals 1
- C. The treated-minus-control difference in means
- D. The Mahalanobis distance between two units

> **Answer: A.** It collapses many covariates into a single number; since it is
> unknown, we match on its estimate (logistic regression -> inverse-logit of the
> fitted linear index).

**Q3. Why prefer Mahalanobis distance over plain Euclidean distance when matching? (Jun 24)**

- A. It standardizes each covariate onto a comparable (SD) scale and adjusts for correlations, so related covariates are not double-counted
- B. It is faster to compute
- C. It guarantees an exact match on every covariate
- D. It requires no covariance matrix

> **Answer: A.** Euclidean distance lets a large-scale covariate dominate and
> double-counts correlated covariates; Mahalanobis fixes both. (It is a
> statistical device -- substantive transforms like logs come first.)

**Q4. After matching, you check covariate balance. Which statement is correct? (Jun 25-26)**

- A. A covariate is balanced when the within-set treated-minus-control difference is near 0; a LARGE balance-test p-value is good news (we do not reject as-if randomization)
- B. A small balance-test p-value means the design succeeded
- C. Balance means the covariate predicts the outcome
- D. Balance is checked after looking at the outcomes

> **Answer: A.** Tools: standardized differences (the love plot) and an omnibus
> test (Hansen & Bowers). A large p-value says the imbalance is unremarkable
> under within-block randomization. Balance is a DESIGN check, done before seeing
> outcomes.

**Bonus (spare). Tom set up two inference frameworks for the matched design. Match each to its target: (Jun 26)**

- A. Sharp framework -> a hypothesis about every unit's outcome (e.g. a constant effect) -> exact randomization test; Weak framework -> a hypothesis about the ATE -> conservative variance + Normal approximation
- B. Sharp -> the ATE; Weak -> individual effects
- C. Both target the ATE; they differ only in software
- D. Both require the outcome model to be correct

> **Answer: A.** Sharp reconstructs outcomes and recomputes a sum statistic over
> all assignments (exact / simulated / Normal p-values, CI by inversion, HL
> estimate); weak targets the ATE with a conservative variance and a Normal test.
> Look-ahead to Monday: every p-value here assumed the assignments are equally
> likely -- a hidden confounder could break that, which is exactly what
> sensitivity analysis probes.
