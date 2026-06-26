##' ---
##' title: "Day 8 (June 24): A Model of an Observational Study; Matching for Comparability and Effective Sample Size"
##' output: github_document
##' ---
##'
##' This script reproduces every quantity, code block, table, and figure in
##' 2026-06-24.tex. It follows Sections 2.1-2.2.3 of Leavitt and Miratrix
##' (2026): the UN peacekeeping running example, distance measures (Euclidean,
##' Mahalanobis, the propensity score), rules for matches that ensure
##' comparability (exact matching and calipers), and the effective-sample-size
##' principles (the within-set harmonic mean and ratio constraints). Forming the
##' matches with optmatch::fullmatch() and evaluating covariate balance (the love
##' plot, standardized differences, and the Hansen and Bowers (2008) omnibus
##' test) move to the companion script 2026-06-25.R.
##'
##' The dataset peace_pre_match.rds (Gilligan and Sergenti 2008, cleaned) is
##' sourced directly from the matching-guide GitHub repository
##' (https://github.com/tl2624/matching-guide).
##'
##' Section 1 is a small numeric demonstration (no figure) that conditioning on a
##' covariate value, and acting as if completely randomized, yields a uniform
##' distribution over the assignments with a fixed number treated.
##'
##' Figures produced (written to the figures/ subdirectory):
##'   * lin_cov_index_boxplot.pdf      (linear covariate index by treatment group)

## ============================================================
## Setup
## ============================================================
## Set saveplots_ <- TRUE to (re)write the figure PDFs the slides include.
if (!exists("saveplots_")) saveplots_ <- FALSE

library(dplyr)     # data manipulation (mutate, group_by, summarize)
library(optmatch)  # match_on(), exactMatch(), fullmatch()
library(ggplot2)   # the boxplot of the linear covariate index by treatment group

## Figures are written to the figures/ subdirectory, which the slides source.
dir.create("figures", showWarnings = FALSE)

## Running this script via Rscript draws each print()ed plot to R's default
## device, which creates a stray Rplots.pdf. Open a null device in
## non-interactive runs so that file is never written (ggsave() is unaffected).
if (!interactive()) pdf(NULL)

## ============================================================
## 1. As-if randomization within a stratum: a numeric demonstration
## ============================================================
## Assignment is by an independent coin per unit whose probability is a FUNCTION
## of that unit's covariate: Pr(Z_i = 1) = e(x_i). We use an arbitrary (logistic)
## function, so the assignment probability genuinely depends on x.
## plogis(u) = 1 / (1 + exp(-u)) is the logistic (inverse-logit): it maps any real
## number u to a probability in (0, 1), so it turns a linear index into a chance.
e <- function(x) plogis(-1 + 0.8 * x)   # Pr(Z_i = 1) = e(x_i): a function of x

## A "stratum" S is a set of units that share one covariate value, so they share
## one probability p = e(x). Take n_S = 4 units, all with covariate value x = 2.
x_S <- 2
n_S <- 4
p   <- e(x = x_S)                        # the common treatment probability in S
p                                        # 0.6457

## Enumerate every assignment for the stratum: the 2^n_S = 16 ways to hand out
## treatment. Each ROW of all_z is one assignment z = (z_1, ..., z_{n_S}).
all_z  <- as.matrix(expand.grid(rep(x = list(c(0, 1)), times = n_S)))
m_of_z <- rowSums(all_z)                 # number treated in each assignment z

## Probability of each assignment under independent Bernoulli(p) coins:
##   Pr(Z = z) = prod_i p^{z_i} (1 - p)^{1 - z_i} = p^{m} (1 - p)^{n_S - m}.
## It depends on z ONLY through m = number treated, not on WHICH units are treated.
pr_z <- p^m_of_z * (1 - p)^(n_S - m_of_z)
head(cbind(all_z, m = m_of_z, pr = round(pr_z, 4)))

## Now CONDITION on the number treated (m_S = m): keep the assignments with that
## many treated, and renormalize so their probabilities sum to 1. This is exactly
## "act as if completely randomized within the stratum."
m       <- 2                             # suppose we observed 2 treated
keep    <- m_of_z == m
pr_cond <- pr_z[keep] / sum(pr_z[keep])  # conditional probability of each z

## Every retained assignment has the SAME conditional probability: the common
## factor p^{m} (1 - p)^{n_S - m} cancels, leaving 1 / choose(n_S, m) -- a UNIFORM
## distribution over the choose(n_S, m) assignments with m treated.
round(pr_cond, 4)                        # all equal
1 / choose(n = n_S, k = m)               # 1/6 = 0.1667

## The unknown p never survives: a DIFFERENT covariate value gives a different p,
## but the SAME uniform conditional distribution. That cancellation is the whole
## point -- we never needed to know p, only that it is shared within the stratum.
p2  <- e(x = 5)                          # different x -> different p
pr2 <- p2^m_of_z * (1 - p2)^(n_S - m_of_z)
all.equal(pr2[keep] / sum(pr2[keep]), pr_cond)   # TRUE

## ============================================================
## 2. Running example: UN peacekeeping and post-conflict peace
## ============================================================
## 87 post-conflict peace episodes (Gilligan and Sergenti 2008). Treatment UN =
## 1 if a UN mission was present, 0 otherwise (19 treated, 68 control). Outcome
## ldur = log duration of the peace spell. We take the authors' substantive
## covariate transformations (logs, scales) as given.
data <- readRDS(url("https://raw.githubusercontent.com/tl2624/matching-guide/main/data/peace_pre_match.rds"))

c(N = nrow(data), treated = sum(data$UN), control = sum(1 - data$UN))

## The 9 covariates we match on, kept in the object covs.
covs <- c("lwdeaths", "lwdurat", "ethfrac", "pop", "lmtnest", "milper",
          "bwgdp", "bwplty2", "region")

## ============================================================
## 3. Measuring similarity: Euclidean distance
## ============================================================
## A distance maps the covariates of a treated and a control unit to a single
## nonnegative number; smaller means more similar. To compute distances "by
## hand" we first expand the region factor into 0/1 dummy indicators.
data <- data |>
  mutate(
    eeurop   = ifelse(test = region == "eeurop",   yes = 1, no = 0),
    lamerica = ifelse(test = region == "lamerica", yes = 1, no = 0),
    asia     = ifelse(test = region == "asia",     yes = 1, no = 0),
    ssafrica = ifelse(test = region == "ssafrica", yes = 1, no = 0),
    nafrme   = ifelse(test = region == "nafrme",   yes = 1, no = 0)
  )

expanded_covs <- c(
  setdiff(x = covs, y = "region"),
  "eeurop", "lamerica", "asia", "ssafrica", "nafrme"
)

## Euclidean distance between treated Liberia and control Guinea-Bissau:
## the square root of the sum of squared covariate differences.
liberia       <- data[data$cname == "Liberia", expanded_covs]
guinea_bissau <- data[data$cname == "Guinea-Bissau", expanded_covs]

euc_liberia_gb <- sqrt(sum((liberia - guinea_bissau)^2))
euc_liberia_gb   # Euclidean distance between the two countries

## Per-covariate squared differences behind the 57.44 (the slide table). lwdurat
## alone contributes ~3249, dominating the Euclidean distance by sheer scale.
euc_table <- data.frame(
  covariate = expanded_covs,
  liberia   = round(as.numeric(liberia), 2),
  guinea    = round(as.numeric(guinea_bissau), 2),
  sq_diff   = round((as.numeric(liberia) - as.numeric(guinea_bissau))^2, 2)
)
print(euc_table)

## The full treated-by-control distance matrix via optmatch::match_on(). The
## formula keeps region as a factor; match_on() expands it automatically.
cov_fmla <- reformulate(termlabels = covs, response = "UN")

dist_mat_euc <- match_on(
  x = cov_fmla,
  data = data,
  standardization.scale = NULL,  # no rescaling of covariates
  method = "euclidean"
)

## Label rows (treated) and columns (control) by country name for readability.
dimnames(dist_mat_euc) <- list(
  data$cname[data$UN == 1], data$cname[data$UN == 0]
)

## A 3-by-4 submatrix (treated rows 11-13 vs. control columns 27-30).
round(x = dist_mat_euc[11:13, 27:30], digits = 2)

## ============================================================
## 4. Mahalanobis distance
## ============================================================
## Euclidean distance depends on covariate scale and ignores correlations among
## covariates. Mahalanobis distance standardizes covariates onto a common scale
## AND adjusts for their correlations, so related covariates are not counted
## twice. This standardization is a statistical device, distinct from the
## researcher's substantive covariate transformations.
dist_mat_mah <- match_on(
  x = cov_fmla,
  data = data,
  standardization.scale = NULL,
  method = "mahalanobis"
)

## By-hand Mahalanobis for Liberia vs. Guinea-Bissau on the 8 NUMERIC covariates
## (region is handled by exact matching later). Standardizing + decorrelating
## removes lwdurat's raw-scale dominance: its contribution falls from 3249 to 0.08.
mcovs <- setdiff(x = covs, y = "region")
S_inv <- solve(cov(data[, mcovs]))
d_lib_gb <- as.numeric(data[data$cname == "Liberia", mcovs] -
                       data[data$cname == "Guinea-Bissau", mcovs])
mah_liberia_gb <- sqrt(t(d_lib_gb) %*% S_inv %*% d_lib_gb)
mah_liberia_gb                                   # 2.79

## Per-covariate Mahalanobis contributions (the slide table); they sum to 7.79.
mah_contrib <- as.numeric(d_lib_gb * (S_inv %*% d_lib_gb))
print(data.frame(covariate = mcovs, contribution = round(mah_contrib, 2)))

## Label the Mahalanobis matrix and show the same 5x4 submatrix as the slide.
dimnames(dist_mat_mah) <- list(data$cname[data$UN == 1], data$cname[data$UN == 0])
round(x = dist_mat_mah[11:15, 27:30], digits = 2)

## Caliper illustration (the slide's Inf table): forbidding any match with
## Mahalanobis distance > 3 sets those cells to Inf. Only Sierra Leone stays
## matchable to these four controls; the rest become impossible matches.
dm_sub <- round(dist_mat_mah[11:15, 27:30], digits = 2)
dm_sub[dm_sub > 3] <- Inf
dm_sub

## ============================================================
## 5. The estimated propensity score (dimension reduction)
## ============================================================
## With many covariates it is hard to find units similar on all of them (the
## "curse of dimensionality"). The estimated propensity score collapses the
## covariates into a single linear index that "separates" treated from control.
## We exclude region: some regions (near-)perfectly predict treatment, causing
## (near-)complete separation.
psm_cov_fmla <- reformulate(
  termlabels = setdiff(x = covs, y = "region"),
  response = "UN"
)

psm <- glm(
  formula = psm_cov_fmla,
  family = binomial(link = "logit"),
  data = data
)

## Each unit's estimated LINEAR covariate index = the model's linear predictor,
## which equals the logit (log odds) of the predicted treatment probability.
lin_cov_inds <- psm$linear.predictors
p_scores     <- psm$fitted.values

## Verify: linear index = log odds of the propensity score, and the propensity
## score = inverse logit of the linear index.
all.equal(lin_cov_inds, log(p_scores / (1 - p_scores)))
all.equal(p_scores, 1 / (1 + exp(-lin_cov_inds)))

## Namibia (treated) and Burundi (control) differ greatly on some covariates
## yet have nearly identical linear indices: covariates on which they differ
## (e.g. lwdurat, ethfrac) have small coefficients, those on which they agree
## (e.g. milper) have large ones.
round(x = coef(psm)[c("lwdurat", "ethfrac", "milper")], digits = 2)

## Raw covariate values behind Namibia/Burundi's near-equal linear indices, plus
## the indices themselves (close on the index's full range, about -7.5 to 2.2).
data[data$cname %in% c("Namibia", "Burundi"),
     c("cname", "UN", "lwdurat", "ethfrac", "milper")]
round(lin_cov_inds[data$cname %in% c("Namibia", "Burundi")], 2)

## Euclidean distance in the estimated propensity score: one scalar replaces all
## eight covariates. |0.74 - 0.17| = 0.57.
data$p_score <- p_scores
ps_liberia <- data$p_score[data$cname == "Liberia"]
ps_guinea  <- data$p_score[data$cname == "Guinea-Bissau"]
sqrt((ps_liberia - ps_guinea)^2)                 # 0.57
round(c(liberia = ps_liberia, guinea_bissau = ps_guinea), 2)

## Figure: distributions of the linear covariate index by treatment group.
## Some -- but not a lot of -- overlap; both groups cluster near 0.
lin_cov_index_boxplot <- ggplot(
  data = data,
  mapping = aes(x = as.factor(UN), y = lin_cov_inds)
) +
  geom_boxplot() +
  xlab(label = "UN intervention") +
  ylab(label = "Estimated linear covariate index") +
  theme_bw() +
  coord_flip()

print(lin_cov_index_boxplot)

##+ eval=saveplots_
ggsave(
  filename = "figures/lin_cov_index_boxplot.pdf",
  plot = lin_cov_index_boxplot,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

## ============================================================
## 6. Rules for comparability: exact matching and calipers
## ============================================================
## A simple illustration: (i) units may match only within the same region
## (exact match); (ii) treated and control more than 2 points apart on Polity
## (bwplty2) may not match (caliper). exactMatch() returns 0 within region and
## Inf across regions; the caliper distance is Inf beyond 2 points apart.
em_region <- exactMatch(
  x = UN ~ region,
  data = data
)

euc_dist_polity_cal_2 <- match_on(
  x = UN ~ bwplty2,
  caliper = 2,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"   # 1 covariate, so scale is not a concern
)

## Combine the two constraints by adding the distance objects: a pair is
## forbidden (Inf) if it violates EITHER constraint.
overall_dist_mat <- em_region + euc_dist_polity_cal_2

## ============================================================
## 7. Effective sample size
## ============================================================
## Effective sample size = sum over matched sets of the within-set harmonic mean
## of the numbers of treated and control units. It depends on HOW units are
## arranged across sets, not just the total matched.
harmonic_ess <- function(m, n_minus_m) {
  1 / ((1 / m + 1 / n_minus_m) / 2)
}

## With 4 total units: two matched pairs give ESS = 2; one set of 1 treated and
## 3 controls gives ESS = 1.5. Balanced sets carry more information.
ess_two_pairs    <- harmonic_ess(m = 1, n_minus_m = 1) * 2   # 2.0
ess_one_to_three <- harmonic_ess(m = 1, n_minus_m = 3)       # 1.5
c(two_pairs = ess_two_pairs, one_to_three = ess_one_to_three)

## Ratio constraints (min.controls, max.controls) shape set composition and so
## the effective sample size. Reusing overall_dist_mat from Section 6 (region
## exact match + a caliper of 2 on Polity): here at least 0.5 controls per
## treated and at most 2 controls per treated. (Output suppressed.)
invisible(
  fullmatch(
    x = overall_dist_mat,
    min.controls = 0.5,
    max.controls = 2,
    omit.fraction = NULL,
    mean.controls = NULL,
    data = data
  )
)

## ============================================================
## 8. Key numbers behind the slides
## ============================================================
## The day-8 quantities the slides report. Forming the matches and the balance
## diagnostics continue in 2026-06-25.R.
cat("Euclidean distance, Liberia vs Guinea-Bissau:       ",
    round(euc_liberia_gb, digits = 2), "\n")
cat("Mahalanobis distance, Liberia vs Guinea-Bissau:     ",
    round(mah_liberia_gb, digits = 2), "\n")
cat("Propensity-score distance, Liberia vs Guinea-Bissau:",
    round(sqrt((ps_liberia - ps_guinea)^2), digits = 2), "\n")
