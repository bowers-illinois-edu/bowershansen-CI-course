##' ---
##' title: "Day 9 (June 25): Forming and Evaluating a Matched Design"
##' output: github_document
##' ---
##'
##' This script reproduces every quantity, code block, table, and figure in
##' 2026-06-25.tex. It follows Sections 2.2.4-2.2.5 of Leavitt and Miratrix
##' (2026): forming the matches with optmatch::fullmatch() and reporting the
##' design's effective sample size, then covariate balance (the love plot and
##' standardized differences) and the Hansen and Bowers (2008) omnibus balance
##' test under both an exact (permutation) and an asymptotic (chi-squared)
##' reference distribution. The effective-sample-size principles (the harmonic
##' mean and ratio constraints) are introduced in 2026-06-24.R.
##'
##' Section 0 reconstructs the design ingredients built in the companion script
##' 2026-06-24.R: the UN peacekeeping data, the covariate set, the estimated
##' propensity score / linear covariate index, and the exact-match-on-region
##' constraint. Distances and rules for comparability are derived there in full.
##'
##' The dataset peace_pre_match.rds (Gilligan and Sergenti 2008, cleaned) is
##' sourced directly from the matching-guide GitHub repository
##' (https://github.com/tl2624/matching-guide).
##'
##' Figures produced (written to the figures/ subdirectory):
##'   * love_plot.pdf                  (covariate balance before vs. after matching)
##'   * cov_balance_exact_normal.pdf   (one covariate: exact dist. vs. Normal)
##'   * omnibus_exact_chisq.pdf        (omnibus statistic: exact dist. vs. chi-sq.)

## ============================================================
## Setup
## ============================================================
## Set saveplots_ <- TRUE to (re)write the figure PDFs the slides include.
if (!exists("saveplots_")) saveplots_ <- FALSE

library(dplyr)     # data manipulation (mutate, filter, select)
library(optmatch)  # match_on(), exactMatch(), fullmatch(), effectiveSampleSize()
library(RItools)   # balanceTest(): standardized differences and omnibus test
library(ggplot2)   # the love plot and the balance-test figures

## A seed is needed for the Monte Carlo permutation balance test in Section 4.
set.seed(seed = 12345)

## Figures are written to the figures/ subdirectory, which the slides source.
dir.create("figures", showWarnings = FALSE)

## Running this script via Rscript draws each print()ed plot to R's default
## device, which creates a stray Rplots.pdf. Open a null device in
## non-interactive runs so that file is never written (ggsave() is unaffected).
if (!interactive()) pdf(NULL)

## ============================================================
## 0. Design ingredients carried over from Day 8 (2026-06-24.R)
## ============================================================
## 87 post-conflict peace episodes (Gilligan and Sergenti 2008). Treatment UN =
## 1 if a UN mission was present, 0 otherwise (19 treated, 68 control). We take
## the authors' substantive covariate transformations (logs, scales) as given.
data <- readRDS(url("https://raw.githubusercontent.com/tl2624/matching-guide/main/data/peace_pre_match.rds"))

## The 9 covariates we match on. cov_fmla keeps region as a factor; match_on()
## and balanceTest() expand it automatically.
covs     <- c("lwdeaths", "lwdurat", "ethfrac", "pop", "lmtnest", "milper",
              "bwgdp", "bwplty2", "region")
cov_fmla <- reformulate(termlabels = covs, response = "UN")

## Estimated propensity score by logistic regression. Region is excluded because
## some regions (near-)perfectly predict treatment (separation). The linear
## predictor is each unit's estimated linear covariate index = logit of the PS.
psm_cov_fmla <- reformulate(
  termlabels = setdiff(x = covs, y = "region"),
  response = "UN"
)
psm <- glm(
  formula = psm_cov_fmla,
  family = binomial(link = "logit"),
  data = data
)
lin_cov_inds <- psm$linear.predictors   # estimated linear covariate index
p_scores     <- psm$fitted.values       # estimated propensity scores

## Exact match on region: 0 within a region, Inf across regions. Built on Day 8
## as one of the rules that forbid non-comparable matches; reused here.
em_region <- exactMatch(x = UN ~ region, data = data)

## ============================================================
## 1. Forming the matches
## ============================================================
## The design we carry forward: rank-based Mahalanobis distance, a caliper of
## 0.5 SD on the logit propensity score, an exact match on region, and direct
## calipers on ethnic fractionalization (ethfrac) and logged GDP (bwgdp).

## Caliper on the logit propensity score = 0.5 population SD of the index.
data$logit_p_score <- lin_cov_inds
pop_sd_logit <- sqrt(mean((data$logit_p_score - mean(data$logit_p_score))^2))

ps_mat <- match_on(
  x = UN ~ logit_p_score,
  caliper = 0.5 * pop_sd_logit,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

## Rank-based Mahalanobis distance: less sensitive to outliers and scale.
rank_mah_mat <- match_on(
  x = reformulate(
    termlabels = setdiff(x = covs, y = "region"),
    response = "UN"
  ),
  data = data,
  standardization.scale = NULL,
  method = "rank_mahalanobis"
)

## Direct calipers on ethfrac (35) and bwgdp (2).
eth_mat <- match_on(
  x = UN ~ ethfrac,
  caliper = 35,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

bwgdp_mat <- match_on(
  x = UN ~ bwgdp,
  caliper = 2,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

## Full matching on the combined distance structure: no more than 4 controls
## per treated unit (min.controls = 0 by default). Pair matching would instead
## use pairmatch() or min.controls = max.controls = 1.
fm <- fullmatch(
  x = ps_mat + rank_mah_mat + eth_mat + bwgdp_mat + em_region,
  data = data,
  max.controls = 4
)

## Effective sample size of the matched design, and the set-structure summary.
ess <- effectiveSampleSize(fm)
ess

summary(fm)

## Inspect one matched set: ssafrica.3. The exact match on region holds (all
## Sub-Saharan Africa); logit propensity scores are close; treated Rwanda is
## matched to 3 controls within all calipers.
data$fm <- fm
data |>
  filter(fm == "ssafrica.3") |>
  select(cname, UN, region, logit_p_score, ethfrac, bwgdp, bwplty2)

## ============================================================
## 2. Deciding whether to move ahead: covariate balance
## ============================================================
## balanceTest() compares treated and control covariate means before matching
## (the "--" / unstratified column) and after matching (stratified by the
## matched set fm). The standardized difference divides the treated-minus-
## control mean difference by the pooled SD of the covariate in the unmatched
## data, so all covariates land on one scale. Stars (p <= 0.05) come from a
## Normal approximation to the randomization distribution; with no multiplicity
## adjustment they are conservative.
cov_bal <- balanceTest(
  fmla = update(cov_fmla, . ~ . + strata(fm)),
  data = data,
  p.adjust.method = "none"
)

## Pull standardized differences before ("--") and after ("fm") matching.
std_diff_before <- cov_bal$results[, "std.diff", "--"]
std_diff_after  <- cov_bal$results[, "std.diff", "fm"]

balance_tab <- data.frame(
  covariate       = names(std_diff_before),
  std_diff_before = round(x = std_diff_before, digits = 2),
  std_diff_after  = round(x = std_diff_after,  digits = 2),
  row.names = NULL
)
balance_tab

## Love plot: absolute standardized differences, before vs. after matching, with
## the 0.1 and 0.25 rules of thumb as vertical reference lines (diagnostics, not
## thresholds). Matching pulls covariates toward 0.
love_df <- rbind(
  data.frame(covariate = names(std_diff_before),
             abs_std_diff = abs(std_diff_before), stage = "Before matching"),
  data.frame(covariate = names(std_diff_after),
             abs_std_diff = abs(std_diff_after),  stage = "After matching")
)
## Order covariates by their before-matching imbalance for a readable plot.
cov_order <- names(sort(abs(std_diff_before)))
love_df$covariate <- factor(love_df$covariate, levels = cov_order)
love_df$stage     <- factor(love_df$stage,
                            levels = c("Before matching", "After matching"))

love_plot <- ggplot(
  data = love_df,
  mapping = aes(x = abs_std_diff, y = covariate, shape = stage)
) +
  geom_vline(xintercept = c(0.1, 0.25), linetype = "dashed", color = "grey50") +
  geom_point(size = 2.4) +
  scale_shape_manual(values = c("Before matching" = 1, "After matching" = 16)) +
  xlab(label = "Absolute standardized difference") +
  ylab(label = NULL) +
  labs(shape = NULL) +
  theme_bw() +
  theme(legend.position = "top")

print(love_plot)

##+ eval=saveplots_
ggsave(
  filename = "figures/love_plot.pdf",
  plot = love_plot,
  width = 6.2,
  height = 4,
  units = "in",
  dpi = 300
)

## ============================================================
## 3. The Hansen and Bowers (2008) omnibus balance test (software)
## ============================================================
## balanceTest() also returns an omnibus chi-squared statistic that combines all
## covariates at once. Under as-if randomization (within-set randomization), it
## is asymptotically chi-squared with degrees of freedom equal to the number of
## (linearly independent) covariates. A large p-value means the matched data are
## consistent with a block-randomized experiment.
cov_bal$overall["fm", ]

## ============================================================
## 4. The omnibus statistic: exact vs. asymptotic reference distribution
## ============================================================
## To see both reference distributions from ONE statistic, we build the Hansen
## and Bowers omnibus statistic by hand on the matched data. For covariate
## matrix X (matched units), treatment z, and matched sets, the vector of sum
## statistics is t = X^T z. Under within-set randomization (m_s treated fixed in
## set s), t has a known mean and covariance; the omnibus statistic is the
## quadratic form d^2 = (t - E[t])^T V^{-1} (t - E[t]), which is asymptotically
## chi-squared. The EXACT reference distribution permutes z within sets.

matched <- data |> filter(!is.na(fm))
matched$set <- droplevels(matched$fm)

## Covariate design matrix on the matched units (region as dummies, intercept
## dropped). A generalized inverse handles any rank deficiency.
x_mat <- model.matrix(
  object = ~ lwdeaths + lwdurat + ethfrac + pop + lmtnest + milper +
    bwgdp + bwplty2 + region,
  data = matched
)[, -1]

z_obs  <- matched$UN
blocks <- matched$set

##' Null mean and covariance of t = X^T z under within-set randomization.
##'
##' @param x_mat matched-unit covariate matrix (rows = units).
##' @param block matched-set membership (one entry per unit).
##' @param z treatment indicator (used only for the per-set treated counts).
##' @return list with the null mean vector e_t and covariance matrix v_t.
null_moments <- function(x_mat, block, z) {
  k   <- ncol(x_mat)
  e_t <- numeric(k)
  v_t <- matrix(data = 0, nrow = k, ncol = k)
  for (b in unique(block)) {
    idx  <- which(block == b)
    x_b  <- x_mat[idx, , drop = FALSE]
    n_b  <- length(idx)
    p_b  <- sum(z[idx]) / n_b                       # treated share in set b
    e_t  <- e_t + crossprod(x_b, rep(x = p_b, times = n_b))
    ## Covariance of a within-set permutation of m_b ones among n_b positions.
    c_b  <- p_b * (1 - p_b) * (n_b / (n_b - 1)) *
      (diag(n_b) - matrix(data = 1, nrow = n_b, ncol = n_b) / n_b)
    v_t  <- v_t + t(x_b) %*% c_b %*% x_b
  }
  return(list(e_t = as.numeric(e_t), v_t = v_t))
}

##' Omnibus statistic d^2 for a given assignment z.
omnibus_d2 <- function(z, x_mat, e_t, v_t_inv) {
  t_vec <- crossprod(x_mat, z)
  dev   <- t_vec - e_t
  as.numeric(t(dev) %*% v_t_inv %*% dev)
}

moments <- null_moments(x_mat = x_mat, block = blocks, z = z_obs)
v_t_inv <- MASS::ginv(moments$v_t)

## Degrees of freedom = number of non-degenerate covariate dimensions. Region is
## constant within sets (the exact match), so its dummies have zero variance and
## contribute nothing; df is the count of non-zero eigenvalues of V (here 8).
## This matches the df reported by balanceTest. (qr()$rank over-counts here
## because of floating-point tolerance, so we count eigenvalues directly.)
v_t_eigen <- eigen(moments$v_t, symmetric = TRUE, only.values = TRUE)$values
df_omni   <- sum(v_t_eigen > max(v_t_eigen) * 1e-8)

## Observed sum-statistic vector t = X^T z, the omnibus statistic, and its
## asymptotic (chi-squared) p-value.
t_obs  <- as.numeric(crossprod(x_mat, z_obs))
d2_obs <- omnibus_d2(z = z_obs, x_mat = x_mat, e_t = moments$e_t,
                     v_t_inv = v_t_inv)
asym_p <- pchisq(q = d2_obs, df = df_omni, lower.tail = FALSE)

## Exact (Monte Carlo) reference distribution: permute z within each matched set,
## holding the per-set treated count fixed. We store the whole permuted t vector
## so the SAME permutations drive both the single-covariate and omnibus tests.
permute_within_sets <- function(z, block) {
  z_new <- z
  for (b in unique(block)) {
    idx <- which(block == b)
    z_new[idx] <- sample(x = z[idx], size = length(idx), replace = FALSE)
  }
  return(z_new)
}

n_sims <- 10000
perm_t <- replicate(
  n = n_sims,
  expr = as.numeric(
    crossprod(x_mat, permute_within_sets(z = z_obs, block = blocks))
  )
)
## perm_t: one column per permutation, one row per covariate (sum statistics).

## Omnibus d^2 for every permutation: the quadratic form, column by column.
perm_dev <- perm_t - moments$e_t                    # recycle E[t] down each column
d2_perm  <- colSums(perm_dev * (v_t_inv %*% perm_dev))
exact_p  <- mean(d2_perm >= d2_obs)

## --- One crucial covariate: logged war deaths (most imbalanced pre-match) ---
## The standardized balance statistic (t_k - E[t_k]) / sqrt(V_kk) is ~ N(0, 1)
## under within-set randomization. Compare its exact permutation distribution to
## that Normal approximation.
crucial_cov <- "lwdeaths"
k_idx       <- which(colnames(x_mat) == crucial_cov)
sd_k        <- sqrt(moments$v_t[k_idx, k_idx])
z_stat_obs  <- (t_obs[k_idx] - moments$e_t[k_idx]) / sd_k
z_stat_perm <- (perm_t[k_idx, ] - moments$e_t[k_idx]) / sd_k
exact_p_cov <- mean(abs(z_stat_perm) >= abs(z_stat_obs))   # two-sided

c(z_stat_obs = round(z_stat_obs, 2), exact_p_cov = round(exact_p_cov, 3))
c(d2_obs = round(d2_obs, 2), df = df_omni,
  exact_p = round(exact_p, 3), asymptotic_p = round(asym_p, 3))

## Figure: one covariate -- exact permutation distribution vs. Normal overlay.
## The dashed line marks the observed statistic; it sits near 0, so logged war
## deaths is well balanced after matching.
cov_test_df <- data.frame(z_stat = z_stat_perm)
cov_test_plot <- ggplot(data = cov_test_df, mapping = aes(x = z_stat)) +
  geom_histogram(mapping = aes(y = after_stat(density)), bins = 40,
                 fill = "grey80", color = "white") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 1) +
  geom_vline(xintercept = z_stat_obs, linetype = "dashed") +
  annotate(geom = "text", x = z_stat_obs, y = 0.43, label = "observed",
           hjust = -0.1, size = 3) +
  xlab(label = "Standardized balance statistic: logged war deaths") +
  ylab(label = "Density") +
  theme_bw()

print(cov_test_plot)

##+ eval=saveplots_
ggsave(
  filename = "figures/cov_balance_exact_normal.pdf",
  plot = cov_test_plot,
  width = 6,
  height = 3.6,
  units = "in",
  dpi = 300
)

## Figure: omnibus statistic -- exact permutation distribution vs. chi-squared
## overlay (df = number of non-degenerate covariates). The dashed line marks the
## observed d^2; it sits in the body of the distribution, so balance is good.
omni_test_df <- data.frame(d2 = d2_perm)
omni_test_plot <- ggplot(data = omni_test_df, mapping = aes(x = d2)) +
  geom_histogram(mapping = aes(y = after_stat(density)), bins = 40,
                 fill = "grey80", color = "white") +
  stat_function(fun = dchisq, args = list(df = df_omni), linewidth = 1) +
  geom_vline(xintercept = d2_obs, linetype = "dashed") +
  annotate(geom = "text", x = d2_obs, y = 0.115, label = "observed",
           hjust = -0.1, size = 3) +
  xlab(label = expression(paste("Omnibus balance statistic  ", italic(d)^2))) +
  ylab(label = "Density") +
  theme_bw()

print(omni_test_plot)

##+ eval=saveplots_
ggsave(
  filename = "figures/omnibus_exact_chisq.pdf",
  plot = omni_test_plot,
  width = 6,
  height = 3.6,
  units = "in",
  dpi = 300
)

## ============================================================
## 5. Key numbers behind the slides
## ============================================================
cat("Population SD of the logit propensity score:   ",
    round(pop_sd_logit, digits = 2), "\n")
cat("Caliper of 0.5 SD on the logit scale:          ",
    round(0.5 * pop_sd_logit, digits = 2), "\n")
cat("Effective sample size of the matched design:   ",
    round(ess, digits = 2), "\n")
cat("lwdeaths standardized balance statistic:       ",
    round(z_stat_obs, digits = 2), "\n")
cat("lwdeaths exact two-sided balance p-value:      ",
    round(exact_p_cov, digits = 3), "\n")
cat("Omnibus balance d^2:                           ",
    round(d2_obs, digits = 2), " on ", df_omni, " df\n")
cat("Omnibus exact (permutation) p-value:           ",
    round(exact_p, digits = 3), "\n")
cat("Omnibus asymptotic (chi-squared) p-value:      ",
    round(asym_p, digits = 3), "\n")
