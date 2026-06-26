##' ---
##' title: "Day 6 (June 22): Covariance Adjustment -- Precision Without Bias"
##' output: github_document
##' ---
##'
##' This script reproduces every quantity, code block, and figure in
##' 2026-06-22.tex. It continues the design-based, finite-population framework of
##' 2026-06-17.R and 2026-06-18.R: potential outcomes are fixed, randomness comes
##' only from the assignment Z, and the Difference-in-Means is the workhorse
##' estimator. Today a pre-treatment covariate is used to CUT THE VARIANCE of that
##' estimator -- without touching its unbiasedness -- in two design-based ways:
##'   (1) BLOCKING   -- randomize within blocks of similar precincts;
##'   (2) RESCALING  -- subtract the covariate (a gain score) before the test,
##'                     and its generalization, regression adjustment (Lin 2013).
##'
##' Running example: ACORN (acorn03.csv), the Kansas City 2003 GOTV experiment
##' (Arceneaux 2005): 28 precincts, complete random assignment of 14 canvassed
##' (z = 1) and 14 control (z = 0); outcome vote03 = 2003 turnout proportion.
##'
##' Figures produced here (written to the figures/ subdirectory):
##'   * acorn_blocked_vs_complete.pdf    (blocking tightens the null distribution)
##'   * acorn_original_vs_rescaled.pdf   (rescaling tightens the null distribution)
##'   * acorn_adjust_difference.pdf      (adjusted - unadjusted -> 0 as N grows)

## ============================================================
## Setup
## ============================================================
## Set saveplots_ <- TRUE to (re)write the figure PDFs the slides include.
if (!exists("saveplots_")) saveplots_ <- FALSE

library(ggplot2)
library(viridis)     # color-blind-friendly fills, matching the source figures
library(blockTools)  # optimal blocking (matched pairs) for the blocked design
library(nbpMatching) # backs blockTools' algorithm = "optimal"
library(estimatr)    # lm_lin(): Lin's regression-adjusted estimator with HC2 se
library(sandwich)    # HC2 robust variance for the lm() form of Lin's estimator

## Figures are written to the figures/ subdirectory, which the slides source.
dir.create("figures", showWarnings = FALSE)

## The estimator from Day 4: treated mean minus control mean.
diff_in_means <- function(z, y) mean(y[z == 1]) - mean(y[z == 0])

## Conservative estimator of the variance (Day 5): the two estimable arm pieces.
## It drops the unestimable -S2_tau/N term, so it estimates an UPPER BOUND on the
## true variance.
conservative_var <- function(z, y) {
  var(y[z == 1]) / sum(z == 1) + var(y[z == 0]) / sum(z == 0)
}

## Standardized test statistic against a null ATE (Day 5).
test_statistic <- function(tau_hat, se_hat, tau_0 = 0) (tau_hat - tau_0) / se_hat

alpha   <- 0.05                        # significance level
z_crit  <- qnorm(p = 1 - alpha / 2)    # 1.96, for two-sided CIs

## ============================================================
## 1. ACORN: the unadjusted Difference-in-Means (recap of Days 4-5)
## ============================================================
acorn  <- read.csv("acorn03.csv")
z_obs  <- acorn$z           # 1 = canvassed (GOTV), 0 = control
y_obs  <- acorn$vote03      # observed 2003 turnout proportion
N  <- length(z_obs)
n1 <- sum(z_obs)
n0 <- N - n1                # 28, 14, 14

tau_hat <- diff_in_means(z = z_obs, y = y_obs)            # ~ 0.036
se_hat  <- sqrt(conservative_var(z = z_obs, y = y_obs))   # ~ 0.024
ci_unadj <- c(lower = tau_hat - z_crit * se_hat,
              upper = tau_hat + z_crit * se_hat)          # ~ [-0.011, 0.084]
print(round(c(tau_hat = tau_hat, se_hat = se_hat, ci_unadj), 4))

## ============================================================
## 2. Baseline covariate: prior-election turnout predicts 2003 turnout
## ============================================================
## A baseline covariate is measured BEFORE assignment, so it is fixed (the same
## under either potential outcome). The 2002 general-election turnout tracks 2003
## turnout, so it carries outcome information the raw Difference-in-Means ignores.
x_cov <- acorn$v_g2002               # 2002 general turnout (pre-treatment)
print(round(cor(y_obs, x_cov), 3))   # ~ 0.55

## Freedman-Diaconis bin count, so paired histograms share bins.
compute_fd_bins <- function(values) {
  bin_width <- 2 * IQR(values) / (length(values)^(1 / 3))
  if (bin_width > 0) {
    ceiling((max(values) - min(values)) / bin_width)
  } else {
    30
  }
}

## ============================================================
## 3. Design lever (1): blocking on the covariate
## ============================================================
## Hypothetical "if ACORN had been blocked." Pair the 28 precincts on the
## covariate with blockTools (optimal nonbipartite matching), then randomize one
## treated per pair. Block randomization keeps only the 2^14 = 16,384 assignments
## balanced on v_g2002 -- a tiny subset of the choose(28,14) ~ 40.1 million under
## CRA -- ideally the assignments whose estimates sit near the truth.
acorn$unit <- seq_len(N)
pairs_out  <- block(
  data       = acorn,        # the data frame of units
  id.vars    = "unit",       # column identifying each unit
  block.vars = "v_g2002",    # covariate(s) to match on
  n.tr       = 2,            # units per block (2 -> matched pairs)
  algorithm  = "optimal"     # minimize total within-pair distance
)
pair_table <- pairs_out$blocks[[1]]    # 14 matched pairs ("Unit 1", "Unit 2")

## One blocked assignment: randomize exactly one treated per pair.
block_randomize <- function() {
  z <- integer(N)
  for (b in seq_len(nrow(pair_table))) {
    pair <- as.integer(pair_table[b, c("Unit 1", "Unit 2")])
    z[sample(x = pair, size = 1)] <- 1
  }
  z
}

set.seed(seed = 12345)
null_complete <- replicate(n = 10^4,
                           expr = diff_in_means(z = sample(x = z_obs), y = y_obs))
null_blocked  <- replicate(n = 10^4,
                           expr = diff_in_means(z = block_randomize(), y = y_obs))
print(round(c(sd_complete = sd(null_complete), sd_blocked = sd(null_blocked),
              ratio = sd(null_blocked) / sd(null_complete)), 4))

## Figure: overlaid null randomization distributions, complete vs. blocked.
block_df <- rbind(
  data.frame(diff = null_complete, method = "Completely Randomized"),
  data.frame(diff = null_blocked,  method = "Blocked")
)
block_df$method <- factor(block_df$method,
                          levels = c("Completely Randomized", "Blocked"))
block_bins <- max(compute_fd_bins(null_complete), compute_fd_bins(null_blocked))
acorn_blocked_vs_complete <- ggplot(data = block_df,
                                    mapping = aes(x = diff, fill = method)) +
  geom_histogram(data = subset(block_df, method == "Completely Randomized"),
                 bins = block_bins, alpha = 0.4, position = "identity") +
  geom_histogram(data = subset(block_df, method == "Blocked"),
                 bins = block_bins, alpha = 0.7, position = "identity") +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Blocked vs. Completely Randomized",
       subtitle = "Randomization Distributions under the Sharp Null of No Effect",
       x = "Difference-in-Means", y = "Frequency", fill = "Randomization Type") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "top")

##+ eval=saveplots_
ggsave(plot = acorn_blocked_vs_complete,
       file = "figures/acorn_blocked_vs_complete.pdf",
       width = 8, height = 5.5, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 4. Design lever (2): rescaling -- the gain score
## ============================================================
## Because x is pre-treatment it cancels from the individual effect, so the gain
## score e_i = y_i - x_i has the SAME ATE: the adjusted estimator stays unbiased.
## Its variance is smaller when x tracks y.
gain_score  <- y_obs - x_cov
tau_hat_adj <- diff_in_means(z = z_obs, y = gain_score)            # ~ 0.057
se_hat_adj  <- sqrt(conservative_var(z = z_obs, y = gain_score))   # ~ 0.020
T_obs_adj   <- test_statistic(tau_hat = tau_hat_adj, se_hat = se_hat_adj, tau_0 = 0)
ci_adj      <- c(lower = tau_hat_adj - z_crit * se_hat_adj,
                 upper = tau_hat_adj + z_crit * se_hat_adj)        # ~ [0.018, 0.097]
print(round(c(tau_hat_adj = tau_hat_adj, se_hat_adj = se_hat_adj,
              var_ratio = (se_hat_adj / se_hat)^2,
              covariate_dim = diff_in_means(z = z_obs, y = x_cov),  # ~ -0.021
              ci_adj,
              p_two_adj = 2 * pnorm(q = abs(T_obs_adj), lower.tail = FALSE)), 4))

## Figure: null randomization distribution, original outcome vs. gain score.
set.seed(seed = 12345)
null_original <- replicate(n = 10^4,
                           expr = diff_in_means(z = sample(x = z_obs), y = y_obs))
null_rescaled <- replicate(n = 10^4,
                           expr = diff_in_means(z = sample(x = z_obs),
                                                y = gain_score))
rescale_df <- rbind(
  data.frame(diff = null_original, outcome = "Original Outcome (vote03)"),
  data.frame(diff = null_rescaled,
             outcome = "Rescaled Outcome (vote03 - v_g2002)")
)
acorn_original_vs_rescaled <- ggplot(data = rescale_df,
                                     mapping = aes(x = diff, fill = outcome)) +
  geom_histogram(data = subset(rescale_df,
                               outcome == "Original Outcome (vote03)"),
                 binwidth = sd(null_original) * 0.3, alpha = 0.4,
                 position = "identity") +
  geom_histogram(data = subset(rescale_df,
                               outcome == "Rescaled Outcome (vote03 - v_g2002)"),
                 binwidth = sd(null_rescaled) * 0.3, alpha = 0.7,
                 position = "identity") +
  scale_fill_manual(values = c("Original Outcome (vote03)" = "blue",
                               "Rescaled Outcome (vote03 - v_g2002)" = "#E1B000")) +
  labs(title = "Complete Randomization Distributions",
       subtitle = "Original vs. Rescaled Outcome under the Sharp Null of No Effect",
       x = "Difference-in-Means", y = "Frequency", fill = "Outcome Scale") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "top")

##+ eval=saveplots_
ggsave(plot = acorn_original_vs_rescaled,
       file = "figures/acorn_original_vs_rescaled.pdf",
       width = 8, height = 5.5, units = "in", dpi = 300)

##+ eval=TRUE
## ---- The adjusted and unadjusted estimates agree asymptotically ----
## adjusted - unadjusted = diff-in-means on the gain score minus on the outcome
##                       = -(diff-in-means on the covariate x).
## E[diff-in-means on x] = 0 (x has no effect) and its variance -> 0 as N grows,
## so the two estimators coincide in large samples. Stack h copies of the 28
## precincts (N = 28h) and watch the gap concentrate at 0.
adjust_gap_draws <- function(h, n_draws = 5000) {
  xv <- rep(x_cov, h)
  zz <- rep(z_obs, h)
  replicate(n = n_draws, expr = -diff_in_means(z = sample(x = zz), y = xv))
}

set.seed(seed = 12345)
gap_sizes <- c(28, 56, 112, 224)
gap_df <- do.call(rbind, lapply(gap_sizes, function(n) {
  data.frame(gap = adjust_gap_draws(h = n / 28), N = n)
}))
gap_df$panel <- factor(paste0("N = ", gap_df$N),
                       levels = paste0("N = ", gap_sizes))
acorn_adjust_difference <- ggplot(data = gap_df,
                                  mapping = aes(x = gap, colour = panel)) +
  geom_density(linewidth = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_viridis_d(option = "viridis", end = 0.85, name = NULL) +
  coord_cartesian(xlim = c(-0.06, 0.06)) +
  labs(x = "Adjusted minus unadjusted estimate", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

##+ eval=saveplots_
ggsave(plot = acorn_adjust_difference,
       file = "figures/acorn_adjust_difference.pdf",
       width = 7, height = 4.2, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 5. Regression adjustment: estimate the slope instead of fixing beta = 1
## ============================================================
## The gain score subtracts 1 * x. Regression adjustment instead RESIDUALIZES the
## outcome on the centered covariate with an ESTIMATED slope (Lin: fit separately
## within each arm), then takes the Difference-in-Means on the residuals. This is
## exactly the coefficient on treatment in the interacted OLS regression.
X <- scale(acorn[, c("v_g2002", "v_p2002", "v_m2002")], scale = FALSE)  # center covs

## (a) residualize with arm-specific OLS slopes, then Difference-in-Means
b1 <- coef(lm(vote03 ~ X, data = acorn, subset = z == 1))[-1]  # treated slopes
b0 <- coef(lm(vote03 ~ X, data = acorn, subset = z == 0))[-1]  # control slopes
e  <- acorn$vote03 - ifelse(acorn$z == 1, X %*% b1, X %*% b0)
tau_hat_lin_resid <- mean(e[acorn$z == 1]) - mean(e[acorn$z == 0])   # 0.0566

## (b) coefficient on z in the interacted regression -- identical
tau_hat_lin <- coef(lm(vote03 ~ z * X, data = acorn))["z"]           # 0.0566
se_lin_hc2  <- sqrt(sandwich::vcovHC(lm(vote03 ~ z * X, data = acorn),
                                     type = "HC2")["z", "z"])

print(c(resid_form = round(tau_hat_lin_resid, 6),
        lm_coef = round(unname(tau_hat_lin), 6),
        equal = isTRUE(all.equal(unname(tau_hat_lin_resid), unname(tau_hat_lin)))))
print(round(c(tau_hat_lin = tau_hat_lin, se_lin_hc2 = se_lin_hc2), 4))

## estimatr::lm_lin is the canonical one-call version (same estimate and HC2 se):
lin_fit2 <- estimatr::lm_lin(vote03 ~ z, covariates = ~ v_g2002 + v_p2002 + v_m2002,
                             data = acorn, se_type = "HC2")
print(round(c(lm_lin_ate = coef(lin_fit2)["z"],
              lm_lin_se  = lin_fit2$std.error["z"]), 4))   # ~ 0.057, se ~ 0.018

## Debiasing (Chang, Middleton, and Aronow 2024): the regression estimator has a
## small O(1/n) bias = covariance of leverage and individual effects -- zero when
## effects are constant or n_1 = n_0 (as in ACORN). For ACORN's balanced design
## the bias is negligible, so no correction is needed here.
print(c(n1 = n1, n0 = n0, balanced = n1 == n0))

## ============================================================
## 6. Key numbers behind the slides
## ============================================================
cat("Unadjusted:  tau =", round(tau_hat, 3),
    " se =", round(se_hat, 3),
    " CI = [", round(ci_unadj[1], 3), ",", round(ci_unadj[2], 3), "]\n")
cat("Blocked null SD:", round(sd(null_blocked), 3),
    " vs complete:", round(sd(null_complete), 3), "\n")
cat("Gain score:  tau =", round(tau_hat_adj, 3),
    " se =", round(se_hat_adj, 3),
    " CI = [", round(ci_adj[1], 3), ",", round(ci_adj[2], 3), "]\n")
cat("Lin estimate:", round(unname(tau_hat_lin), 4),
    " HC2 se =", round(se_lin_hc2, 4), "\n")
