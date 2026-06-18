##' ---
##' title: "Day 5: Uncertainty, Testing, and Covariance Adjustment"
##' output: github_document
##' ---
##'
##' This script reproduces every quantity, code block, and figure in
##' 2026-06-18.tex. It continues 2026-06-17.R: the estimand is the ATE
##' (tau_bar), the estimator is the Difference-in-Means. Two examples appear:
##'   * VILLAGE HEADS (both potential outcomes known) -- used to ILLUSTRATE the
##'     conservative variance estimator and the Normal approximation, because
##'     there we can compute the truth (Gerber and Green 2012, Ch. 2).
##'   * ACORN (acorn03.csv) -- the running empirical example for the standard
##'     error, test, p-values, confidence interval, and covariance adjustment.
##'
##' The standard-Normal, conservative-Normal, and covariance-adjustment figures
##' follow the styling of the source scripts in 04_Uncertainty_and_Hypothesis_
##' Testing/figures/ and 05_Covariance_Adjustment/Arceneaux_2005/analysis.R.
##'
##' Figures produced here (written to the figures/ subdirectory):
##'   * villageheads_consv_var.pdf       (conservative estimator overstates truth)
##'   * villageheads_normal_approx.pdf   (standardized estimator -> Normal as N grows)
##'   * stand_norm_plot_lower_tail.pdf   (rejection region, smaller-ATE alternative)
##'   * stand_norm_plot_upper_tail.pdf   (rejection region, larger-ATE alternative)
##'   * stand_norm_plot_two_side.pdf     (rejection region, two-sided alternative)
##'   * stand_norm_cdf.pdf               (the CDF Phi behind the p-value)
##'   * consv_stand_norm_plot.pdf        (conservative variance => valid tests)
##'   * stand_norm_plot_two_side_CI.pdf  (confidence interval by test inversion)
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

## The estimator from yesterday: treated mean minus control mean.
diff_in_means <- function(z, y) mean(y[z == 1]) - mean(y[z == 0])

## Conservative estimator of the variance: the two estimable arm pieces. It
## drops the unestimable -S2_tau/N term, so it estimates an UPPER BOUND on the
## true variance.
conservative_var <- function(z, y) {
  var(y[z == 1]) / sum(z == 1) + var(y[z == 0]) / sum(z == 0)
}

alpha <- 0.05   # significance level used throughout

## ============================================================
## 1. ACORN: the Difference-in-Means estimate (recap of Day 4)
## ============================================================
## Kansas City 2003 GOTV experiment (Arceneaux 2005): 28 precincts, complete
## random assignment of 14 canvassed (z = 1) and 14 control (z = 0).
acorn  <- read.csv("acorn03.csv")
z_obs  <- acorn$z           # 1 = canvassed (GOTV), 0 = control
y_obs  <- acorn$vote03      # observed 2003 turnout proportion
N  <- length(z_obs)
n1 <- sum(z_obs)
n0 <- N - n1                # 28, 14, 14

tau_hat <- diff_in_means(z = z_obs, y = y_obs)   # ~ 0.036 (about 3.6 points)

## ============================================================
## 2. Conservative variance estimation
## ============================================================
var_hat <- conservative_var(z = z_obs, y = y_obs)   # ~ 0.000595
se_hat  <- sqrt(var_hat)                             # ~ 0.0244
print(round(c(tau_hat = tau_hat, var_hat = var_hat, se_hat = se_hat), 5))

## ---- Village heads: the worked example (Gerber and Green 2012, Ch. 2) ----
## Both potential outcomes are known, so we can compute the TRUE variance and the
## EXPECTED value of the conservative estimator, and watch the estimator overstate.
vh_y0  <- c(10, 15, 20, 20, 10, 15, 15)   # y_i(0): control potential outcomes
vh_y1  <- c(15, 15, 30, 15, 20, 15, 30)   # y_i(1): treated potential outcomes
vh_N   <- length(vh_y0)
vh_n1  <- 2                               # complete assignment: 2 treated of 7

## Enumerate the design: each column of vh_omega is one assignment vector z.
vh_omega <- apply(X = combn(x = vh_N, m = vh_n1), MARGIN = 2,
                  FUN = function(idx) as.integer(seq_len(vh_N) %in% idx))

## For each of the 21 equally likely assignments, form the observed outcome and
## apply BOTH the Difference-in-Means and the conservative variance estimator.
vh_est <- apply(X = vh_omega, MARGIN = 2, FUN = function(z) {
  diff_in_means(z = z, y = z * vh_y1 + (1 - z) * vh_y0)
})
vh_var_hat <- apply(X = vh_omega, MARGIN = 2, FUN = function(z) {
  conservative_var(z = z, y = z * vh_y1 + (1 - z) * vh_y0)
})

vh_true_var <- mean((vh_est - mean(vh_est))^2)   # ~ 21.19  (true Var[tau_hat])
vh_exp_var  <- mean(vh_var_hat)                  # ~ 28.33  (> true variance)
vh_s2_tau   <- var(vh_y1 - vh_y0)
print(round(c(true_var = vh_true_var, exp_conservative = vh_exp_var,
              gap = vh_exp_var - vh_true_var, S2_tau_over_N = vh_s2_tau / vh_N), 4))

## ---- Figure: the conservative estimator overstates, in expectation ----
## y-axis is PROBABILITY: each of the 21 assignments is equally likely.
villageheads_consv_var <- ggplot(data = data.frame(var_hat = vh_var_hat),
                                 mapping = aes(x = var_hat)) +
  geom_histogram(mapping = aes(y = after_stat(count / sum(count))),
                 bins = 12, fill = "grey75", colour = "white") +
  geom_vline(xintercept = vh_true_var, linetype = "solid", linewidth = 0.8) +
  geom_vline(xintercept = vh_exp_var, linetype = "dashed",
             colour = viridis(3)[1], linewidth = 0.8) +
  annotate(geom = "text", x = vh_true_var, y = 0.27, hjust = 1.05,
           label = "true Var = 21.2", size = 3) +
  annotate(geom = "text", x = vh_exp_var, y = 0.22, hjust = -0.05,
           label = "E[conservative] = 28.3", colour = viridis(3)[1], size = 3) +
  labs(x = "Conservative variance estimate (over the 21 assignments)",
       y = "Probability") +
  theme_minimal(base_size = 14)

##+ eval=saveplots_
ggsave(plot = villageheads_consv_var,
       file = "figures/villageheads_consv_var.pdf",
       width = 6, height = 4, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 3. Normal approximation (shown on village heads)
## ============================================================
## The finite-population CLT (Li and Ding 2017) lets us describe the distribution
## of the Difference-in-Means -- standardized by its TRUE mean and variance --
## using a Normal, knowing only the mean and variance (not the missing POs). We
## stack h copies of the 7 villages (N = 7h) and watch the standardized estimator
## approach the standard Normal as N grows.
standardized_draws <- function(h, n_draws = 10^5) {
  yc <- rep(vh_y0, h)
  yt <- rep(vh_y1, h)
  n_units <- length(yc)
  n_treated <- 2 * h
  est <- replicate(n = n_draws, expr = {
    z <- sample(rep(c(1, 0), c(n_treated, n_units - n_treated)))
    diff_in_means(z = z, y = z * yt + (1 - z) * yc)
  })
  (est - mean(est)) / sd(est)               # standardize by true mean and sd
}

set.seed(seed = 12345)
normal_sizes <- c(7, 14, 28, 56)
normal_df <- do.call(rbind, lapply(normal_sizes, function(n) {
  data.frame(std = standardized_draws(h = n / 7), N = n)
}))
normal_df$panel <- factor(paste0("N = ", normal_df$N),
                          levels = paste0("N = ", normal_sizes))

villageheads_normal_approx <- ggplot(data = normal_df, mapping = aes(x = std)) +
  geom_histogram(mapping = aes(y = after_stat(density)),
                 bins = 40, fill = "grey75", colour = "white") +
  stat_function(fun = dnorm, colour = viridis(3)[1], linewidth = 0.8) +
  facet_wrap(facets = ~ panel, nrow = 1) +
  coord_cartesian(xlim = c(-3.5, 3.5)) +
  labs(x = "Standardized Difference-in-Means (village heads)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

##+ eval=saveplots_
ggsave(plot = villageheads_normal_approx,
       file = "figures/villageheads_normal_approx.pdf",
       width = 9, height = 2.8, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 4. Hypothesis testing: the test statistic and its reference
## ============================================================
## To test H_0: ATE = tau_0, standardize the Difference-in-Means by tau_0 and the
## standard error.  The reference distribution is the standard Normal.  In
## practice we plug in the conservative standard error:
##   T = (Difference-in-Means - tau_0) / conservative standard error.
test_statistic <- function(tau_hat, se_hat, tau_0 = 0) (tau_hat - tau_0) / se_hat

T_obs <- test_statistic(tau_hat = tau_hat, se_hat = se_hat, tau_0 = 0)   # ~ 1.49

## p-values from the Normal approximation (Phi = pnorm):
p_upper <- pnorm(q = T_obs, lower.tail = FALSE)            # ~ 0.068  (H_a: ATE > 0)
p_lower <- pnorm(q = T_obs, lower.tail = TRUE)             # ~ 0.932  (H_a: ATE < 0)
p_two   <- 2 * pnorm(q = abs(T_obs), lower.tail = FALSE)   # ~ 0.137  (two-sided)
print(round(c(T_obs = T_obs, p_upper = p_upper,
              p_lower = p_lower, p_two = p_two), 4))

## ---- Figures: standard-Normal rejection regions (styling from
##      04_Uncertainty_and_Hypothesis_Testing/figures/standard_normal_plots.R) ----
stand_norm_data <- data.frame(x = seq(from = -3.5, to = 3.5, length.out = 1000),
                              dens = dnorm(seq(from = -3.5, to = 3.5,
                                               length.out = 1000)))
stand_norm_plot <- ggplot(data = stand_norm_data, mapping = aes(x = x, y = dens)) +
  geom_line(colour = "black") +
  labs(x = "Test statistic", y = "Probability Density") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(from = -3, to = 3, by = 1),
                     limits = c(-3.5, 3.5)) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

## Rejection region shaded YELLOW throughout; the two-sided plot also shows the
## one-sided 0.05 cutoffs in PURPLE (the "yellow with purple" comparison).
purple <- viridis(3)[1]
yellow <- viridis(3)[3]

stand_norm_plot_lower_tail <- stand_norm_plot +
  geom_area(data = subset(stand_norm_data, x <= qnorm(alpha)),
            fill = purple, alpha = 0.7) +
  annotate(geom = "text", x = qnorm(alpha) - 0.85, y = 0.085,
           label = "Pr(shaded) = 0.05", colour = "grey20", size = 4)

stand_norm_plot_upper_tail <- stand_norm_plot +
  geom_area(data = subset(stand_norm_data, x >= qnorm(1 - alpha)),
            fill = purple, alpha = 0.7) +
  annotate(geom = "text", x = qnorm(1 - alpha) + 0.85, y = 0.085,
           label = "Pr(shaded) = 0.05", colour = "grey20", size = 4)

stand_norm_plot_two_side <- stand_norm_plot +
  geom_area(data = subset(stand_norm_data, x <= qnorm(alpha)),
            fill = purple, alpha = 0.7) +
  geom_area(data = subset(stand_norm_data, x >= qnorm(1 - alpha)),
            fill = purple, alpha = 0.7) +
  geom_area(data = subset(stand_norm_data, x <= qnorm(alpha / 2)),
            fill = yellow, alpha = 0.9) +
  geom_area(data = subset(stand_norm_data, x >= qnorm(1 - alpha / 2)),
            fill = yellow, alpha = 0.9) +
  annotate(geom = "text", x = qnorm(alpha / 2) - 0.85, y = 0.085,
           label = "0.025", colour = "grey20", size = 4) +
  annotate(geom = "text", x = qnorm(1 - alpha / 2) + 0.85, y = 0.085,
           label = "0.025", colour = "grey20", size = 4)

##+ eval=saveplots_
ggsave(plot = stand_norm_plot_lower_tail,
       file = "figures/stand_norm_plot_lower_tail.pdf",
       width = 6.5, height = 4, units = "in", dpi = 300)
ggsave(plot = stand_norm_plot_upper_tail,
       file = "figures/stand_norm_plot_upper_tail.pdf",
       width = 6.5, height = 4, units = "in", dpi = 300)
ggsave(plot = stand_norm_plot_two_side,
       file = "figures/stand_norm_plot_two_side.pdf",
       width = 6.5, height = 4, units = "in", dpi = 300)

##+ eval=TRUE
## ---- Figure: the CDF Phi behind the p-value ----
## On the standard-Normal CDF, Phi(t) is the lower-tail area and 1 - Phi(t) is the
## upper-tail area (the upper p-value). The two-sided rejection regions
## (t <= z_{0.025} and t >= z_{0.975}) are the yellow vertical bands.
t_show <- 1.5
rej_lo  <- qnorm(alpha / 2)
rej_hi  <- qnorm(1 - alpha / 2)
cdf_grid <- seq(from = -3.5, to = 3.5, length.out = 1000)
cdf_data <- data.frame(x = cdf_grid, cdf = pnorm(cdf_grid))
stand_norm_cdf <- ggplot(data = cdf_data, mapping = aes(x = x, y = cdf)) +
  annotate(geom = "rect", xmin = -3.5, xmax = rej_lo, ymin = 0, ymax = 1,
           fill = yellow, alpha = 0.25) +
  annotate(geom = "rect", xmin = rej_hi, xmax = 3.5, ymin = 0, ymax = 1,
           fill = yellow, alpha = 0.25) +
  annotate(geom = "text", x = (-3.5 + rej_lo) / 2, y = 0.5, label = "reject",
           angle = 90, colour = "grey35", size = 3) +
  annotate(geom = "text", x = (3.5 + rej_hi) / 2, y = 0.5, label = "reject",
           angle = 90, colour = "grey35", size = 3) +
  geom_line(colour = "black", linewidth = 0.8) +
  ## locate t on the x-axis, then read the HEIGHT Phi(t) on the y-axis
  geom_segment(x = t_show, xend = t_show, y = 0, yend = pnorm(t_show),
               linetype = "dashed", colour = "grey40") +
  geom_segment(x = -3.5, xend = t_show, y = pnorm(t_show), yend = pnorm(t_show),
               linetype = "dashed", colour = "grey40") +
  annotate(geom = "point", x = t_show, y = pnorm(t_show), size = 2) +
  annotate(geom = "text", x = t_show, y = -0.04, label = "t", colour = "grey25",
           size = 3.6) +
  annotate(geom = "text", x = -3.3, y = pnorm(t_show) + 0.05, hjust = 0,
           label = "height Phi(t) = Pr(T <= t)", colour = "grey30", size = 3.3) +
  ## upper-tail probability 1 - Phi(t): the vertical gap from Phi(t) up to 1
  annotate(geom = "segment", x = t_show, xend = t_show,
           y = pnorm(t_show), yend = 1, colour = purple, linewidth = 1.1,
           arrow = arrow(ends = "both", length = unit(0.02, "npc"))) +
  annotate(geom = "text", x = t_show + 0.2, y = 0.80,
           hjust = 0, label = "upper p = 1 - Phi(t)\n(vertical gap to 1)",
           colour = purple, size = 3.3) +
  labs(x = "Test statistic t", y = expression(Phi(t))) +
  theme_minimal(base_size = 14)

##+ eval=saveplots_
ggsave(plot = stand_norm_cdf, file = "figures/stand_norm_cdf.pdf",
       width = 8, height = 4, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 5. Validity: conservative se => narrower limiting law => valid test
## ============================================================
## Under H_0 the test statistic equals (estimator standardized by the TRUE se,
## which is ~ standard Normal) times (true se / conservative se). The conservative
## se concentrates on sqrt(upper bound) >= true se (its probability limit), so the
## ratio's limit is <= 1: the limiting law is at least as narrow as standard
## Normal, and a true null is rejected with probability <= alpha.
## Figure styling from conservative_hyp_tests.R.
consv_c <- 0.75   # illustrative probability limit of (true se / conservative se)
scaled_norm_data <- data.frame(x = seq(from = -3.5, to = 3.5, length.out = 1000),
                               dens = dnorm(seq(from = -3.5, to = 3.5,
                                                length.out = 1000),
                                            mean = 0, sd = consv_c))
## Rejection region = beyond the standard-Normal cutoff z_{1-alpha}. Shade it under
## BOTH laws: the standard Normal gives area alpha = 0.05; the narrower limiting law
## gives a SMALLER area, so the conservative test rejects a true null < alpha.
cutoff   <- qnorm(1 - alpha)
area_narrow <- 1 - pnorm(cutoff, mean = 0, sd = consv_c)   # ~ 0.014
consv_stand_norm_plot <- ggplot() +
  geom_area(data = subset(stand_norm_data, x >= cutoff),
            mapping = aes(x = x, y = dens), fill = purple, alpha = 0.35) +
  geom_area(data = subset(scaled_norm_data, x >= cutoff),
            mapping = aes(x = x, y = dens), fill = purple, alpha = 0.85) +
  geom_line(data = stand_norm_data, mapping = aes(x = x, y = dens),
            colour = "black") +
  geom_line(data = scaled_norm_data, mapping = aes(x = x, y = dens),
            colour = "gray40", linetype = "dashed") +
  annotate(geom = "text", x = -0.2, y = 0.29, label = "Standard Normal",
           colour = "black", size = 3, hjust = 1) +
  annotate(geom = "text", x = 0.15, y = 0.55,
           label = "Limiting law of test statistic\nwith conservative variance",
           colour = "gray35", size = 3, hjust = 0) +
  annotate(geom = "text", x = cutoff + 0.9, y = 0.02,
           label = "<= alpha", colour = "grey15", size = 3.4) +
  labs(x = "Test statistic", y = "Probability Density") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(from = -3, to = 3, by = 1),
                     limits = c(-3.5, 3.5)) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

##+ eval=saveplots_
ggsave(plot = consv_stand_norm_plot,
       file = "figures/consv_stand_norm_plot.pdf",
       width = 6, height = 4, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 6. Confidence interval by inverting the test
## ============================================================
## A (1 - alpha) CI is the set of null ATEs we do NOT reject. Inverting the
## two-sided test keeps every tau_0 whose test statistic lies in [-1.96, 1.96];
## the endpoints solve T(tau_0) = +/- 1.96, i.e. tau_hat +/- 1.96 * se.
z_crit   <- qnorm(p = 1 - alpha / 2)               # 1.96
ci_lower <- tau_hat - z_crit * se_hat              # ~ -0.011
ci_upper <- tau_hat + z_crit * se_hat              # ~  0.084
print(round(c(ci_lower = ci_lower, ci_upper = ci_upper), 4))

## ---- Figure: ACORN confidence interval by test inversion ----
## Styling from standard_normal_plots.R (stand_norm_plot_two_side_CI block).
test_stat_range <- seq(from = -z_crit, to = z_crit, length.out = 5)
null_ates       <- tau_hat - test_stat_range * se_hat

stand_norm_plot_two_side_CI <- stand_norm_plot +
  geom_area(data = subset(stand_norm_data, x <= qnorm(alpha / 2)),
            fill = yellow, alpha = 0.9) +
  geom_area(data = subset(stand_norm_data, x >= qnorm(1 - alpha / 2)),
            fill = yellow, alpha = 0.9) +
  geom_vline(xintercept = test_stat_range, linetype = "dashed",
             colour = "#2a9d8f", alpha = 0.8) +
  annotate(geom = "text", x = test_stat_range, y = 0.27,
           label = paste0("ATE[0] == ", round(null_ates, 3)), parse = TRUE,
           angle = 90, vjust = 1, hjust = 1, colour = "#2a9d8f", size = 2.6) +
  scale_x_continuous(breaks = c(-3, -1.96, 0, 1.96, 3),
                     labels = c("-3", "-1.96", "0", "1.96", "3"),
                     limits = c(-3.5, 3.5))

##+ eval=saveplots_
ggsave(plot = stand_norm_plot_two_side_CI,
       file = "figures/stand_norm_plot_two_side_CI.pdf",
       width = 6, height = 4, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 7. Covariance adjustment
## ============================================================
## A baseline covariate is measured BEFORE assignment (fixed, same under both
## POs). Prior-election turnout predicts 2003 turnout, so it carries outcome
## information. Two design-based ways to use it (Arceneaux 2005):
##   (1) BLOCKING -- randomize within blocks of similar precincts;
##   (2) RESCALING -- subtract the covariate (a gain score) before the test.
x_cov <- acorn$v_g2002               # 2002 general turnout (pre-treatment)
print(round(cor(y_obs, x_cov), 3))   # ~ 0.55

## Freedman-Diaconis bin count (as in analysis.R), so the two histograms share bins.
compute_fd_bins <- function(values) {
  bin_width <- 2 * IQR(values) / (length(values)^(1 / 3))
  if (bin_width > 0) {
    ceiling((max(values) - min(values)) / bin_width)
  } else {
    30
  }
}

## ---- (1) Blocking: form 14 pairs by prior turnout, randomize within pairs ----
## Hypothetical "if ACORN had been blocked." Pair the 28 precincts on the
## covariate with blockTools (optimal nonbipartite matching), then randomize one
## treated per pair. Block randomization keeps only the 2^14 = 16,384 assignments
## balanced on v_g2002 -- a tiny subset of the choose(28,14) ~ 40.1 million under
## CRA -- ideally the assignments whose estimates sit near the truth.
acorn$unit <- seq_len(N)
pairs_out  <- blockTools::block(data = acorn, id.vars = "unit", block.vars = "v_g2002",
                                n.tr = 2, algorithm = "optimal")
pair_table <- pairs_out$blocks[[1]]      # 14 matched pairs ("Unit 1", "Unit 2")

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

## Plot styled after analysis.R's blocked_vs_randomized.png: overlaid histograms,
## plasma fill, shared Freedman-Diaconis bins.
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
## ---- (2) Rescaling: subtract the covariate (gain score) ----
## Because x is pre-treatment it cancels from the individual effect, so the gain
## score has the SAME ATE: the adjusted estimator stays unbiased. Its variance is
## smaller when x tracks y.
gain_score  <- y_obs - x_cov
tau_hat_adj <- diff_in_means(z = z_obs, y = gain_score)            # ~ 0.057
se_hat_adj  <- sqrt(conservative_var(z = z_obs, y = gain_score))   # ~ 0.020
T_obs_adj   <- test_statistic(tau_hat = tau_hat_adj, se_hat = se_hat_adj, tau_0 = 0)
ci_adj      <- c(lower = tau_hat_adj - z_crit * se_hat_adj,
                 upper = tau_hat_adj + z_crit * se_hat_adj)     # ~ [0.018, 0.097]
print(round(c(tau_hat_adj = tau_hat_adj, se_hat_adj = se_hat_adj,
              var_ratio = (se_hat_adj / se_hat)^2,
              covariate_dim = diff_in_means(z = z_obs, y = x_cov),  # ~ -0.021
              ci_adj,
              p_two_adj = 2 * pnorm(q = abs(T_obs_adj), lower.tail = FALSE)), 4))

## ---- Regression adjustment: estimate the coefficient instead of fixing beta=1 ----
## The gain score subtracts 1 * x. Regression adjustment instead RESIDUALIZES the
## outcome on the centered covariate with an ESTIMATED slope (Lin: fit separately
## within each arm), then takes the Difference-in-Means on the residuals. This is
## exactly the coefficient on treatment in the interacted OLS regression.
## Center a MATRIX of covariates (Lin extends to many covariates at once).
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

## the two are identical, even with many covariates:
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
## effects are constant or n_1 = n_0 (as in ACORN), growing with effect
## heterogeneity and arm imbalance. CMA give an exact closed-form correction with
## the SAME limiting distribution. No CRAN package yet; for ACORN's balanced
## design the bias is negligible, so no correction is needed here.
print(c(n1 = n1, n0 = n0, balanced = n1 == n0))

## Null randomization distribution: original outcome vs. rescaled (gain score).
## Plot styled after analysis.R's complete_randomization.png: overlaid histograms,
## blue vs. yellow, binwidth proportional to each distribution's SD.
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
## Overlay the gap's distribution at each N: all centered at 0, narrowing to a
## spike as N grows -- the adjusted and unadjusted estimates converge.
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
