##' Randomization inference for the 2026-06-16 lecture: the ACORN GOTV experiment.
##' Reproduces every number and figure shown on the slides.
##'
##' Before running: download acorn03.csv into this script's working directory.
##' Then run top-to-bottom (e.g. `Rscript 2026-06-16.R`); figures are written to
##' an figures/ subfolder.

library(ggplot2)

# Setup: data, test statistic, and the randomization distribution ------------

acorn <- read.csv("acorn03.csv")
z_obs <- acorn$z                 # observed assignment: 1 = canvassed, 0 = control
y_obs <- acorn$vote03 * 100      # 2003 precinct turnout, in percent
n_1   <- sum(z_obs)              # number treated  (fixed by complete randomization)
n_0   <- sum(1 - z_obs)          # number control

#' Difference-in-Means test statistic: treated mean minus control mean.
diff_in_means <- function(z, y) {
  return(mean(y[z == 1]) - mean(y[z == 0]))
}
t_obs <- diff_in_means(z = z_obs, y = y_obs)   # observed effect, ~ 3.6 points

# Complete randomization: sample assignments by permuting the observed z.
# Hold this set fixed so every test below uses the same randomization draws.
set.seed(seed = 12345)
assignments <- replicate(n = 10^4, expr = sample(x = z_obs))

#' Randomization distribution of the Difference-in-Means on a FIXED outcome
#' vector y, as the assignment varies over the sampled set.
randomization_dist <- function(y) {
  apply(X = assignments, MARGIN = 2,
        FUN = function(z) diff_in_means(z = z, y = y))
}

# Sharp null of no effect ----------------------------------------------------
# No effect: outcomes are unchanged by assignment, so hold the observed y fixed.
null_no_effect <- randomization_dist(y_obs)
p_no_effect    <- mean(null_no_effect >= t_obs)          # upper p-value
cat("No effect: t_obs =", round(t_obs, 2),
    " upper p =", round(p_no_effect, 3), "\n")

# Constant-effect null H_tau -------------------------------------------------
# H_tau: a constant effect tau for every precinct. Adjust outcomes by tau, then
# test as no effect on the adjusted outcomes y - tau * z. Returns both
# one-sided p-values.
test_tau <- function(tau) {
  y_adj <- y_obs - tau * z_obs
  obs   <- diff_in_means(z = z_obs, y = y_adj)
  null  <- randomization_dist(y_adj)
  return(c(upper = mean(null >= obs), lower = mean(null <= obs)))
}
cat("H_2.5: upper p =", round(test_tau(2.5)["upper"], 3), "\n")

# Confidence set by inverting the two-sided test -----------------------------
# Keep every tau the two-sided test does not reject (both tails >= alpha/2).
alpha <- 0.05
taus  <- seq(from = -3, to = 10, by = 0.02)
pvals <- sapply(X = taus, FUN = test_tau)
keep  <- taus[pvals["upper", ] >= alpha / 2 & pvals["lower", ] >= alpha / 2]
ci    <- c(lower = min(keep), upper = max(keep))
cat("95% confidence set:", round(ci, 2), "\n")

# Hodges-Lehmann point estimate ----------------------------------------------
# The tau that makes the observed statistic equal its null mean (0 here); for
# the Difference-in-Means this is just the observed difference.
hl_estimate <- t_obs
cat("Hodges-Lehmann estimate:", round(hl_estimate, 2), "\n")

# Normal approximation to the sharp-null distribution ------------------------
# Under the sharp null the potential outcomes are fixed, so the Difference-in-
# Means has a known finite-population variance; standardize and use N(0, 1).
s2_y   <- var(y_obs)
v_null <- (1 / n_1 + 1 / n_0) * s2_y
p_norm <- pnorm(q = t_obs / sqrt(v_null), lower.tail = FALSE)
cat("Normal-approximation upper p =", round(p_norm, 3), "\n")

# Figures --------------------------------------------------------------------
if (!dir.exists("figures")) dir.create("figures")

# Shared look: Freedman-Diaconis bin width, common x-range, and fill colors.
bin_width <- 2 * IQR(null_no_effect) / length(null_no_effect)^(1 / 3)
x_limits  <- c(-8.33, 8.33)
fills     <- c(reject = "black", retain = "grey75")

#' Histogram of a null distribution with an upper-tail rejection region (the
#' largest upper tail with null probability <= alpha) and the observed value.
upper_reject_plot <- function(null, obs, x_label, out_file) {
  values  <- sort(unique(null))
  c_alpha <- min(values[sapply(values, function(c) mean(null >= c)) <= alpha])
  plot_df <- data.frame(stat   = null,
                        region = ifelse(null >= c_alpha, "reject", "retain"))
  p <- ggplot(plot_df, aes(x = stat,
                           y = after_stat(count) / sum(after_stat(count)),
                           fill = region)) +
    geom_histogram(binwidth = bin_width, boundary = 0) +
    geom_vline(xintercept = obs, linetype = "dashed") +
    annotate(geom = "text", x = mean(c(c_alpha, x_limits[2])), y = Inf,
             label = "rejection region", vjust = 1.4, size = 3) +
    annotate(geom = "text", x = obs, y = Inf, label = "observed",
             hjust = 1.1, vjust = 1.4, size = 3) +
    scale_fill_manual(values = fills) +
    coord_cartesian(xlim = x_limits) +
    guides(fill = "none") +
    theme_bw() +
    xlab(label = x_label) +
    ylab(label = "Probability")
  ggsave(filename = out_file, plot = p,
         width = 6, height = 3.6, units = "in", dpi = 600)
}

# Sharp null of no effect: observed lies outside the rejection region.
upper_reject_plot(null = null_no_effect, obs = t_obs,
                  x_label = "Difference-in-means under no effect",
                  out_file = "figures/null_dist_no_effect_plot.pdf")

# Constant-effect null at tau = 2.5 (test on adjusted outcomes).
upper_reject_plot(null = randomization_dist(y_obs - 2.5 * z_obs),
                  obs = t_obs - 2.5,
                  x_label = "Difference-in-means on adjusted outcomes",
                  out_file = "figures/null_unif_plot.pdf")

# Two-sided rejection region: alpha/2 in each tail.
c_lo <- quantile(x = null_no_effect, probs = alpha / 2)
c_hi <- quantile(x = null_no_effect, probs = 1 - alpha / 2)
two_sided_df <- data.frame(
  stat   = null_no_effect,
  region = ifelse(null_no_effect <= c_lo | null_no_effect >= c_hi,
                  "reject", "retain"))
p_two_sided <- ggplot(two_sided_df, aes(x = stat,
                                        y = after_stat(count) / sum(after_stat(count)),
                                        fill = region)) +
  geom_histogram(binwidth = bin_width, boundary = 0) +
  annotate(geom = "text", x = mean(c(x_limits[1], c_lo)), y = Inf,
           label = "alpha/2", parse = TRUE, vjust = 1.4, size = 3) +
  annotate(geom = "text", x = mean(c(c_hi, x_limits[2])), y = Inf,
           label = "alpha/2", parse = TRUE, vjust = 1.4, size = 3) +
  scale_fill_manual(values = fills) +
  coord_cartesian(xlim = x_limits) +
  guides(fill = "none") +
  theme_bw() +
  xlab(label = "Difference-in-means under no effect") +
  ylab(label = "Probability")
ggsave(filename = "figures/two_sided_rejection.pdf", plot = p_two_sided,
       width = 6, height = 3.6, units = "in", dpi = 600)

# Why a TAIL region: power of a tail vs a middle region, each of size alpha.
# A true effect shifts the distribution, so the tail captures the most
# alternative mass. Shaded areas are drawn under the density curves.
delta    <- 6
alt_dist <- null_no_effect + delta
values   <- sort(unique(null_no_effect))
c_tail   <- min(values[sapply(values, function(c) mean(null_no_effect >= c)) <= alpha])
m_lo     <- quantile(x = null_no_effect, probs = 0.5 - alpha / 2)   # central alpha
m_hi     <- quantile(x = null_no_effect, probs = 0.5 + alpha / 2)
power_tail <- mean(alt_dist >= c_tail)
power_mid  <- mean(alt_dist >= m_lo & alt_dist <= m_hi)

density_of <- function(x, label) {
  d <- density(x)
  return(data.frame(x = d$x, y = d$y, dist = label))
}
null_curve <- density_of(null_no_effect, "Null (no effect)")
alt_curve  <- density_of(alt_dist,       "Alternative (constant effect = 6)")
panels  <- c("Tail region (size 0.05)", "Middle region (size 0.05)")
regions <- data.frame(panel = panels, r_min = c(c_tail, m_lo),
                      r_max = c(max(alt_curve$x), m_hi))
in_region <- function(curve, panel) {
  bounds <- regions[regions$panel == panel, ]
  return(cbind(curve[curve$x >= bounds$r_min & curve$x <= bounds$r_max, ],
               panel = panel))
}
curves     <- do.call(rbind, lapply(panels, function(pn) {
  rbind(cbind(null_curve, panel = pn), cbind(alt_curve, panel = pn))
}))
shade_alt  <- do.call(rbind, lapply(panels, function(pn) in_region(alt_curve, pn)))
shade_null <- do.call(rbind, lapply(panels, function(pn) in_region(null_curve, pn)))
power_lab  <- data.frame(panel = panels,
                         lab = paste0("power = ", sprintf("%.3f", c(power_tail, power_mid))))
curves$panel     <- factor(curves$panel,     levels = panels)
shade_alt$panel  <- factor(shade_alt$panel,  levels = panels)
shade_null$panel <- factor(shade_null$panel, levels = panels)
power_lab$panel  <- factor(power_lab$panel,  levels = panels)
p_power <- ggplot() +
  geom_area(data = shade_null, aes(x = x, y = y), fill = "grey60", alpha = 0.5) +
  geom_area(data = shade_alt,  aes(x = x, y = y), fill = "firebrick", alpha = 0.4) +
  geom_line(data = curves, aes(x = x, y = y, colour = dist), linewidth = 0.6) +
  geom_text(data = power_lab, aes(x = 11, y = Inf, label = lab),
            vjust = 1.8, size = 3) +
  facet_wrap(~ panel, ncol = 1) +
  scale_colour_manual(values = c("Null (no effect)" = "grey40",
                                 "Alternative (constant effect = 6)" = "firebrick"),
                      name = NULL) +
  coord_cartesian(xlim = c(-8, 14)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab(label = "Difference-in-means") +
  ylab(label = "Density")
ggsave(filename = "figures/rejection_region_power.pdf", plot = p_power,
       width = 6, height = 4.4, units = "in", dpi = 600)

# Confidence-set bounds: null at a hypothesized tau, with both alpha/2 tails
# shaded and the observed adjusted statistic. At each bound the observed value
# sits just at a tail edge.
tail_panel <- function(tau, label) {
  y_adj <- y_obs - tau * z_obs
  null  <- randomization_dist(y_adj)
  lo <- quantile(x = null, probs = alpha / 2)
  hi <- quantile(x = null, probs = 1 - alpha / 2)
  return(list(hist = data.frame(stat = null, panel = label,
                                region = ifelse(null <= lo | null >= hi,
                                                "reject", "retain")),
              line = data.frame(panel = label, obs = t_obs - tau)))
}
lo_lab <- paste0("'Lower bound:'~tau[0]==", round(ci["lower"], 1))
hi_lab <- paste0("'Upper bound:'~tau[0]==", round(ci["upper"], 1))
lo_b   <- tail_panel(ci["lower"], lo_lab)
hi_b   <- tail_panel(ci["upper"], hi_lab)
bound_hist <- rbind(lo_b$hist, hi_b$hist)
bound_line <- rbind(lo_b$line, hi_b$line)
bound_hist$panel <- factor(bound_hist$panel, levels = c(lo_lab, hi_lab))
bound_line$panel <- factor(bound_line$panel, levels = c(lo_lab, hi_lab))
p_bounds <- ggplot(bound_hist, aes(x = stat,
                                   y = after_stat(count) / sum(after_stat(count)),
                                   fill = region)) +
  geom_histogram(binwidth = bin_width, boundary = 0) +
  geom_vline(data = bound_line, aes(xintercept = obs), linetype = "dashed") +
  geom_text(data = bound_line, aes(x = obs, y = Inf, label = "observed"),
            inherit.aes = FALSE, vjust = 1.4, hjust = 1.1, size = 2.7) +
  facet_wrap(~ panel, ncol = 2, labeller = label_parsed) +
  scale_fill_manual(values = fills) +
  coord_cartesian(xlim = x_limits) +
  guides(fill = "none") +
  theme_bw() +
  xlab(label = "Difference-in-means on adjusted outcomes") +
  ylab(label = "Probability")
ggsave(filename = "figures/ci_bounds.pdf", plot = p_bounds,
       width = 6.6, height = 3.4, units = "in", dpi = 600)

# Hodges-Lehmann: at tau = HL the observed adjusted statistic sits at the
# null mean (0).
hl <- tail_panel(hl_estimate, "HL")
p_hl <- ggplot(hl$hist, aes(x = stat,
                            y = after_stat(count) / sum(after_stat(count)),
                            fill = region)) +
  geom_histogram(binwidth = bin_width, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate(geom = "text", x = 0, y = Inf, label = "observed = null mean",
           vjust = 1.4, hjust = -0.05, size = 2.9) +
  scale_fill_manual(values = fills) +
  coord_cartesian(xlim = x_limits) +
  guides(fill = "none") +
  theme_bw() +
  xlab(label = "Difference-in-means on adjusted outcomes") +
  ylab(label = "Probability")
ggsave(filename = "figures/hl_estimate.pdf", plot = p_hl,
       width = 6, height = 3.4, units = "in", dpi = 600)

# Normal approximation: the simulated sharp-null histogram with the N(0, V)
# density overlaid (V = the exact randomization variance computed above).
normal_curve <- data.frame(x = seq(from = x_limits[1], to = x_limits[2],
                                   length.out = 400))
normal_curve$y <- dnorm(x = normal_curve$x, mean = 0, sd = sqrt(v_null)) * bin_width
p_normal <- ggplot(data.frame(stat = null_no_effect), aes(x = stat)) +
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count))),
                 binwidth = bin_width, boundary = 0, fill = "grey75") +
  geom_line(data = normal_curve, aes(x = x, y = y), colour = "firebrick",
            linewidth = 0.7) +
  geom_vline(xintercept = t_obs, linetype = "dashed") +
  annotate(geom = "text", x = t_obs, y = Inf, label = "observed",
           hjust = 1.1, vjust = 1.4, size = 3) +
  coord_cartesian(xlim = x_limits) +
  theme_bw() +
  xlab(label = "Difference-in-means under no effect") +
  ylab(label = "Probability")
ggsave(filename = "figures/normal_approx.pdf", plot = p_normal,
       width = 6, height = 3.6, units = "in", dpi = 600)

# Size vs power (Lady Tasting Tea, t = z^T y). Reject iff t = 4, so the
# rejection region is {t = 4}; its probability is the SIZE under no
# discrimination and the POWER under perfect discrimination.
t_grid   <- 0:4
p_tea    <- choose(4, t_grid) * choose(4, 4 - t_grid) / choose(8, 4)  # hypergeometric
p_perf   <- as.numeric(t_grid == 4)                                   # all mass at 4
tea_lvls <- c("No discrimination", "Perfect discrimination")
tea_df   <- rbind(
  data.frame(t = t_grid, prob = p_tea,  panel = tea_lvls[1]),
  data.frame(t = t_grid, prob = p_perf, panel = tea_lvls[2]))
tea_df$panel  <- factor(tea_df$panel, levels = tea_lvls)
tea_df$region <- ifelse(tea_df$t == 4, "reject", "retain")
tea_lab <- data.frame(panel = factor(tea_lvls, levels = tea_lvls),
                      t = c(4, 4), prob = c(0.10, 1.0),
                      lab = c("size = 1/70", "power = 1"))
p_size_power <- ggplot(tea_df, aes(x = t, y = prob, fill = region)) +
  geom_col(width = 0.9) +
  geom_text(data = tea_lab, aes(x = t, y = prob, label = lab),
            inherit.aes = FALSE, vjust = -0.6, size = 3.2) +
  facet_wrap(~ panel) +
  scale_fill_manual(values = fills) +
  coord_cartesian(ylim = c(0, 1.15)) +
  guides(fill = "none") +
  theme_bw() +
  xlab(expression(t == bold(z)^T * bold(y))) +
  ylab(label = "Probability")
ggsave(filename = "figures/size_power_tea.pdf", plot = p_size_power,
       width = 7, height = 3.2, units = "in", dpi = 600)
