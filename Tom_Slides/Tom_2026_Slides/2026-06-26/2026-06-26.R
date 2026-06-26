##' ---
##' title: "Day 10 (June 26): Inference under As-If Randomization -- Sharp and Weak Frameworks"
##' output: github_document
##' ---
##'
##' This script reproduces every quantity, code block, and figure in
##' 2026-06-26.tex. It follows Sections 2.3-2.3.2 of Leavitt and Miratrix
##' (2026): potential outcomes and the assignment process; the set Omega of
##' possible assignments; inference under the SHARP framework (a sum statistic
##' equal to the harmonic-mean-weighted Difference-in-Means, its randomization
##' distribution -- exact and simulated -- the Normal approximation, p-values,
##' confidence sets by test inversion, and the Hodges-Lehmann estimate); and
##' inference under the WEAK framework (the ATE, its Difference-in-Means
##' estimator, the variance problem in finely stratified designs, the Fogarty
##' (2018) and Pashley-Miratrix (2021) variance estimators, the weak-null test,
##' and a confidence interval by inversion).
##'
##' Section 1 rebuilds the matched design exactly as in 2026-06-24.R so this
##' script runs on its own. The new material begins at Section 2.
##'
##' The dataset peace_pre_match.rds and the helpers hm_stat_rescale.R and
##' fine_strat_var_est.R are all sourced directly from the matching-guide
##' GitHub repository (https://github.com/tl2624/matching-guide).
##'
##' Figures produced (written to the figures/ subdirectory):
##'   * sharp_null_three_panel.pdf  (exact / simulated / Normal null distributions)
##'   * sharp_conf_set.pdf          (p-value curves and the inverted confidence set)
##'   * weak_null_normal.pdf        (standard Normal test of the weak null)

## ============================================================
## Setup
## ============================================================
## Set saveplots_ <- TRUE to (re)write the figure PDFs the slides include.
if (!exists("saveplots_")) saveplots_ <- FALSE

library(dplyr)      # data manipulation
library(optmatch)   # match_on(), exactMatch(), fullmatch()
library(randomizr)  # obtain_permutation_matrix(), declare_ra()
library(senstrat)   # ev(): Rosenbaum-Krieger (1990) null moments of a sum statistic
library(PSweight)   # PSmethod(): overlap weights (Li et al. 2018)
library(blkvar)     # block_estimator(): Pashley-Miratrix (2021) variance estimators
library(ggplot2)    # figures

## Seed for the simulated randomization distribution (Section 4).
set.seed(seed = 12345)

dir.create("figures", showWarnings = FALSE)

## Running via Rscript draws each print()ed plot to a stray Rplots.pdf; open a
## null device in non-interactive runs so that file is never written.
if (!interactive()) pdf(NULL)

alpha <- 0.05   # significance level used throughout

## ============================================================
## 1. Rebuild the matched design (from 2026-06-24.R / Section 2.2.4)
## ============================================================
data <- readRDS(url("https://raw.githubusercontent.com/tl2624/matching-guide/main/data/peace_pre_match.rds"))

covs <- c("lwdeaths", "lwdurat", "ethfrac", "pop", "lmtnest", "milper",
          "bwgdp", "bwplty2", "region")

psm <- glm(
  formula = reformulate(setdiff(x = covs, y = "region"), response = "UN"),
  family = binomial(link = "logit"),
  data = data
)
data$logit_p_score <- psm$linear.predictors
pop_sd_logit <- sqrt(mean((data$logit_p_score - mean(data$logit_p_score))^2))

ps_mat <- match_on(
  x = UN ~ logit_p_score, caliper = 0.5 * pop_sd_logit, data = data,
  standardization.scale = NULL, method = "euclidean"
)
rank_mah_mat <- match_on(
  x = reformulate(setdiff(x = covs, y = "region"), response = "UN"),
  data = data, standardization.scale = NULL, method = "rank_mahalanobis"
)
eth_mat <- match_on(
  x = UN ~ ethfrac, caliper = 35, data = data,
  standardization.scale = NULL, method = "euclidean"
)
bwgdp_mat <- match_on(
  x = UN ~ bwgdp, caliper = 2, data = data,
  standardization.scale = NULL, method = "euclidean"
)
em_region <- exactMatch(x = UN ~ region, data = data)

fm <- fullmatch(
  x = ps_mat + rank_mah_mat + eth_mat + bwgdp_mat + em_region,
  data = data, max.controls = 4
)
data$fm <- fm

## ============================================================
## 2. Potential outcomes and the assignment process
## ============================================================
## Keep only matched units. The causal targets are defined among these units:
## individual effects tau_si = y_si(1) - y_si(0) and the ATE. We observe only
## one potential outcome per unit (the observed outcome ldur).
data_matched <- filter(.data = data, !is.na(fm))

## Omega_s = all assignments in set s holding the observed treated count fixed.
## In set ssafrica.3 there are 4 units, 1 treated, so |Omega_s| = choose(4,1) = 4.
ssafrica_3 <- data_matched |>
  filter(fm == "ssafrica.3") |>
  select(cname, UN)
n_set <- nrow(ssafrica_3)
m_set <- sum(ssafrica_3$UN)
omega_ssafrica_3 <- obtain_permutation_matrix(
  declaration = declare_ra(N = n_set, m = m_set)
)
rownames(omega_ssafrica_3) <- ssafrica_3$cname
omega_ssafrica_3   # the choose(4, 1) = 4 possible assignments (Table 2)

## Total number of assignments in Omega = product over sets of choose(n_s, m_s).
block_ns <- data_matched |>
  group_by(fm) |>
  summarise(n = n(), m = sum(UN), .groups = "drop")
total_assigns <- prod(choose(n = block_ns$n, k = block_ns$m))
total_assigns   # 311,040

## ============================================================
## 2b. A small illustration: removing the true effect centers the test
## ============================================================
## Four "village heads", 2 treated and 2 control, with a CONSTANT effect tau = 6.
## Observed outcome y = y0 + tau * z. Test statistic = treated mean - control mean.
## Under H0: tau_h we adjust outcomes y_tilde = y - tau_h * z, hold them fixed,
## and re-randomize the labels over the C(4, 2) = 6 ways to pick 2 treated.
vh <- data.frame(
  unit = c("V1", "V2", "V3", "V4"),
  z    = c(0, 1, 0, 1),     # observed assignment (V2, V4 treated)
  y0   = c(1, 2, 3, 4)      # control potential outcomes
)
vh_tau <- 6
vh$y <- vh$y0 + vh_tau * vh$z              # observed outcomes: 1, 8, 3, 10

vh_assigns <- combn(x = 4, m = 2)         # the 6 within-set assignments
vh_stat <- function(yvec, treated_idx) {
  mean(yvec[treated_idx]) - mean(yvec[-treated_idx])
}
vh_dist <- function(yvec) {
  apply(X = vh_assigns, MARGIN = 2, FUN = function(idx) vh_stat(yvec, idx))
}

obs_idx      <- which(vh$z == 1)          # observed treated = V2, V4
y_tilde_true <- vh$y - vh_tau * vh$z      # remove the TRUE effect -> recovers y0
dist_h0   <- vh_dist(vh$y)                # tau_h = 0: no adjustment
dist_true <- vh_dist(y_tilde_true)        # tau_h = 6: true effect removed
obs_h0    <- vh_stat(vh$y, obs_idx)       # 7  (extreme: in the upper tail)
obs_true  <- vh_stat(y_tilde_true, obs_idx)  # 1  (near the center, ~0)
c(obs_h0 = obs_h0, obs_true = obs_true)

## Both null distributions are centered at 0; what differs is where the OBSERVED
## statistic lands. Removing the true effect drops it from the tail to the center.
## Each of the 6 assignments is equally likely (prob 1/6); we stack the balls so a
## value's TALLEST ball sits at its probability = (number of assignments) / 6.
lab_h0   <- "No~adjustment:~tau[h]==0"
lab_true <- "True~effect~removed:~tau[h]==6"
n_assign <- ncol(vh_assigns)              # 6
vh_fig_df <- rbind(
  data.frame(stat = dist_h0,   panel = lab_h0),
  data.frame(stat = dist_true, panel = lab_true)
)
## Each ball = one unique assignment vector; under complete random assignment all
## are equally likely (prob 1/6), so all balls are the SAME size and we stack them
## FLUSH on top of one another (geom_dotplot, auto-fitting the panel as in the
## original). A value's probability = (# balls)/6, so a taller stack = more likely;
## the caption supplies the per-ball probability.
vh_obs_df <- data.frame(
  panel = c(lab_h0, lab_true),
  obs   = c(obs_h0, obs_true),
  hj    = c(1.12, -0.12)                  # left of the line (right panel edge); right of it
)
village_heads_centering <- ggplot(data = vh_fig_df, mapping = aes(x = stat)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_vline(data = vh_obs_df, mapping = aes(xintercept = obs),
             color = "#08519c", linewidth = 1) +
  geom_dotplot(binwidth = 1, method = "histodot", dotsize = 1, stackratio = 1,
               fill = "grey60", color = "white") +
  geom_text(data = vh_obs_df,
            mapping = aes(x = obs, y = Inf, label = paste0("observed = ", obs),
                          hjust = hj),
            vjust = 1.4, color = "#08519c", size = 3.1, inherit.aes = FALSE) +
  facet_wrap(facets = ~ panel, labeller = label_parsed) +
  scale_x_continuous(expand = expansion(mult = 0.14)) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(clip = "off") +
  xlab(label = "Difference-in-means (treated - control)") +
  ylab(label = "Probability") +
  labs(caption = "Each ball = one of 6 equally likely assignment vectors (1/6); a value's probability = (# balls) / 6.") +
  theme_bw(base_size = 12) +
  theme(plot.margin = margin(t = 14, r = 8, b = 4, l = 4),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0.5))

print(village_heads_centering)

##+ eval=saveplots_
ggsave(
  filename = "figures/village_heads_centering.pdf",
  plot = village_heads_centering,
  width = 8, height = 3.4, units = "in", dpi = 300
)

## ============================================================
## 3. Sharp framework: reconstruct outcomes, build the sum statistic
## ============================================================
## A sum statistic adds treated outcomes within each set, then across sets. We
## test a homogeneous (constant) effect tau_h by RECONSTRUCTING the outcomes
## treated units would show under that effect: ldur_tilde = ldur - tau_h * UN.
## Under the null these reconstructed outcomes are fixed across assignments.
tau_h <- 0
data_matched <- data_matched |>
  mutate(ldur_tilde = ldur - tau_h * UN)

## Source hm_stat_rescale(): it rescales outcomes so the SUM over treated units
## equals the harmonic-mean-weighted Difference-in-Means.
source("https://raw.githubusercontent.com/tl2624/matching-guide/main/R/hm_stat_rescale.R")

data_matched <- hm_stat_rescale(
  data = data_matched,
  outcome = ldur_tilde,  # outcome to rescale within sets
  treat = UN,            # 0/1 treatment indicator
  strata = fm            # matched set id
)

## Observed sum statistic = sum of rescaled outcomes among treated units.
obs_stat <- sum(data_matched$ldur_tilde_hm_scaled[data_matched$UN == 1])
obs_stat   # ~0.673

## --- The sum statistic equals three familiar quantities -------------------
## (i) the harmonic-mean-weighted average of within-set Differences-in-Means.
dim_hm <- data_matched |>
  group_by(fm) |>
  summarize(
    n_treated = sum(UN == 1),
    n_control = sum(UN == 0),
    dim_set   = mean(ldur_tilde[UN == 1]) - mean(ldur_tilde[UN == 0]),
    # harmonic-mean weight = set's contribution to the effective sample size
    w_hm      = 2 * n_treated * n_control / (n_treated + n_control),
    .groups   = "drop"
  ) |>
  summarize(dim_hm = sum(w_hm * dim_set) / sum(w_hm)) |>
  pull(dim_hm)
all.equal(dim_hm, obs_stat)   # TRUE

## (ii) the coefficient on UN in a fixed-effects (matched-set) regression.
fe_fit <- lm(formula = ldur_tilde ~ UN + fm, data = data_matched)
all.equal(unname(coef(fe_fit)["UN"]), obs_stat)   # TRUE

## (iii) the overlap-weighted Difference-in-Means built from the as-if
## assignment probabilities (control weight = prob, treated weight = 1 - prob).
ps_fit <- PSmethod(
  ps.formula = UN ~ factor(fm),   # reproduces n_treated / n in each set
  method     = "glm",
  data       = as.data.frame(data_matched),
  ncate      = 2
)
assign_prob <- ps_fit$e.h[, "1"]
w_ow <- ifelse(test = data_matched$UN == 1, yes = 1 - assign_prob, no = assign_prob)
dim_ow <- with(
  data_matched,
  sum(w_ow[UN == 1] * ldur_tilde[UN == 1]) / sum(w_ow[UN == 1]) -
    sum(w_ow[UN == 0] * ldur_tilde[UN == 0]) / sum(w_ow[UN == 0])
)
all.equal(dim_ow, obs_stat)   # TRUE

## ============================================================
## 4. The randomization distribution under the sharp null
## ============================================================
## Hold the reconstructed, rescaled outcomes fixed and recompute the sum
## statistic over the assignments in Omega. Our study is small enough to
## enumerate Omega exactly.
exact_assigns <- obtain_permutation_matrix(
  declaration = declare_ra(
    N = nrow(data_matched),
    blocks = data_matched$fm,
    block_m = block_ns$m
  ),
  maximum_permutations = total_assigns
)
q_tau_h_0 <- data_matched$ldur_tilde_hm_scaled
exact_sharp_null_dist <- as.numeric(t(q_tau_h_0) %*% exact_assigns)

## In most applications Omega is too large to enumerate; we sample assignments
## (here 10,000) to approximate the randomization distribution.
sim_assigns <- obtain_permutation_matrix(
  declaration = declare_ra(
    N = nrow(data_matched),
    blocks = data_matched$fm,
    block_m = block_ns$m
  ),
  maximum_permutations = 10^4
)
sim_sharp_null_dist <- as.numeric(t(q_tau_h_0) %*% sim_assigns)

## One-sided (upper) p-values: proportion of null statistics >= observed.
exact_upper_p_value <- mean(exact_sharp_null_dist >= obs_stat)
sim_upper_p_value   <- mean(sim_sharp_null_dist   >= obs_stat)
round(c(exact = exact_upper_p_value, simulated = sim_upper_p_value), digits = 4)

## ============================================================
## 5. Normal approximation (Rosenbaum and Krieger 1990 moments)
## ============================================================
## A fast alternative: standardize the observed statistic by its null
## expectation and variance (summed over sets) and refer to the standard Normal.
per_block_moms <- data_matched |>
  group_by(fm) |>
  summarize(
    expect   = ev(sc = ldur_tilde_hm_scaled, z = UN, m = 1, g = 1, method = "RK")$expect,
    variance = ev(sc = ldur_tilde_hm_scaled, z = UN, m = 1, g = 1, method = "RK")$vari,
    .groups  = "drop"
  )
null_ev  <- sum(per_block_moms$expect)    # 0 for our centered statistic
null_var <- sum(per_block_moms$variance)
null_sd  <- sqrt(null_var)

z_sharp <- (obs_stat - null_ev) / null_sd
norm_upper_p_value <- pnorm(q = z_sharp, lower.tail = FALSE)
round(norm_upper_p_value, digits = 4)

## Two-sided p-value (exact null): the deck shows BOTH tails. The one-sided
## upper test rejects at alpha = 0.05; the two-sided test is borderline.
exact_two_sided_p_value <- 2 * min(
  mean(exact_sharp_null_dist >= obs_stat),
  mean(exact_sharp_null_dist <= obs_stat)
)
round(exact_two_sided_p_value, digits = 4)

## ============================================================
## 6. Sharp confidence sets and the Hodges-Lehmann estimate
## ============================================================
## A confidence set = the null values NOT rejected by the test. Normal-based:
## one-sided (upper tail) excludes 0; two-sided (alpha/2 each tail) includes 0
## -- consistent with the borderline two-sided test.
cs_sharp_one_sided <- c(
  lower = obs_stat - qnorm(p = 1 - alpha) * null_sd,
  upper = Inf
)
cs_sharp_two_sided <- c(
  lower = obs_stat - qnorm(p = 1 - alpha / 2) * null_sd,
  upper = obs_stat + qnorm(p = 1 - alpha / 2) * null_sd
)
cs_sharp_one_sided
cs_sharp_two_sided

## Hodges-Lehmann point estimate: the null value tau_h making the observed sum
## statistic equal its null expectation (0). It equals the harmonic-mean-
## weighted Difference-in-Means computed from the observed outcomes.
hl_estimate <- obs_stat
hl_estimate

## ============================================================
## 7. Figure: three reference distributions for the sharp null
## ============================================================
## Side-by-side: exact (enumerate Omega), simulated (10,000 draws), and the
## Normal approximation. Each panel shades the TWO-SIDED rejection regions
## (alpha/2 per tail), marks the one-sided upper cutoff (dashed), and the
## observed statistic (solid). Observed sits past the one-sided cutoff (reject
## one-sided) but inside the upper two-sided region (borderline two-sided).
panel_levels <- c("Exact (all assignments)",
                  "Simulated (10,000 draws)",
                  "Normal approximation")

## One-sided upper rejection cutoff: the 0.95 quantile of the exact null
## (the test is one-sided -- only the relevant tail is the rejection region).
crit_one_hi <- as.numeric(quantile(exact_sharp_null_dist, probs = 1 - alpha))

## Histogram data for the exact and simulated panels; flag each draw by whether
## it lies in the upper-tail (0.05) rejection region so we can shade the bars.
hist_df <- data.frame(
  stat  = c(exact_sharp_null_dist, sim_sharp_null_dist),
  panel = factor(
    c(rep(panel_levels[1], length(exact_sharp_null_dist)),
      rep(panel_levels[2], length(sim_sharp_null_dist))),
    levels = panel_levels
  )
)
hist_df$region <- ifelse(hist_df$stat >= crit_one_hi, "reject", "keep")

## Normal-approximation panel: the density curve and its upper-tail area.
norm_grid <- seq(from = -1.4, to = 1.4, length.out = 600)
norm_df <- data.frame(
  x     = norm_grid,
  dens  = dnorm(x = norm_grid, mean = null_ev, sd = null_sd),
  panel = factor(panel_levels[3], levels = panel_levels)
)
norm_rej_df <- subset(norm_df, x >= crit_one_hi)
obs_df <- data.frame(panel = factor(panel_levels, levels = panel_levels))
fill_vals <- c(keep = "grey78", reject = "#d7301f")

sharp_null_three_panel <- ggplot() +
  geom_histogram(
    data = hist_df,
    mapping = aes(x = stat, y = after_stat(count), fill = region),
    bins = 38
  ) +
  geom_line(
    data = norm_df, mapping = aes(x = x, y = dens), linewidth = 0.8
  ) +
  geom_area(
    data = norm_rej_df, mapping = aes(x = x, y = dens),
    fill = "#d7301f", alpha = 0.85
  ) +
  geom_vline(
    data = obs_df, aes(xintercept = obs_stat),
    color = "#08519c", linewidth = 1
  ) +
  facet_wrap(facets = ~ panel, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = fill_vals, guide = "none") +
  xlab(label = "Harmonic-mean weighted Difference-in-Means") +
  ylab(label = NULL) +
  labs(
    caption = paste0(
      "Shaded: upper-tail rejection region (area = 0.05). ",
      "Blue line: observed = ", round(obs_stat, 2),
      " (inside the rejection region)."
    )
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0))

print(sharp_null_three_panel)

##+ eval=saveplots_
ggsave(
  filename = "figures/sharp_null_three_panel.pdf",
  plot = sharp_null_three_panel,
  width = 10, height = 3.8, units = "in", dpi = 300
)

## ============================================================
## 8. Figure: the sharp confidence set by test inversion
## ============================================================
## A (1 - alpha) confidence set = the null constant effects we do NOT reject. On
## the standard-Normal reference the two-sided test keeps standardized statistics
## in [-z, z]; each kept value maps to a null tau_h = obs_stat - (stat)*null_sd.
## The endpoints obs_stat -/+ z*null_sd are the two-sided 95% CI [-0.05, 1.40].
z_crit    <- qnorm(p = 1 - alpha / 2)              # 1.96
stat_grid <- seq(from = -z_crit, to = z_crit, length.out = 5)
null_grid <- obs_stat - stat_grid * null_sd        # null tau_h mapped to each stat

stand_norm_df <- data.frame(
  x    = seq(from = -3.5, to = 3.5, length.out = 1000),
  dens = dnorm(seq(from = -3.5, to = 3.5, length.out = 1000))
)

sharp_ci_inversion <- ggplot(data = stand_norm_df, mapping = aes(x = x, y = dens)) +
  geom_area(data = subset(stand_norm_df, x <= qnorm(alpha / 2)),
            fill = "#fdae61", alpha = 0.9) +
  geom_area(data = subset(stand_norm_df, x >= qnorm(1 - alpha / 2)),
            fill = "#fdae61", alpha = 0.9) +
  geom_line(color = "black") +
  geom_vline(xintercept = stat_grid, linetype = "dashed",
             color = "#2a9d8f", alpha = 0.85) +
  annotate(geom = "text", x = stat_grid, y = 0.27,
           label = paste0("tau[h] == ", round(null_grid, 2)), parse = TRUE,
           angle = 90, vjust = 1, hjust = 1, color = "#2a9d8f", size = 2.9) +
  scale_x_continuous(breaks = c(-3, -1.96, 0, 1.96, 3),
                     labels = c("-3", "-1.96", "0", "1.96", "3"),
                     limits = c(-3.5, 3.5)) +
  xlab(label = "Standardized statistic under each candidate null") +
  ylab(label = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(sharp_ci_inversion)

##+ eval=saveplots_
ggsave(
  filename = "figures/sharp_ci_inversion.pdf",
  plot = sharp_ci_inversion,
  width = 6.8, height = 4, units = "in", dpi = 300
)

## ============================================================
## 8b. Figure: the Hodges-Lehmann point estimate
## ============================================================
## The HL estimate is the null value tau_h at which the observed statistic equals
## its null expectation (0). At tau_h = hl_estimate, reconstruct/rescale outcomes
## and verify the observed statistic sits exactly at the center (0) of the
## randomization distribution -- the data look like no effect remains.
hl_dat <- hm_stat_rescale(
  data    = transform(data_matched, ldur_tilde_hl = ldur - hl_estimate * UN),
  outcome = ldur_tilde_hl, treat = UN, strata = fm
)
q_hl         <- hl_dat$ldur_tilde_hl_hm_scaled
hl_null_dist <- as.numeric(t(q_hl) %*% sim_assigns)
obs_hl       <- sum(hl_dat$UN * q_hl)              # ~ 0 by construction
round(obs_hl, 6)

sharp_hl_estimate <- ggplot(data = data.frame(stat = hl_null_dist),
                            mapping = aes(x = stat)) +
  geom_histogram(bins = 40, fill = "grey78") +
  geom_vline(xintercept = obs_hl, color = "#08519c", linewidth = 1) +
  annotate(geom = "text", x = obs_hl, y = Inf, label = "observed = null mean (0)",
           vjust = 1.6, hjust = -0.03, size = 3.2, color = "#08519c") +
  xlab(label = expression("Statistic at " * tau[h] * " = Hodges-Lehmann estimate (0.67)")) +
  ylab(label = NULL) +
  theme_bw(base_size = 12) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(sharp_hl_estimate)

##+ eval=saveplots_
ggsave(
  filename = "figures/sharp_hl_estimate.pdf",
  plot = sharp_hl_estimate,
  width = 6.8, height = 3.6, units = "in", dpi = 300
)

## ============================================================
## 9. Weak framework: the ATE and its Difference-in-Means estimator
## ============================================================
## The ATE is a weighted average of set-level effects with weights equal to each
## set's SHARE of units: tau = sum_s (n_s / n) tau_s. The matching estimator
## uses the same weights -- these usual weights DEFINE THE ESTIMAND. (The
## harmonic-mean weights were only the test statistic's weights in Section 3;
## they do not redefine the causal target.)
set_stats <- data_matched |>
  group_by(fm) |>
  summarize(
    n = n(),                                                  # set size n_s
    diff_in_means = mean(ldur[UN == 1L]) - mean(ldur[UN == 0L]),  # tau_hat_s
    .groups = "drop"
  )
n_total  <- sum(set_stats$n)
ate_hat  <- sum((set_stats$n / n_total) * set_stats$diff_in_means)
ate_hat   # ~0.615

## ============================================================
## 10. Weak framework: estimating the variance of the ATE estimator
## ============================================================
## The usual (Neyman) approach estimates a within-treated and within-control
## sample variance in each set -- but var() divides by (n - 1), so it is
## undefined in a set with a single treated OR single control unit. Every set
## here has exactly one treated or one control unit, so the usual estimator
## fails. Two solutions avoid the (n - 1) problem.

## --- Fogarty (2018): finely stratified variance estimator -----------------
## Works at the SET level: it regresses the set-level effect estimates on the
## set weights and uses the residual variation, so it never needs a within-set
## (n - 1) variance. With many sets it is consistent for an UPPER BOUND on the
## true variance -- conservative, hence valid.
source("https://raw.githubusercontent.com/tl2624/matching-guide/main/R/fine_strat_var_est.R")
var_fogarty <- fine_strat_var_est(
  strat_ns   = set_stats$n,              # vector of set sizes
  strat_ests = set_stats$diff_in_means   # vector of set-level estimates
)
se_fogarty <- sqrt(var_fogarty)
c(var_fogarty = var_fogarty, se_fogarty = se_fogarty)

## --- Pashley and Miratrix (2021): hybrid estimators -----------------------
## hybrid_m requires >= 2 matched sets of every unique set size; hybrid_p only
## requires that no single set hold half or more of the units. Our set sizes
## are 2, 3, 4, 5 (size 5 occurs once), so hybrid_m does not apply; the largest
## set holds 5 / 36 = 14% < 50%, so hybrid_p does.
largest_set_share <- max(block_ns$n) / sum(block_ns$n)
largest_set_share   # 0.139 < 0.5

res <- block_estimator(
  Yobs   = ldur,         # observed outcomes
  Z      = UN,           # treatment indicator
  B      = fm,           # block (matched set) membership
  data   = data_matched,
  method = "hybrid_p"    # Pashley-Miratrix variance estimator
)
se_hybrid_p <- sqrt(res$var_est)
c(ATE_hat = res$ATE_hat, var_hybrid_p = res$var_est, se_hybrid_p = se_hybrid_p)

## The two variance estimates are nearly identical.
round(c(fogarty = var_fogarty, hybrid_p = res$var_est), digits = 4)

## ============================================================
## 10b. Figure: why a conservative variance still gives a valid test
## ============================================================
## Both estimators converge to an UPPER BOUND on the true Var(tau_hat): the extra
## piece is the across-set variance of the true set-level effects (heterogeneity),
## which cannot be separated from sampling noise with one unit per arm. So the
## standardized statistic's limiting law is NARROWER than the standard Normal, and
## the upper-tail rejection area is <= alpha -- the test never over-rejects.
consv_c <- 0.8   # illustrative probability limit of (true se / conservative se) <= 1
xg        <- seq(from = -3.5, to = 3.5, length.out = 1000)
consv_df  <- data.frame(x = xg, dens = dnorm(xg))
narrow_df <- data.frame(x = xg, dens = dnorm(xg, mean = 0, sd = consv_c))
consv_cut <- qnorm(1 - alpha)

conservative_variance <- ggplot() +
  geom_area(data = subset(consv_df, x >= consv_cut), mapping = aes(x = x, y = dens),
            fill = "#fdae61", alpha = 0.5) +
  geom_area(data = subset(narrow_df, x >= consv_cut), mapping = aes(x = x, y = dens),
            fill = "#d7301f", alpha = 0.85) +
  geom_line(data = consv_df,  mapping = aes(x = x, y = dens), color = "black") +
  geom_line(data = narrow_df, mapping = aes(x = x, y = dens), color = "gray40",
            linetype = "dashed") +
  geom_vline(xintercept = consv_cut, linetype = "dotted", color = "grey55") +
  annotate(geom = "text", x = -0.15, y = 0.31, hjust = 1, size = 3.1,
           label = "if we knew the true variance") +
  annotate(geom = "text", x = 0.2, y = 0.52, hjust = 0, size = 3.1, color = "gray35",
           label = "conservative variance:\nnarrower limiting law") +
  annotate(geom = "text", x = consv_cut + 0.85, y = 0.02, size = 3.3,
           label = "area <= 0.05") +
  xlab(label = "Standardized statistic") + ylab(label = NULL) +
  scale_x_continuous(limits = c(-3.5, 3.5)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(conservative_variance)

##+ eval=saveplots_
ggsave(
  filename = "figures/conservative_variance.pdf",
  plot = conservative_variance,
  width = 6.8, height = 4, units = "in", dpi = 300
)

## ============================================================
## 11. Weak framework: test of the weak null and a confidence interval
## ============================================================
## Standardize the ATE estimate by its estimated standard error and refer to the
## standard Normal. We use the Fogarty standard error (conservative). The
## upper one-sided test rejects the weak null ATE = 0 at alpha = 0.05; the
## two-sided test is borderline (the two-sided CI just includes 0).
z_weak <- (ate_hat - 0) / se_fogarty
weak_upper_p_value <- pnorm(q = z_weak, lower.tail = FALSE)
c(z_weak = z_weak, weak_upper_p_value = weak_upper_p_value)

## 95% confidence intervals by inverting the test (Fogarty SE).
cs_weak_one_sided <- c(
  lower = ate_hat - qnorm(p = 1 - alpha) * se_fogarty,
  upper = Inf
)
cs_weak_two_sided <- c(
  lower = ate_hat - qnorm(p = 1 - alpha / 2) * se_fogarty,
  upper = ate_hat + qnorm(p = 1 - alpha / 2) * se_fogarty
)
cs_weak_one_sided
cs_weak_two_sided

## ============================================================
## 12. Figure: the weak-null test against the standard Normal
## ============================================================
## Standard Normal reference. Shade the two-sided rejection regions (|z| >=
## 1.96), mark the one-sided upper cutoff (1.645, dashed), and the observed
## standardized statistic (solid). It sits past 1.645 (reject one-sided) but
## inside 1.96 (borderline two-sided).
z_crit_one <- qnorm(p = 1 - alpha)       # 1.645 (one-sided upper cutoff)
z_grid <- seq(from = -4, to = 4, length.out = 600)
weak_df <- data.frame(z = z_grid, dens = dnorm(z_grid))

## One-sided upper test: shade only the relevant (upper) rejection region, area 0.05.
weak_null_normal <- ggplot(data = weak_df, mapping = aes(x = z, y = dens)) +
  geom_area(
    data = subset(weak_df, z >= z_crit_one),
    fill = "#d7301f", alpha = 0.6
  ) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = z_weak, color = "#08519c", linewidth = 1) +
  annotate(
    geom = "text", x = z_weak, y = 0.30,
    label = paste0("observed z = ", round(z_weak, 2)),
    color = "#08519c", hjust = -0.06, size = 3.4
  ) +
  annotate(
    geom = "text", x = z_crit_one + 0.95, y = 0.045, size = 3.3,
    label = "area = 0.05"
  ) +
  xlab(label = expression("Standardized weak-null statistic " *
                            (hat(tau) - 0) / hat(se))) +
  ylab(label = NULL) +
  scale_x_continuous(limits = c(-4, 4)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(weak_null_normal)

##+ eval=saveplots_
ggsave(
  filename = "figures/weak_null_normal.pdf",
  plot = weak_null_normal,
  width = 7, height = 4, units = "in", dpi = 300
)

## ============================================================
## 13. Key numbers behind the slides
## ============================================================
cat("---- SHARP framework ----\n")
cat("Observed sum statistic (HM-weighted Diff-in-Means): ",
    round(obs_stat, digits = 3), "\n")
cat("Total assignments in Omega:                          ",
    format(total_assigns, scientific = FALSE, big.mark = ","), "\n")
cat("Upper one-sided p -- exact / simulated / Normal:     ",
    round(exact_upper_p_value, 4), "/",
    round(sim_upper_p_value, 4), "/",
    round(norm_upper_p_value, 4), "\n")
cat("Two-sided p (exact):                                 ",
    round(exact_two_sided_p_value, 4), "\n")
cat("Sharp one-sided 95% CI (Normal):  [",
    round(cs_sharp_one_sided["lower"], 3), ", Inf)\n", sep = "")
cat("Sharp two-sided 95% CI (Normal):  [",
    round(cs_sharp_two_sided["lower"], 3), ", ",
    round(cs_sharp_two_sided["upper"], 3), "]\n", sep = "")
cat("Hodges-Lehmann point estimate:                       ",
    round(hl_estimate, digits = 3), "\n\n")

cat("---- WEAK framework ----\n")
cat("ATE estimate (n_s/n-weighted Diff-in-Means):         ",
    round(ate_hat, digits = 3), "\n")
cat("SE -- Fogarty / hybrid_p:                            ",
    round(se_fogarty, 3), "/", round(se_hybrid_p, 3), "\n")
cat("Weak-null standardized statistic z (Fogarty):        ",
    round(z_weak, digits = 3), "\n")
cat("Weak-null upper one-sided p (Fogarty):               ",
    round(weak_upper_p_value, digits = 4), "\n")
cat("Weak two-sided 95% CI (Fogarty):  [",
    round(cs_weak_two_sided["lower"], 3), ", ",
    round(cs_weak_two_sided["upper"], 3), "]\n", sep = "")
