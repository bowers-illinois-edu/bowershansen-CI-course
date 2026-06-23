##' ---
##' title: "Day 6: Noncompliance, Attrition, and the CACE"
##' output: github_document
##' ---
##'
##' This script reproduces every quantity, code block, and figure in
##' 2026-06-22.tex. It continues the design-based, finite-population framework of
##' 2026-06-17.R and 2026-06-18.R: potential outcomes are fixed, randomness comes
##' only from the assignment Z, and the Difference-in-Means is the workhorse
##' estimator. Today the assigned treatment Z and the received treatment D come
##' apart (noncompliance), so the estimand of interest is the Complier Average
##' Causal Effect (CACE = ITT_Y / ITT_D), a RATIO of two Difference-in-Means.
##'
##' Running example: the 1980 Adams and Smith telephone GOTV experiment, a clean
##' case of ONE-SIDED noncompliance (no control subject can be contacted). The
##' cell counts are taken from the course materials (per_protocol_vs_itt_iv.Rmd):
##'   * N = 2650, n1 = n0 = 1325 (complete randomization)
##'   * Telephone arm (z = 1): 950 contacted (d = 1), 375 not (d = 0); 392 voted
##'   * Control arm   (z = 0): 0 contacted (one-sided); 315 voted
##'
##' Figures produced here (written to the figures/ subdirectory):
##'   * gotv_itt_joint_dist.pdf   (joint randomization dist of ITT_Y_hat, ITT_D_hat)
##'   * gotv_cace_sampling_dist.pdf (CACE_hat is approx. Normal -- delta method)

## ============================================================
## Setup
## ============================================================
## Set saveplots_ <- TRUE to (re)write the figure PDFs the slides include.
if (!exists("saveplots_")) saveplots_ <- FALSE

library(ggplot2)
library(viridis)   # color-blind-friendly fills, matching the other decks
library(plot3D)    # static 3D surfaces (base graphics; no OpenGL needed)

## Figures are written to the figures/ subdirectory, which the slides source.
dir.create("figures", showWarnings = FALSE)

## The workhorse estimator from Days 4-5: treated mean minus control mean. Applied
## to the OUTCOME y it estimates ITT_Y; applied to the RECEIVED treatment d it
## estimates ITT_D (the first stage).
diff_in_means <- function(z, y) mean(y[z == 1]) - mean(y[z == 0])

## Conservative estimator of the variance of a Difference-in-Means (Day 5): the
## two estimable arm pieces. It drops the unestimable -S2_tau/N term, so its
## expectation is an UPPER BOUND on the true variance.
conservative_var <- function(z, y) {
  var(y[z == 1]) / sum(z == 1) + var(y[z == 0]) / sum(z == 0)
}

## Conservative variance of the CACE (Wald) estimator: build the feasible
## transformed outcome W_hat = y - cace_hat * d, take its conservative
## Difference-in-Means variance, and divide by the squared first stage. This is
## the function the slides hand the students for the exercise.
cace_var <- function(z, y, d) {
  itt_d_hat <- diff_in_means(z = z, y = d)
  cace_hat  <- diff_in_means(z = z, y = y) / itt_d_hat
  w_hat     <- y - cace_hat * d
  conservative_var(z = z, y = w_hat) / itt_d_hat^2
}

alpha   <- 0.05                 # significance level throughout
z_crit  <- qnorm(p = 1 - alpha / 2)   # 1.96

## ============================================================
## 1. Build the Adams-Smith GOTV data from its cell counts
## ============================================================
## We reconstruct the individual-level data (every row is one potential voter)
## from the published 2 x 2 x 2 cell counts. y = 1 if the subject voted; d = 1 if
## the subject was actually contacted by a caller; z = 1 if assigned a call.
make_rows <- function(z, d, n_voted, n_notvoted) {
  data.frame(z = z, d = d,
             y = c(rep(1, n_voted), rep(0, n_notvoted)))
}

gotv <- rbind(
  make_rows(z = 1, d = 1, n_voted = 310, n_notvoted = 640),  # telephoned, contacted
  make_rows(z = 1, d = 0, n_voted =  82, n_notvoted = 293),  # telephoned, not reached
  make_rows(z = 0, d = 0, n_voted = 315, n_notvoted = 1010)  # control (never contacted)
)

z_obs <- gotv$z
d_obs <- gotv$d
y_obs <- gotv$y

N  <- nrow(gotv)                 # 2650
n1 <- sum(z_obs == 1)            # 1325
n0 <- sum(z_obs == 0)            # 1325
print(c(N = N, n1 = n1, n0 = n0))

## The design table the students see (assignment x contact):
print(xtabs(formula = ~ z + d, data = gotv))
## Turnout by assignment arm and by contact status:
print(round(c(turnout_treated = mean(y_obs[z_obs == 1]),
              turnout_control = mean(y_obs[z_obs == 0]),
              turnout_contacted    = mean(y_obs[d_obs == 1]),
              turnout_notcontacted = mean(y_obs[d_obs == 0])), 4))

## ============================================================
## 2. Per-protocol vs. ITT: why "analyze as treated" misleads
## ============================================================
## The per-protocol contrast compares the contacted to the not-contacted. It is
## confounded: contact (d) is NOT randomly assigned, so the two groups differ in
## ways beyond the call itself.
per_protocol <- mean(y_obs[d_obs == 1]) - mean(y_obs[d_obs == 0])   # ~ 0.092
print(round(c(per_protocol = per_protocol), 4))

## ============================================================
## 3. The three effects of assignment: ITT_Y, ITT_D, and the CACE
## ============================================================
## ITT_Y : effect of ASSIGNMENT on the outcome (reduced form).
## ITT_D : effect of ASSIGNMENT on receipt of treatment (first stage).
## CACE  : effect of RECEIPT among Compliers = ITT_Y / ITT_D (Wald / Bloom ratio).
itt_y_hat <- diff_in_means(z = z_obs, y = y_obs)   # ~ 0.0581
itt_d_hat <- diff_in_means(z = z_obs, y = d_obs)   # ~ 0.7170 (one-sided: = share contacted)
cace_hat  <- itt_y_hat / itt_d_hat                 # ~ 0.0810
print(round(c(itt_y_hat = itt_y_hat, itt_d_hat = itt_d_hat, cace_hat = cace_hat), 4))

## ============================================================
## 4. Standard error of the CACE: the finite-population delta method
## ============================================================
## CACE_hat = A / B with A = ITT_Y_hat, B = ITT_D_hat. The delta method linearizes
## g(A, B) = A / B around the truth; its gradient is (1/B, -A/B^2). The resulting
## approximate variance combines THREE pieces -- Var(A), Var(B), and Cov(A, B):
##
##   Var(CACE_hat) ~ Var(A)/B^2 - 2 A Cov(A,B)/B^3 + A^2 Var(B)/B^4.
##
## (a) The three estimable design-based pieces, each a Day-5 conservative estimate.
var_itt_y <- conservative_var(z = z_obs, y = y_obs)
var_itt_d <- conservative_var(z = z_obs, y = d_obs)
## Covariance of two Difference-in-Means built from the SAME assignment: sum the
## within-arm sample covariances of (y, d), divided by the arm sizes.
cov_itt_yd <- cov(y_obs[z_obs == 1], d_obs[z_obs == 1]) / n1 +
              cov(y_obs[z_obs == 0], d_obs[z_obs == 0]) / n0
print(round(c(var_itt_y = var_itt_y, var_itt_d = var_itt_d,
              cov_itt_yd = cov_itt_yd), 6))

## Plug the gradient pieces into the delta-method variance.
term_varA <-  var_itt_y / itt_d_hat^2                  # Var(A)/B^2
term_cov  <- -2 * itt_y_hat * cov_itt_yd / itt_d_hat^3 # -2 A Cov(A,B)/B^3
term_varB <-  itt_y_hat^2 * var_itt_d / itt_d_hat^4    # A^2 Var(B)/B^4
var_cace_grad <- term_varA + term_cov + term_varB
## The three terms, so the slide can show that the first dominates HERE only
## because the first stage is strong and precise (ITT_D large, Var(B) small) and
## Cov is small; with a WEAK instrument the last two terms are not negligible.
print(round(c(term_varA = term_varA, term_cov = term_cov,
              term_varB = term_varB, total = var_cace_grad), 7))

## (b) The SAME variance via the TRANSFORMED (adjusted) outcome W = y - CACE * d.
## Because the Difference-in-Means is linear in the outcome,
##   A - CACE * B = ITT_hat of W,   so   Var(CACE_hat) ~ Var(ITT_hat_W) / B^2.
## The cross-covariance term is absorbed automatically into Var(W). The feasible
## version plugs in CACE_hat for the unknown CACE: W_hat = y - cace_hat * d.
w_obs <- y_obs - cace_hat * d_obs

## The two estimable, within-arm pieces of the conservative variance (before the
## 1/ITT_D^2 scaling). These are the numbers the variance slide reports.
s2_w_treated  <- var(w_obs[z_obs == 1])
s2_w_control  <- var(w_obs[z_obs == 0])
piece_treated <- s2_w_treated / n1
piece_control <- s2_w_control / n0
print(round(c(s2_w_treated = s2_w_treated, s2_w_control = s2_w_control,
              piece_treated = piece_treated, piece_control = piece_control), 6))

var_cace_transf <- conservative_var(z = z_obs, y = w_obs) / itt_d_hat^2

## The two routes agree exactly (a useful internal check):
print(round(c(var_cace_grad = var_cace_grad,
              var_cace_transf = var_cace_transf,
              equal = isTRUE(all.equal(var_cace_grad, var_cace_transf))), 8))

## SUBTLETY: by construction itt_y_hat - cace_hat * itt_d_hat = 0, so the
## difference in means of the FEASIBLE transformed outcome W_hat is exactly zero.
## We never use it as a point estimate; only its WITHIN-arm variation feeds the
## variance estimate above.
print(round(c(diff_in_means_w_hat = diff_in_means(z = z_obs, y = w_obs)), 12))

se_cace <- sqrt(var_cace_transf)                    # ~ 0.0239
ci_cace <- c(lower = cace_hat - z_crit * se_cace,
             upper = cace_hat + z_crit * se_cace)   # ~ [0.034, 0.128]
print(round(c(cace_hat = cace_hat, se_cace = se_cace, ci_cace), 4))

## The cace_var() helper (defined above, handed to students) reproduces this se.
print(round(c(se_from_helper = sqrt(cace_var(z = z_obs, y = y_obs, d = d_obs))), 4))

## A test of H_0: CACE = 0 (equivalently ITT_Y = 0, the safely testable null).
T_cace <- cace_hat / se_cace
p_two  <- 2 * pnorm(q = abs(T_cace), lower.tail = FALSE)
print(round(c(T_cace = T_cace, p_two_sided = p_two), 4))   # T ~ 3.40, p ~ 0.0007

## CONTRAST: naively dividing the ITT_Y standard error by the ITT_D POINT estimate
## ignores both the ratio structure and the covariance -- it is NOT the CACE se.
se_itt_y    <- sqrt(var_itt_y)
naive_ratio <- se_itt_y / itt_d_hat
print(round(c(se_itt_y = se_itt_y, naive_se_ratio = naive_ratio,
              correct_se_cace = se_cace), 4))

## ============================================================
## 5. A constructed science table matching the study margins
## ============================================================
## To VISUALIZE the joint randomization distribution of (ITT_Y_hat, ITT_D_hat) and
## the sampling distribution of CACE_hat, we build a finite population of N = 2650
## units whose fixed potential outcomes reproduce every observed margin exactly:
##   * 1900 Compliers   : d(0) = 0, d(1) = 1
##   * 750  Never-Takers: d(0) = d(1) = 0  (exclusion restriction: y(1) = y(0))
## Turnout counts are chosen so that mean y(0), mean y(1), ITT_D, and the CACE all
## equal their observed values. This is an ILLUSTRATIVE population, fixed once;
## randomness in the figures comes only from re-randomizing the assignment.
n_comp <- 1900   # = 2650 * (950 / 1325): the share contacted when telephoned
n_nt   <- N - n_comp                                   # 750

## Receipt potential outcomes: only Compliers take the dose, and only if assigned.
d0 <- rep(0, N)
d1 <- c(rep(1, n_comp), rep(0, n_nt))

## Outcome potential outcomes (1 = vote), pairing voters so the two POs are
## positively associated within Compliers (a plausible science table).
##   Compliers   : 466 vote under control, 620 vote under treatment (CACE on the
##                 1900 Compliers = (620 - 466) / 1900 = 0.0810).
##   Never-Takers : 164 vote regardless of assignment (exclusion restriction).
comp_y0 <- c(rep(1, 466), rep(0, n_comp - 466))
comp_y1 <- c(rep(1, 620), rep(0, n_comp - 620))
nt_y    <- c(rep(1, 164), rep(0, n_nt - 164))
y0 <- c(comp_y0, nt_y)
y1 <- c(comp_y1, nt_y)

## Confirm the constructed truth reproduces the observed estimates.
itt_y_true <- mean(y1) - mean(y0)
itt_d_true <- mean(d1) - mean(d0)
cace_true  <- itt_y_true / itt_d_true
print(round(c(itt_y_true = itt_y_true, itt_d_true = itt_d_true,
              cace_true = cace_true), 4))

## ============================================================
## 6. Simulate the randomization distribution (figures)
## ============================================================
## Complete randomization: 1325 of the 2650 units assigned a call. For each draw,
## reveal D and Y from the fixed science table and recompute all three estimates.
set.seed(seed = 12345)
n_sims <- 10^4
base_z <- rep(c(1, 0), times = c(n1, n0))

sim <- replicate(n = n_sims, expr = {
  z <- sample(base_z)
  d <- z * d1 + (1 - z) * d0
  y <- z * y1 + (1 - z) * y0
  itt_y <- diff_in_means(z = z, y = y)
  itt_d <- diff_in_means(z = z, y = d)
  c(itt_y = itt_y, itt_d = itt_d, cace = itt_y / itt_d)
})
sim_df <- as.data.frame(t(sim))

## The estimators are jointly variable and POSITIVELY correlated -- this is the
## covariance the delta method must account for.
print(round(c(sd_itt_y = sd(sim_df$itt_y),
              sd_itt_d = sd(sim_df$itt_d),
              cor_itt_yd = cor(sim_df$itt_y, sim_df$itt_d),
              sd_cace = sd(sim_df$cace)), 4))

## ---- Figure: linearizing the ratio (tangent slices) ----
## The CACE is a NONLINEAR function g(ITT_Y, ITT_D) = ITT_Y / ITT_D of two random
## differences in means. The delta method replaces g by its tangent PLANE at the
## (unknown) truth. We visualize that plane one coordinate at a time: holding the
## other coordinate at its estimate, each slice of the tangent plane is a tangent
## LINE. The numerator slice is exactly linear, so the tangent COINCIDES with g;
## the denominator slice is a hyperbola whose curvature -- and the tangent's
## error -- explodes as the first stage ITT_D nears zero. The expansion point is
## the unknown truth; we draw it at the Adams-Smith estimates as a stand-in.
panel_num    <- "Vary~widehat(ITT)[Y]~~(numerator)"
panel_den    <- "Vary~widehat(ITT)[D]~~(denominator)"
panel_levels <- c(panel_num, panel_den)

slope_den <- -itt_y_hat / itt_d_hat^2   # d/d(ITT_D) of ITT_Y / ITT_D at the truth

## Numerator slice: hold ITT_D at its estimate, vary ITT_Y. Here g is linear, so
## the tangent line equals g exactly.
ty_grid <- seq(from = 0, to = 0.13, length.out = 200)
num_df  <- data.frame(
  panel   = panel_num,
  x       = ty_grid,
  ratio   = ty_grid / itt_d_hat,
  tangent = cace_hat + (ty_grid - itt_y_hat) / itt_d_hat
)

## Denominator slice: hold ITT_Y at its estimate, vary ITT_D. Here g is a
## hyperbola; the tangent touches at the expansion point and drifts away -- badly
## as ITT_D -> 0, where the ratio curves sharply.
td_grid <- seq(from = 0.12, to = 1.30, length.out = 300)
den_df  <- data.frame(
  panel   = panel_den,
  x       = td_grid,
  ratio   = itt_y_hat / td_grid,
  tangent = cace_hat + slope_den * (td_grid - itt_d_hat)
)

## Long format: one row per (panel, x, curve) for a single shared legend.
slice_long <- rbind(
  data.frame(num_df[c("panel", "x")], value = num_df$ratio,   curve = "ratio g"),
  data.frame(num_df[c("panel", "x")], value = num_df$tangent, curve = "tangent"),
  data.frame(den_df[c("panel", "x")], value = den_df$ratio,   curve = "ratio g"),
  data.frame(den_df[c("panel", "x")], value = den_df$tangent, curve = "tangent")
)
slice_long$panel <- factor(slice_long$panel, levels = panel_levels)

## Expansion point (the truth, drawn at the Adams-Smith estimates) in each panel.
point_df <- data.frame(
  panel = factor(panel_levels, levels = panel_levels),
  x     = c(itt_y_hat, itt_d_hat),
  value = c(cace_hat, cace_hat)
)

## Shaded band = where the estimator is likely to land (so the tangent stays
## accurate): roughly +/- 2 simulated SDs of each ITT estimator around the truth.
band_df <- data.frame(
  panel = factor(panel_levels, levels = panel_levels),
  xmin  = c(itt_y_hat - 2 * sd(sim_df$itt_y), itt_d_hat - 2 * sd(sim_df$itt_d)),
  xmax  = c(itt_y_hat + 2 * sd(sim_df$itt_y), itt_d_hat + 2 * sd(sim_df$itt_d))
)

## One short annotation per panel, naming the lesson of each slice.
anno_df <- data.frame(
  panel = factor(panel_levels, levels = panel_levels),
  x     = c(0.005, 0.30),
  value = c(0.155, 0.40),
  label = c("exactly linear:\ntangent = ratio",
            "curvature grows\nas ITT_D -> 0")
)

ratio_linearization <- ggplot() +
  geom_rect(data = band_df,
            mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey85", alpha = 0.6) +
  geom_line(data = slice_long,
            mapping = aes(x = x, y = value, colour = curve, linetype = curve),
            linewidth = 1) +
  geom_point(data = point_df, mapping = aes(x = x, y = value),
             size = 2.6, colour = "black") +
  geom_text(data = anno_df,
            mapping = aes(x = x, y = value, label = label),
            hjust = 0, vjust = 1, size = 3.3, colour = "grey25",
            lineheight = 0.95) +
  facet_wrap(facets = ~ panel, scales = "free", labeller = label_parsed) +
  scale_colour_manual(name = NULL,
                      values = c("ratio g" = viridis(3)[1],
                                 "tangent" = viridis(3)[2]),
                      labels = c("ratio  g = ITT_Y / ITT_D",
                                 "tangent (delta method)")) +
  scale_linetype_manual(name = NULL,
                        values = c("ratio g" = "solid", "tangent" = "21"),
                        labels = c("ratio  g = ITT_Y / ITT_D",
                                   "tangent (delta method)")) +
  labs(x = "coordinate varied (the other held at its estimate)",
       y = expression("ratio  " * ITT[Y] / ITT[D])) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.spacing = unit(1.4, "lines"))

##+ eval=saveplots_
ggsave(plot = ratio_linearization,
       file = "figures/ratio_linearization.pdf",
       width = 8, height = 4.2, units = "in", dpi = 300)

##+ eval=TRUE
## ---- Figure: the right-hand (denominator) slice only, for the slides ----
## The trimmed deck shows just this panel: holding the numerator ITT_Y at its
## (true) value, g = ITT_Y / ITT_D is a hyperbola in the first stage ITT_D. The
## tangent line at the true ITT_D is the linear approximation -- accurate in the
## shaded likely region, degrading as ITT_D -> 0.
right_df <- data.frame(
  x     = c(td_grid, td_grid),
  value = c(itt_y_hat / td_grid,
            cace_hat + slope_den * (td_grid - itt_d_hat)),
  curve = rep(c("ratio g", "tangent"), each = length(td_grid))
)
band_right <- data.frame(
  xmin = itt_d_hat - 2 * sd(sim_df$itt_d),
  xmax = itt_d_hat + 2 * sd(sim_df$itt_d)
)
ratio_linearization_right <- ggplot() +
  geom_rect(data = band_right,
            mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey85", alpha = 0.6) +
  geom_line(data = right_df,
            mapping = aes(x = x, y = value, colour = curve, linetype = curve),
            linewidth = 1.1) +
  geom_point(mapping = aes(x = itt_d_hat, y = cace_hat),
             size = 3, colour = "black") +
  annotate(geom = "text", x = itt_d_hat, y = cace_hat,
           label = "expand at true ITT_D", hjust = -0.08, vjust = -1,
           size = 4, colour = "grey25") +
  annotate(geom = "text", x = 0.27, y = 0.40,
           label = "curvature grows\nas ITT_D -> 0", hjust = 0, vjust = 1,
           size = 4, colour = "grey25", lineheight = 0.95) +
  scale_colour_manual(name = NULL,
                      values = c("ratio g" = viridis(3)[1],
                                 "tangent" = viridis(3)[2]),
                      labels = c("ratio  g = ITT_Y / ITT_D",
                                 "tangent (linear approx.)")) +
  scale_linetype_manual(name = NULL,
                        values = c("ratio g" = "solid", "tangent" = "21"),
                        labels = c("ratio  g = ITT_Y / ITT_D",
                                   "tangent (linear approx.)")) +
  labs(x = expression(ITT[D] ~ ~ "(first stage; " * ITT[Y] * " held at its true value)"),
       y = expression("ratio  " * ITT[Y] / ITT[D])) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

##+ eval=saveplots_
ggsave(plot = ratio_linearization_right,
       file = "figures/ratio_linearization_right.pdf",
       width = 6.5, height = 4.6, units = "in", dpi = 300)

##+ eval=TRUE
## ---- Figure: the multivariate linearization in 3D (surface + tangent plane) --
## The faithful multivariate picture: g(ITT_Y, ITT_D) = ITT_Y / ITT_D is a curved
## SURFACE, and the delta method replaces it by its tangent PLANE at the (unknown)
## truth. The plane grazes the surface at the expansion point and pulls away where
## the surface curves -- fastest toward small ITT_D. We draw it at the Adams-Smith
## estimates as a stand-in for the truth. A static 3D view is hard to read
## precisely, which is exactly why the companion figure slices it in 2D.
ty_grid_3d    <- seq(from = 0.00, to = 0.12, length.out = 80)
td_grid_3d    <- seq(from = 0.40, to = 1.05, length.out = 80)
ratio_surface <- outer(ty_grid_3d, td_grid_3d, function(y, d) y / d)

## A coarser grid renders the tangent plane as a clean wireframe.
ty_coarse     <- seq(from = 0.00, to = 0.12, length.out = 14)
td_coarse     <- seq(from = 0.40, to = 1.05, length.out = 14)
tangent_plane <- outer(ty_coarse, td_coarse, function(y, d)
  cace_hat + (y - itt_y_hat) / itt_d_hat -
    itt_y_hat / itt_d_hat^2 * (d - itt_d_hat))

##+ eval=saveplots_
pdf(file = "figures/ratio_linearization_3d.pdf", width = 7, height = 5.6)
par(mar = c(1.5, 1.5, 1, 1))
persp3D(x = ty_grid_3d, y = td_grid_3d, z = ratio_surface,
        theta = -130, phi = 24, expand = 0.7,
        col = viridis(120), colkey = FALSE, border = NA, shade = 0.25,
        xlab = "ITT_Y", ylab = "ITT_D", zlab = "ITT_Y / ITT_D",
        bty = "b2", ticktype = "detailed", cex.axis = 0.65, cex.lab = 0.9)
## Tangent plane as a translucent orange wireframe -- clearly flat.
persp3D(x = ty_coarse, y = td_coarse, z = tangent_plane, add = TRUE,
        facets = NA, border = "darkorange", lwd = 0.9, colkey = FALSE)
## Dotted drop line marking the expansion point (the truth).
segments3D(x0 = itt_y_hat, y0 = itt_d_hat, z0 = 0,
           x1 = itt_y_hat, y1 = itt_d_hat, z1 = cace_hat,
           add = TRUE, col = "grey25", lty = 3, lwd = 1.2, colkey = FALSE)
scatter3D(x = itt_y_hat, y = itt_d_hat, z = cace_hat, add = TRUE,
          col = "red", pch = 19, cex = 1.7, colkey = FALSE)
dev.off()

##+ eval=TRUE
## ---- Figure: joint randomization distribution of the two ITT estimators ----
gotv_itt_joint_dist <- ggplot(data = sim_df,
                              mapping = aes(x = itt_d, y = itt_y)) +
  geom_point(alpha = 0.12, colour = viridis(3)[1], size = 0.7) +
  geom_vline(xintercept = itt_d_true, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = itt_y_true, linetype = "dashed", colour = "grey40") +
  annotate(geom = "text", x = itt_d_true, y = max(sim_df$itt_y),
           label = paste0("r = ", round(cor(sim_df$itt_y, sim_df$itt_d), 2)),
           hjust = -0.1, vjust = 1, size = 4.2, colour = "grey20") +
  labs(x = expression(widehat(ITT)[D] ~ "(first stage)"),
       y = expression(widehat(ITT)[Y] ~ "(reduced form)")) +
  theme_minimal(base_size = 14)

##+ eval=saveplots_
ggsave(plot = gotv_itt_joint_dist,
       file = "figures/gotv_itt_joint_dist.pdf",
       width = 6, height = 4.2, units = "in", dpi = 300)

##+ eval=TRUE
## ---- Figure: the CACE estimator is approximately Normal (delta method) ----
## Overlay the Normal centered at the true CACE with the simulated (true) sampling
## SD: the ratio's distribution is well approximated by a Normal, which is what
## licenses the delta-method standard error and the Wald confidence interval.
norm_grid <- seq(from = min(sim_df$cace), to = max(sim_df$cace),
                 length.out = 400)
norm_df <- data.frame(cace = norm_grid,
                      dens = dnorm(norm_grid, mean = cace_true,
                                   sd = sd(sim_df$cace)))
gotv_cace_sampling_dist <- ggplot(data = sim_df, mapping = aes(x = cace)) +
  geom_histogram(mapping = aes(y = after_stat(density)),
                 bins = 40, fill = "grey75", colour = "white") +
  geom_line(data = norm_df, mapping = aes(x = cace, y = dens),
            colour = viridis(3)[1], linewidth = 0.9) +
  geom_vline(xintercept = cace_true, linetype = "solid", linewidth = 0.7) +
  geom_vline(xintercept = ci_cace, linetype = "dashed",
             colour = viridis(3)[2], linewidth = 0.7) +
  annotate(geom = "text", x = cace_true, y = 0,
           label = paste0("CACE = ", round(cace_true, 3)),
           vjust = -0.6, hjust = -0.05, size = 4, colour = "grey20") +
  labs(x = expression(widehat(CACE) ~ "over re-randomizations"),
       y = "Density") +
  theme_minimal(base_size = 14)

##+ eval=saveplots_
ggsave(plot = gotv_cace_sampling_dist,
       file = "figures/gotv_cace_sampling_dist.pdf",
       width = 6.5, height = 4.2, units = "in", dpi = 300)

##+ eval=TRUE
## ============================================================
## 6b. Two representations of the numerator are identical
## ============================================================
## The linear approximation's numerator, tau_hat_W, can be written two ways:
##   (1) a difference of two Difference-in-Means: tau_hat(Z, Y) - c * tau_hat(Z, D)
##   (2) one Difference-in-Means on the transformed outcome W = Y - c * D.
## They are equal by LINEARITY of the Difference-in-Means, for ANY constant c
## (here the value we linearize at, i.e. CACE). No access to the truth is needed.
## This block is fully self-contained: it uses fresh simulated data.
set.seed(seed = 1)
n <- 500
z <- rbinom(n = n, size = 1, prob = 0.5)   # random assignment
y <- rbinom(n = n, size = 1, prob = 0.4)   # observed outcome
d <- rbinom(n = n, size = 1, prob = 0.7)   # observed receipt

cace <- 0.08                                # the constant we linearize at
## numerator of the linear approximation, computed two ways:
a <- diff_in_means(z = z, y = y) - cace * diff_in_means(z = z, y = d)  # (a) two Diff-in-Means
b <- diff_in_means(z = z, y = y - cace * d)                            # (b) one DiM on W
print(round(c(two_diff_in_means = a,
              one_on_transformed = b,
              equal = isTRUE(all.equal(a, b))), 6))

## ============================================================
## 7. Key numbers echoed on the slides (single tidy printout)
## ============================================================
print(round(c(
  itt_y_hat       = itt_y_hat,
  itt_d_hat       = itt_d_hat,
  cace_hat        = cace_hat,
  per_protocol    = per_protocol,
  se_cace         = se_cace,
  ci_lower        = unname(ci_cace["lower"]),
  ci_upper        = unname(ci_cace["upper"]),
  p_two_sided     = p_two,
  naive_se_ratio  = naive_ratio
), 4))

## ============================================================
## 8. Attrition: conditioning on the reporter split (enumeration)
## ============================================================
## A small completely randomized experiment to make the conditioning concrete:
## N = 6 units, of which 4 are Always-Reporters (AR) and 2 are Never-Reporters;
## n1 = 3 are treated. Enumerate all C(6, 3) = 20 treated sets, group them by the
## number of treated AR (N_1^R = m1), and confirm that, conditional on m1, each AR
## unit is treated with probability m1 / N^AR -- i.e. the treated reporters are a
## simple random sample of the AR, so among the reporters it is a CRE.
is_ar        <- c(rep(TRUE, 4), rep(FALSE, 2))   # units 1-4 are AR, 5-6 NR
n_ar         <- sum(is_ar)                       # 4
treated_sets <- combn(x = 6, m = 3)              # all 20 treated sets (columns)
n1_R <- apply(X = treated_sets, MARGIN = 2,
              FUN = function(tr) sum(is_ar[tr]))  # treated-AR count per assignment
print(table(n1_R))                               # m1 = 1: 4,  2: 12,  3: 4

## Within each split m1, the treatment rate of a fixed AR unit (unit 1) = m1 / 4:
for (m1 in sort(unique(n1_R))) {
  cols <- treated_sets[, n1_R == m1, drop = FALSE]
  rate <- mean(apply(X = cols, MARGIN = 2, FUN = function(tr) 1 %in% tr))
  cat("m1 =", m1, ": P(AR unit treated | N_1^R = m1) =", round(rate, 3),
      "= m1/N^AR =", m1 / n_ar, "\n")
}
