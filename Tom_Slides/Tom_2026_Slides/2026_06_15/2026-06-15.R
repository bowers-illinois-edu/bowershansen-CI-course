###############################################################################
## Fisher's "Lady Tasting Tea": randomization calculation and figures
##
## What this script does:
##   - computes the randomization distribution of Fisher's tea-tasting
##     experiment under the null hypothesis of no discrimination, and
##   - saves two figures into an "images/" folder.
##
## How to run it:
##   Put this file in its own folder, create an "images/" subfolder next to it,
##   then from that folder run, open file in RStudio and run it top to bottom.
##
## A note on style: argument names are written out (e.g. combn(x = 1:8, m = 4))
## whenever the name either teaches what an argument does or guards against
## silently swapping two arguments ("transposing" them, e.g. mixing up x and m).
## Names are dropped for an obvious single input, an operator, or a "..." item.
###############################################################################

## This script uses the ggplot2 package for its figures. If you do not already
## have it installed, run the next line once (delete the leading "#"), then put
## the "#" back so it does not re-install every time you run the script:
# install.packages("ggplot2")
library(ggplot2)

## ===========================================================================
## 1. The observed experiment  (this is the code chunk shown on the slide)
## ===========================================================================

# Observed assignment: 1 = milk-first, 0 = tea-first
z_obs <- c(1, 1, 1, 1, 0, 0, 0, 0)
# Taster's labels:     1 = says "milk-first", 0 = says "tea-first"
y_obs <- c(1, 1, 1, 1, 0, 0, 0, 0)

# Test statistic t = z^T y: number of milk-first cups (z = 1) that the
# taster also labels milk-first (y = 1)
t_obs <- sum(z_obs * y_obs)              # = 4 (all four correct)

## ===========================================================================
## 2. Two ways to list assignments
## ===========================================================================

# (a) ALL 2^8 = 256 assignments: each cup independently milk- or tea-first.
#     This is the Bernoulli assignment space (NOT Fisher's design).
all_256 <- expand.grid(rep(x = list(0:1), times = 8))
nrow(all_256)                            # = 256

# (b) Fisher's design: exactly 4 of the 8 cups are milk-first.
#     choose(8, 4) = 70 possible assignments; the other 4 are tea-first.
milk_first_sets <- combn(x = 1:8, m = 4)   # columns = the 70 choices
all_z <- apply(X = milk_first_sets, MARGIN = 2, FUN = function(cups) {
  z <- rep(x = 0, times = 8)             # start all tea-first
  z[cups] <- 1                           # set chosen cups milk-first
  z
})
ncol(all_z)                              # = 70

## ===========================================================================
## 3. Randomization distribution under NO DISCRIMINATION
## ===========================================================================

# Under no discrimination the labels y_obs are FIXED; only z varies.
# Recompute t = z^T y for each of the 70 assignments.
all_t <- apply(X = all_z, MARGIN = 2, FUN = function(z) sum(z * y_obs))

# Chance, under no discrimination, of a score as high as observed = 1/70
mean(all_t >= t_obs)                     # = 0.01428571

## ===========================================================================
## 4. Figure 1: the no-discrimination randomization distribution
##    (used on the "Is the observed result surprising?" slide)
## ===========================================================================

# Turn the 70 values of t into a probability for each possible value
null_df        <- as.data.frame(table(all_t) / length(all_t))
names(null_df) <- c("t", "prob")
null_df$t      <- as.integer(as.character(null_df$t))
null_df$prob   <- as.numeric(null_df$prob)

fig_null <- ggplot(data = null_df, mapping = aes(x = t, y = prob)) +
  geom_col(fill = "grey55", colour = "black", width = 0.85) +
  geom_vline(xintercept = t_obs, linetype = "dashed",
             colour = "magenta", linewidth = 1) +
  annotate(geom = "text", x = t_obs - 0.1, y = 0.5, label = "observed = 4",
           colour = "magenta", hjust = 1, size = 4.5) +
  scale_x_continuous(breaks = 0:4) +
  labs(x = expression(t == bold(z)^T * bold(y) ~ "(cups correctly identified)"),
       y = "Probability under no discrimination") +
  theme_bw(base_size = 15)

ggsave(filename = "images/fisher_null_dist.png", plot = fig_null,
       width = 8, height = 4, units = "in", dpi = 300)

## ===========================================================================
## 5. Figure 2: same design, two models (no vs perfect discrimination)
##    (used on the "Same design, two models" slide)
## ===========================================================================

# Under PERFECT discrimination the taster is always right: her labels track z,
# so t = z^T z = 4 for every assignment.
all_t_perfect <- apply(X = all_z, MARGIN = 2, FUN = function(z) sum(z * z))

perfect_df        <- as.data.frame(table(all_t_perfect) / length(all_t_perfect))
names(perfect_df) <- c("t", "prob")
perfect_df$t      <- as.integer(as.character(perfect_df$t))
perfect_df$prob   <- as.numeric(perfect_df$prob)

# Stack the two distributions, label each by its model, and plot side by side
two_models_df <- rbind(
  data.frame(null_df,    model = "No discrimination"),
  data.frame(perfect_df, model = "Perfect discrimination")
)

fig_two <- ggplot(data = two_models_df, mapping = aes(x = t, y = prob)) +
  geom_col(fill = "grey55", colour = "black", width = 0.85) +
  geom_vline(xintercept = t_obs, linetype = "dashed",
             colour = "magenta", linewidth = 1) +
  facet_wrap(facets = ~ model) +
  scale_x_continuous(breaks = 0:4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = expression(t == bold(z)^T * bold(y)), y = "Probability") +
  theme_bw(base_size = 15)

ggsave(filename = "images/fisher_two_models.png", plot = fig_two,
       width = 8, height = 3.6, units = "in", dpi = 300)
