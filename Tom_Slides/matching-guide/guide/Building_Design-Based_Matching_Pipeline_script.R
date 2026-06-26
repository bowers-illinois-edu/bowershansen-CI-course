# ---- chunk ----
# Define base URL for the Matching Guide GitHub repo
base_url <- "https://raw.githubusercontent.com/tl2624/matching-guide/main"

# Load the cleaned dataset
data <- readRDS(
  url(
    paste0(
      base_url, "/data/peace_pre_match.rds"
    )
  )
)

# ---- chunk ----
# Define character vector of the 9 covariate names in the dataset
covs <- c("lwdeaths", "lwdurat", "ethfrac", "pop", "lmtnest", "milper",
          "bwgdp", "bwplty2", "region")

# ---- chunk ----
# Install "dplyr" package (only run if not already installed)
# install.packages("dplyr")

# Load package for data manipulation (mutate, group_by, summarize, etc.)
library(dplyr)

# Convert categorical variable "region" into 0/1 dummy indicators
data <- data |> # Pipe (|>) to pass left-hand result into next function call
  mutate(
    # Use mutate() to create new variables by transforming existing columns
    # ifelse() returns one value if condition is TRUE and another if FALSE
    eeurop   = ifelse(test = region == "eeurop",   yes = 1, no = 0),
    lamerica = ifelse(test = region == "lamerica", yes = 1, no = 0),
    asia     = ifelse(test = region == "asia",     yes = 1, no = 0),
    ssafrica = ifelse(test = region == "ssafrica", yes = 1, no = 0),
    nafrme   = ifelse(test = region == "nafrme",   yes = 1, no = 0)
  )

# Covariate names with "region" replaced by dummy indicators
expanded_covs <- c(
  # setdiff() returns elements in 'x' that are not in 'y'
  setdiff(
    x = covs,
    y = "region" # Drops "region" from the covariate list
  ),
  "eeurop", "lamerica", "asia", "ssafrica", "nafrme"
)

# ---- chunk ----
# Extract covariates for Liberia and Guinea-Bissau (cname = country name)
liberia <- data[data$cname == "Liberia", expanded_covs]

guinea_bissau <- data[data$cname == "Guinea-Bissau", expanded_covs]

# Compute Euclidean distance between the two countries on these covariates
sqrt(
  sum(
    (liberia - guinea_bissau)^2
  )
)  # Display the Euclidean distance

# ---- chunk ----
# Create a formula: UN (treatment indicator) ~ covariates
# Note: we keep "region" in covs as a factor
cov_fmla <- reformulate(
  termlabels = covs,
  response = "UN"
)

# Install optmatch if not already installed
# install.packages("optmatch")

# Load optmatch, which provides the match_on() function
library(optmatch)

# Euclidean distance matrix: treated (UN = 1) vs control (UN = 0)
dist_mat_euc <- match_on(
  x = cov_fmla,                 # Formula for covariates
  data = data,                  # Dataset used
  standardization.scale = NULL, # No rescaling of covariates
  method = "euclidean"          # Use Euclidean distance
)

# Add country names (cname) as row/column labels for clarity
dimnames(dist_mat_euc) <- list(
  data$cname[data$UN == 1], data$cname[data$UN == 0]
)

# Display submatrix of distances:
round(        # Round distances for display
  # treated units in rows 11-15 vs. control units in columns 27-30
  x = dist_mat_euc[11:15, 27:30],
  digits = 2  # Number of decimal places to round
)

# ---- chunk ----
# Mahalanobis distance matrix: treated (UN = 1) vs control (UN = 0)
dist_mat_mah <- match_on(
  x = cov_fmla,
  data = data,
  standardization.scale = NULL,
  method = "mahalanobis" # Use Mahalanobis distance
)

# ---- chunk ----
# Formula for UN ~ covariates (excluding "region")
psm_cov_fmla <- reformulate(
  termlabels = setdiff(
    x = covs,
    y = "region"
  ),
  response = "UN"
)

# Fit logistic regression for propensity score model
psm <- glm(
  formula = psm_cov_fmla,            # Treatment ~ covariates
  family = binomial(link = "logit"), # Logistic regression (logit link)
  data = data
)

# ---- chunk ----
# Extract logit propensity scores (linear predictors from fitted model)
lin_cov_inds <- psm$linear.predictors  # = model.matrix(psm) %*% coef(psm)

# ---- chunk ----
# Extract estimated propensity scores (predicted probabilities of UN = 1)
p_scores <- psm$fitted.values

# Check that lin_cov_inds equals log odds (propensity scores on logit scale)
all.equal(
  lin_cov_inds, log(p_scores / (1 - p_scores))
)

# Also check that p_scores equals logistic(lin_cov_inds)
all.equal(
  p_scores, 1 / (1 + exp(-lin_cov_inds))
)

# ---- chunk ----
# Install "ggplot2" (only run if not already installed)
# Install.packages("ggplot2")

# Load ggplot2 package for visualization
library(ggplot2)

# Boxplot of estimated linear covariate index by treatment status
ggplot(data = data,  # Dataset
       mapping = aes(x = as.factor(UN),    # Treatment indicator (0/1 as factor)
                     y = lin_cov_inds)) +  # Linear covariate index (from logistic regression)
  geom_boxplot() +                         # Draw boxplots
  xlab(label = "UN intervention") +        # X-axis label for treatment
  ylab(label = "Estimated linear covariate index") + # Y-axis label for lin cov index
  theme_bw() +                             # Apply black-and-white theme
  coord_flip()                             # Flip axes for readability

# ---- chunk ----
# Create distance structure: 0 if units are in same region, Inf otherwise
em_region <- exactMatch(
  x = UN ~ region,
  data = data
)

# ---- chunk ----
# Euclidean distance on Polity score (bwplty2) with caliper = 2
# Pairs differing by > 2 are set to Inf
euc_dist_polity_cal_2 <- match_on(
  x = UN ~ bwplty2,
  caliper = 2,  # Set caliper
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

# ---- chunk ----
# Overall distance matrix by element-wise addition of two distance matrices
overall_dist_mat <- em_region + euc_dist_polity_cal_2

# ---- chunk ----
# Apply a caliper of width 3 to the polity Euclidean distance matrix
euc_dist_polity_cal_3 <- match_on(
  x = UN ~ bwplty2,
  caliper = 3,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

# Combine regional exact match distance with polity distance
em_region + euc_dist_polity_cal_3

# ---- chunk ----
# Full matching using overall distance matrix
fullmatch(
  x = overall_dist_mat,
  min.controls = 0.5,   # At least 0.5 controls per treated unit
  # (i.e., no more than 2 treated per control)
  max.controls = 2,     # At most 2 controls per treated unit
  omit.fraction = NULL, # Governs fraction of units discarded
  mean.controls = NULL, # Governs average controls per treated unit
  # Only one of omit.fraction or mean.controls can be non-NULL
  data = data
)

# ---- chunk ----
# Add logistic regression linear predictors to dataset
data$logit_p_score <- lin_cov_inds

# Population standard deviation of logit_p_score (divides by n, not n - 1)
pop_sd_logit <- sqrt(
  mean(
    (data$logit_p_score - mean(data$logit_p_score))^2
  )
)

# Distance matrix for logit of estimated propensity scores
ps_mat <- match_on(
  x = UN ~ logit_p_score,
  caliper = 0.5 * pop_sd_logit,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

# ---- chunk ----
# Rank-based Mahalanobis distance on covariates
# Covs defined earlier as covariate names; here, drop "region"
rank_mah_mat <- match_on(
  x      = reformulate(
    termlabels = setdiff(
      x = covs,
      y = "region"),
    response   = "UN"),
  data   = data,
  standardization.scale = NULL,
  method = "rank_mahalanobis" # Use rank-based Mahalanobis distance
)

# ---- chunk ----
# Compute Euclidean distance matrix for ethnic fractionalization
eth_mat <- match_on(
  x = UN ~ ethfrac,
  caliper = 35,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

# Compute Euclidean distance matrix for logged GDP per capita
bwgdp_mat <- match_on(
  x = UN ~ bwgdp,
  caliper = 2,
  data = data,
  standardization.scale = NULL,
  method = "euclidean"
)

# ---- chunk ----
# Full matching: 
# PS + rank Mahalanobis + Euclidean (ethfrac, bwgdp) + region exact
fm <- fullmatch(
  # x specifies the distance structure:
  # match_on() formula, distance matrix, or sum of match_on() distances
  x            = ps_mat + rank_mah_mat + eth_mat + bwgdp_mat + em_region,
  data         = data,
  max.controls = 4 # <= 4 controls per treated (min.controls = 0 by default)
)

# ---- chunk ----
# Effective sample size of matched sets
effectiveSampleSize(
  fm
)

# ---- chunk ----
# Summarize matched sets and report effective sample size
summary(
  fm
)

# ---- chunk ----
# Add matched set ID to data for each unit
data$fm <- fm

# Look at one matched set ("ssafrica.3") for illustration
data |>
  filter(
    fm == "ssafrica.3" # Keep only units in set "ssafrica.3"
  ) |>
  # Display selected variables
  select(
    cname, UN, region, logit_p_score, ethfrac, bwgdp, bwplty2
  )

# ---- chunk ----
# Install "RItools" package (only run if not already installed)
# install.packages("RItools")

# Load RItools package for balance diagnostics (balanceTest)
library(RItools)
# Covariate balance test
cov_bal <- balanceTest(
  # Formula: treatment ~ covariates
  # update(): keep the original formula (. ~ .)
  # and add stratification by matched set, + strata(fm)
  fmla = update(
    cov_fmla, . ~ . + strata(fm)
  ),
  data = data,
  p.adjust.method = "none" # Method of p-value adjustment (none here)
)

# ---- chunk ----
# Install packages (only run these lines if not already installed)
# install.packages("tibble")
# install.packages("kableExtra")

# Load tibble for cleaner data frame printing/handling
library(tibble)

# Load kableExtra for formatting tables for LaTeX/HTML output
library(kableExtra)

# Extract balanceTest 3-D results:
# [vars, stats (Control,Treatment,std.diff,p), strata (fm, --)]
arr  <- cov_bal$results
vars <- dimnames(arr)$vars

# Get readable labels from data (haven-style), else fall back to variable name
label_from_data <- function(v) {
  if (v %in% names(data)) {
    lb <- attr(data[[v]], "label", exact = TRUE)
    if (!is.null(lb) && nzchar(lb)) return(lb)
  }
  v
}

# Map region dummy names → readable labels
region_map <- c(
  regioneeurop   = "Eastern Europe",
  regionlamerica = "Latin America",
  regionnafrme   = "North Africa & Middle East",
  regionssafrica = "Sub-Saharan Africa",
  regionasia     = "Asia"
)

# Final covariate labels
# Use region_map if present; else dataset label; else raw name
cov_labels <- sapply(
  X = vars,
  FUN = function(v) {
    ifelse(
      test = v %in% names(region_map),
      yes  = region_map[[v]],
      no   = label_from_data(v)
    )
  },
  USE.NAMES = FALSE
)

# Helper: slice a 2-D matrix (vars × stats) for one stratum
slice_mat <- function(a, stratum) {
  a[, c("Control", "Treatment", "std.diff", "p"),
    stratum,
    drop = FALSE][,,1, drop = TRUE]
}

# Before/after matrices
# In balanceTest output, "--" is the overall (unstratified) result
before <- slice_mat(arr, "--")
after  <- slice_mat(arr, "fm")

# Build tibble with distinct internal names
cov_tab <- tibble(
  Covariate      = cov_labels,
  Control_before = round(x = before[, "Control"],   digits = 2),
  Treated_before = round(x = before[, "Treatment"], digits = 2),
  StdDiff_before = before[, "std.diff"],
  Control_after  = round(x = after[,  "Control"],   digits = 2),
  Treated_after  = round(x = after[,  "Treatment"], digits = 2),
  StdDiff_after  = after[,  "std.diff"],
  p_before       = before[, "p"],  # keep for star annotation
  p_after        = after[,  "p"]
)

# Format standardized differences, adding * when p <= 0.05
fmt_sd <- function(x, p) {
  ifelse(
    test = !is.na(p) & p <= 0.05,
    yes  = sprintf("%.2f*", x),
    no   = sprintf("%.2f",  x)
  )
}

# Apply formatting; drop helper p-values
cov_tab <- cov_tab |>
  mutate(
    StdDiff_before = fmt_sd(StdDiff_before, p_before),
    StdDiff_after  = fmt_sd(StdDiff_after,  p_after)
  ) |>
  select(-p_before, -p_after)

# Print LaTeX table:
# - grouped headers: Before matching / After matching
# - printed column names omit "(Before)/(After)"
# Build the LaTeX table *without* a caption
tab_tex <- kbl(
  cov_tab,
  booktabs  = TRUE,
  align     = c("l","c","c","c","c","c","c"),
  col.names = c("Covariate", "Control mean", "Treated mean", "Std. diff",
                "Control mean", "Treated mean", "Std. diff"),
  # no caption here!
  linesep   = ""
) |>
  add_header_above(
    c(" " = 1, "Before matching" = 3, "After matching" = 3),
    bold = TRUE
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down"),
                full_width    = FALSE) |>
  as.character()

# Insert caption + label *before* \end{table} so it appears below the tabular
tab_tex <- sub(
  "\\\\end\\{table\\}",
  paste0(
    "\\\\captionof{table}{Adjusted covariate means for treated and control groups ",
    "and standardized treated--control differences, before and after matching. ",
    "Asterisks denote two-tailed $p$-values less than or equal to 0.05.}\n",
    "\\\\label{tab: balance}\n",
    "\\\\end{table}"
  ),
  tab_tex
)

cat(tab_tex)

# ---- chunk ----
# Extract overall chi-square balance test by matched set (fm)
cov_bal$overall["fm", ]

# ---- chunk ----
# ---- define matched set and subset data ----
set_id <- "ssafrica.3"  # Matched set name
sdat <- data |> filter(fm == set_id) |> select(cname, UN)
n <- nrow(sdat)  # Number of total units
m <- sum(sdat$UN)  # Number of units treated

# ---- enumerate all possible assignments ----
# Install "randomizr" (only run if not already installed)
# Install.packages("randomizr")
# Load randomizr for generating random assignments
library(randomizr)
Z <- obtain_permutation_matrix(declaration = declare_ra(N = n, m = m))
# Label assignment columns
colnames(Z) <- paste0("Assignment ", seq_len(ncol(Z)))  

# ---- build LaTeX table manually (no unwanted vertical lines) ----
assign_cols <- ncol(Z)
# One vline only after first col
preamble <- paste0("l|", paste(rep("c", assign_cols), collapse = ""))
header <- paste(c("", colnames(Z)), collapse = " & ")  # Blank header for unit

# ---- create one LaTeX row per unit ----
row_lines <- vapply(
  seq_len(n),
  function(i) {
    cells <- as.integer(Z[i, ])
    paste(c(sdat$cname[i], cells), collapse = " & ")
  },
  character(1)
)

# ---- wrap table in center environment ----
tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  paste0("\\begin{tabular}{", preamble, "}"),
  "\\hline",
  header, " \\\\",
  "\\hline",
  paste0(row_lines, " \\\\"),
  "\\hline",
  "\\end{tabular}",
  "\\caption{All possible treatment assignments within matched set \\texttt{ssafrica.3}, holding fixed the observed number of treated units in that set.}",
  "\\label{tab: ssafrica.3 assignments}",
  "\\end{table}"
)

# ---- print final LaTeX code ----
cat(paste(tex, collapse = "\n"))

# ---- chunk ----
# Keep only rows assigned to a matched set (drop NA in fm)
data_matched <- filter(
  .data = data, !is.na(fm)
)

# Null hypothesis value
tau_h <- 0

# Reconstruct outcomes under sharp null (tau_h = 0)
data_matched <- data_matched |>
  mutate(
    ldur_tilde = ldur - tau_h * UN
  )

# ---- chunk ----
# Load hm_stat_rescale() from GitHub repo
# base_url (defined earlier) points to repo URL
source(
  paste0(
    base_url, "/R/hm_stat_rescale.R"
  )
)

# Apply rescaling: adds .hm_scaled and returns matched dataset
data_matched <- hm_stat_rescale(
  data = data_matched,
  outcome = ldur_tilde, # Set outcome to be rescaled within matched sets
  treat = UN,           # Set name of treatment indicator variable
  strata = fm           # Set name of matched strata (block) variable
)

# Observed HM-weighted diff-in-means statistic
obs_stat <- sum(
  data_matched$ldur_tilde_hm_scaled[data_matched$UN == 1]
)

# ---- chunk ----
# Harmonic-mean-weighted difference in means (weights computed within sets)
dim_hm <- data_matched |>
  group_by(
    fm
  ) |>
  summarize(
    n_treated = sum(UN == 1),
    n_control = sum(UN == 0),
    # Within-set Difference-in-Means
    dim_set   = mean(ldur_tilde[UN == 1]) - mean(ldur_tilde[UN == 0]),
    # Harmonic-mean weight (set's contribution to effective sample size)
    w_hm      = 2 * n_treated * n_control / (n_treated + n_control),
    .groups   = "drop" # Drop grouping after summarise
  ) |>
  summarize(
    # Harmonic-mean-weighted average of within-set Differences-in-Means
    dim_hm = sum(
      w_hm * dim_set) / sum(w_hm)
  ) |>
  pull( # Pull column out of data frame
    dim_hm
  )

# Approaches coincides with the sum statistic
all.equal(
  dim_hm, obs_stat
)

# ---- chunk ----
# FE coefficient on UN (matched-set FE)
fe_fit <- lm(
  formula = ldur_tilde ~ UN + fm,
  data = data_matched
)

# Drop name so comparison is purely numeric
treat_coef_fe_fit <- unname(
  coef(fe_fit)["UN"]
)

# Approaches coincides with the sum statistic
all.equal(
  treat_coef_fe_fit, obs_stat
)

# ---- chunk ----
# Install "PSweight" package (only run if not already installed)
# install.packages("PSweight")
library(PSweight) # For overlap weights from Li et al (2018)

# Treatment probabilities within matched sets: n_treated / n
ps_fit <- PSmethod(
  ps.formula = UN ~ factor(fm), # Reproduces n_treated / n in each set
  method     = "glm",           # Estimate using generalized linear model
  # Use as.data.frame() for PSmethod() compatibility
  data       = as.data.frame(data_matched),
  ncate      = 2                # Binary treatment (treated vs. control)
)

assign_prob <- ps_fit$e.h[, "1"]  # Column for treatment level "1"

# Overlap weights from treatment probabilities
# Controls: treatment probability ; Treated: 1 - treatment probability
w_ow <- ifelse(
  test = data_matched$UN == 1,
  yes  = 1 - assign_prob,
  no   = assign_prob
)

# Overlap-weighted difference in means
dim_ow <- data_matched |>
  summarize( # Aggregate rows into summary values
    # Overlap-weighted mean outcome for treated units
    treated_weighted_mean =
      sum(w_ow[UN == 1] * ldur_tilde[UN == 1]) / sum(w_ow[UN == 1]),
    
    # Overlap-weighted mean outcome for control units
    control_weighted_mean =
      sum(w_ow[UN == 0] * ldur_tilde[UN == 0]) / sum(w_ow[UN == 0]),
    
    # Difference between overlap-weighted treated and control means
    dim_ow = treated_weighted_mean - control_weighted_mean
  ) |>
  pull(
    dim_ow
  )

# Approaches coincides with the sum statistic
all.equal(
  dim_ow, obs_stat
)

# ---- chunk ----
# For each matched set (fm), record:
# n = total units in the set
# m = number treated (UN == 1)
block_ns <- data_matched |>
  group_by(          # Group results by key variables
    fm
  ) |>
  summarise(         # Aggregate to one row per group
    n = n(),         # Row count per group
    m = sum(UN),
    .groups = "drop"
  )

# Total possible treatment assignments = product of binomial coefficients
# (choose n_s units for treatment in each set and multiply across sets)
exact_n_assigns <- prod(
  choose(
    n = block_ns$n,
    k = block_ns$m
  )
)

# ---- chunk ----
# Install "randomizr" (only run if not already installed)
# install.packages("randomizr")

# Load randomizr for generating random assignments
library(randomizr)

exact_assigns <- obtain_permutation_matrix(
  declaration = declare_ra(   # Declare assignment procedure
    N = nrow(data_matched),   # Total number of units
    blocks = data_matched$fm, # Matched set membership
    block_m = block_ns$m      # Number treated in each set
  ),
  # Total number of assignments with treated counts fixed within sets
  maximum_permutations = exact_n_assigns
)

# ---- chunk ----
# Set RNG seed for reproducibility
set.seed(11242017)

# Generate permutation matrix of treatment assignments
# Each column = one possible assignment consistent with block structure
sim_assigns <- obtain_permutation_matrix(
  declaration = declare_ra(
    N = nrow(data_matched),
    blocks = data_matched$fm,
    block_m = block_ns$m
  ),
  maximum_permutations = 10^4  # Cap at 10,000 random draws
)

# ---- chunk ----
# Randomization distribution under sharp null
# Apply sum statistic to each assignment in sim_assigns
# Outcome transformed so sum = harmonic-mean weighted diff in means
sim_sharp_null_dist <- apply(
  X = sim_assigns,    # Matrix of treatment assignments
  MARGIN = 2,         # Iterate over columns (assignments)
  FUN = function(x) {
    # Sum transformed outcomes among treated
    sum(
      data_matched$ldur_tilde_hm_scaled[x == 1]
    )
  }
)
# Faster equivalent computation via matrix multiplication
# as.numeric(
#   t(data_matched$ldur_tilde_hm_scaled) %*% sim_assigns
# )

# ---- chunk ----
# One-sided, upper p-value:
# Propprtion of simulated randomization stats >= observed
round(
  x = mean(
    sim_sharp_null_dist >= obs_stat
  ),
  digits = 4
)

# ---- chunk ----
# Transformed outcomes under sharp null (tau_h = 0)
q_tau_h_0 <- data_matched$ldur_tilde_hm_scaled

# Fast computation of exact null distribution using matrix multiplication
exact_sharp_null_dist <- as.numeric(
  t(q_tau_h_0) %*% exact_assigns
)

# Slower apply()-based computation (for reference)
# apply(
#  X = exact_assigns,
#  MARGIN = 2,
#  FUN = function(x) {
#    sum(data_matched$ldur_tilde_hm_scaled[x == 1])
#  }
#)

# Exact one-sided, upper p-value
round(
  x = mean(
    exact_sharp_null_dist >= obs_stat
  ),
  digits = 4
)

# ---- chunk ----
# Install "senstrat" package (only run if not already installed)
# install.packages("senstrat")

# Load senstrat for stratum-level null moments and sensitivity analysis
# (Rosenbaum & Krieger 1990)
library(senstrat)

# Compute per-block null expectations and variances
per_block_moms <- data_matched |>
  group_by(
    fm
  ) |>
  summarize(
    expect   = ev(
      sc = ldur_tilde_hm_scaled, # Transformed outcomes for the stratum
      z = UN,                    # Treatment indicator
      m = 1,                     # Count of 1s in hidden confounder
      # Irrelevant here since Gamma = 1
      g = 1,                     # Sensitivity parameter Gamma
      method = "RK"              # Use Rosenbaum and Krieger (1990) formula 
    )$expect,                    # Null expectation of sum statistic by set
    variance = ev(
      sc     = ldur_tilde_hm_scaled,
      z      = UN,
      m = 1,
      g      = 1,
      method = "RK"
    )$vari,                      # Null variance of sum statistic by set
    .groups  = "drop"
  )

# Sum across blocks to get overall null expectation
null_ev  <- sum(
  per_block_moms$expect
)

# Sum across blocks to get overall null variance
null_var <- sum(
  per_block_moms$variance
)

# Standardized test statistic and one-sided Normal p-value (upper tail)
norm_upper_p_value <- pnorm(
  q = (obs_stat - null_ev) / sqrt(null_var), # Standardized statistic
  lower.tail = FALSE                         # Compute upper-tail prob
)
# By default in pnorm(): mean = 0 and sd = 1 (standard normal distribution)

# ---- chunk ----
# Significance level
alpha <- 0.05

# Upper-tail confidence set (tau_h values not rejected by upper-tail test)
# Lower endpoint: smallest tau_h with upper-tail p-value >= alpha
# (any smaller tau_h would be rejected)
cs_sharp_lower_one_sided <- c(
  lower = obs_stat - qnorm(p = 1 - alpha) * sqrt(null_var),
  upper = Inf
)
# qnorm(1 - alpha): upper one-sided Normal critical value

cs_sharp_two_sided <- c(
  lower = obs_stat - qnorm(p = 1 - alpha / 2) * sqrt(null_var),
  upper = obs_stat + qnorm(p = 1 - alpha / 2) * sqrt(null_var)
)
# At lower endpoint, upper-tail one-sided p-value equals alpha/2;
# any smaller null value would be rejected by two-sided test
# At upper endpoint, lower-tail one-sided p-value equals alpha/2;
# any larger null value would be rejected by two-sided test

# ---- chunk ----
# Grid of constant-effect null values (sharp framework)
tau_h_grid <- seq(
  from = -0.02,
  to = 1.5,
  by = 0.0001
)

# Repeat the randomization test for each tau_h
# sapply() computes and stacks the resulting p-values
p_mat <- sapply(X = tau_h_grid,
                FUN = function(tau_h) {
                  
                  # Shift outcomes under null: ldur_i - tau_h * UN_i
                  dat_tau_h <- hm_stat_rescale(
                    # transform(): copy data_matched adding shifted outcome
                    data    = transform(
                      data_matched,
                      ldur_tilde_shift = ldur - tau_h * UN),
                    outcome = ldur_tilde_shift,
                    treat   = UN,
                    strata  = fm
                  )
                  
                  # Transformed outcomes under sharp null tau_h
                  q_tau_h <- dat_tau_h$ldur_tilde_shift_hm_scaled
                  
                  # Observed statistic under null tau_h
                  obs_stat_tau_h <- sum(
                    dat_tau_h$UN * q_tau_h
                  )
                  
                  # Randomization distribution via simulated assignments
                  # Matrix multiplication used for speed (vs apply())
                  sim_null_dist_tau_h <- as.numeric(
                    t(q_tau_h) %*% sim_assigns
                  )
                  
                  
                  # Upper-tail and lower-tail randomization p-values
                  p_upper_tau_h <- mean(
                    sim_null_dist_tau_h >= obs_stat_tau_h
                  )
                  
                  p_lower_tau_h <- mean(
                    sim_null_dist_tau_h <= obs_stat_tau_h
                  )
                  
                  c(
                    upper_tail = p_upper_tau_h,
                    lower_tail = p_lower_tau_h
                  )
                })

# Extract vector of upper-tail p-values
p_upper <- p_mat["upper_tail", ]

# Extract vector of lower-tail p-values
p_lower <- p_mat["lower_tail", ]

# Simulation-based confidence sets

# Upper-tail confidence set: retain all tau_h with upper-tail p >= alpha
cs_sharp_upper_tail_sim <- tau_h_grid[p_upper >= alpha]

# Lower bound of the upper-tail confidence set
cs_sharp_upper_tail_sim_bound <- min(
  cs_sharp_upper_tail_sim
)

# Two-sided confidence set (inversion using alpha/2 in each tail):
# retain tau_h only if neither one-sided test rejects at level alpha/2
cs_sharp_two_sided_sim <- tau_h_grid[
  p_upper >= alpha / 2 & p_lower >= alpha / 2
]

# Two-sided confidence set summarized by its bounds
cs_sharp_two_sided_sim_bounds <- c(
  lower = min(
    cs_sharp_two_sided_sim
  ),
  upper = max(
    cs_sharp_two_sided_sim
  )
)

# ---- chunk ----
# Install blkvar (only run if not already installed)
# install.packages("remotes")
# Remotes::install_github("lmiratrix/blkvar")

# Load blkvar for block randomization variance estimators
library(blkvar)

# Compute results with hybrid_p method
res <- block_estimator(
  Yobs = ldur,         # Observed outcomes
  Z = UN,              # Treatment indicator
  B = fm,              # Block (matched set) membership
  data = data_matched, # Dataset
  method = "hybrid_p"  # variance estimation method
)

# Extract variance estimate
res$var_est

# ---- chunk ----
# Load the fine_strat_var_est() function from the GitHub repo
source(
  paste0(
    base_url, "/R/fine_strat_var_est.R"
  )
)

# ---- chunk ----
# Compute matched set sizes and matched-set-specific differences in means
set_stats <- data_matched |>
  group_by(
    fm
  ) |>
  summarize(
    n = n(),                              # Matched set size
    diff_in_means = mean(                 # Treated mean
      ldur[UN == 1L]
    ) -                                   # minus
      mean(                               # control mean
        ldur[UN == 0L]
      ),
    .groups = "drop"
  )

# Apply Fogarty (2018/2023) variance estimator
fine_strat_var_est(
  strat_ns = set_stats$n,               # Vector of stratum sizes
  strat_ests = set_stats$diff_in_means  # Vector of stratum estimates
)

# ---- chunk ----
pnorm(
  q = (res$ATE_hat - 0) / sqrt(res$var_est),
  lower.tail = FALSE
)

# ---- chunk ----
# One-sided (upper-tail) confidence set
cs_weak_upper_tail <- c(
  lower = res$ATE_hat - qnorm(p = 1 - alpha) * sqrt(res$var_est),
  upper = Inf
)

# Two-sided confidence set
cs_weak_two_sided <- c(
  lower = res$ATE_hat - qnorm(p = 1 - alpha / 2) * sqrt(res$var_est),
  upper = res$ATE_hat + qnorm(p = 1 - alpha / 2) * sqrt(res$var_est)
)

# ---- chunk ----
# Rosenbaum (2018) objective: expectation - observed + kappa * SD
# Nonpositive values imply rejection at level alpha
sens_objective <- function(null_expect, null_variance, obs_stat, alpha = 0.05) {
  # null_expect   : overall null expectation of the test statistic
  # null_variance : overall null variance of the test statistic
  # obs_stat      : observed value of the test statistic
  # alpha         : test size (e.g., 0.05 gives kappa about 1.64)
  
  # Normal critical value
  kappa  <- qnorm(
    p = 1 - alpha
  )
  
  # Null standard deviation
  null_sd <- sqrt(
    null_variance
  )
  
  # Gap between null expectation and observed statistic
  # (null_expect - obs_stat), + Normal buffer (kappa * null_sd)
  obj_val <- (null_expect - obs_stat) + kappa * null_sd
  
  return(obj_val)
}

# ---- chunk ----
# Evaluate under Gamma = 1 using null_ev, null_var, and obs_stat
sens_objective(
  null_expect   = null_ev,
  null_variance = null_var,
  obs_stat      = obs_stat,
  alpha         = 0.05
)

# ---- chunk ----
# Grid of Gamma values
Gamma_vals <- seq(
  from = 1,
  to = 1.5,
  by = 0.0001
)

# lapply() runs sensitivity analysis at each Gamma, returns list of results
sens_results <- lapply(
  X = Gamma_vals,
  FUN = function(g) {
    out <- senstrat(
      sc          = data_matched$ldur_tilde_hm_scaled, # outcome
      z           = data_matched$UN,                   # treatment indicator
      st          = data_matched$fm,                   # matched set
      gamma       = g,                                 # Gamma
      alternative = "greater",                         # upper-tail test
      detail      = TRUE # return intermediate quantities
    )
    
    # Separable p-value
    p_sep <- as.numeric(
      out$Separable["P-value"]
    )
    
    # Taylor series margin (cvA)
    cvA <- as.numeric(
      out$lambda["Linear Taylor bound"]
    )
    
    # Decisions
    sep_reject <- p_sep <= alpha          # separable rejects?
    tay_reject <- cvA <= 0                # Taylor rejects?
    
    # Store results for this value of Gamma
    data.frame(
      Gamma     = g,        # sensitivity parameter
      p_sep     = p_sep,    # separable p-value
      cvA       = cvA,      # Taylor bound
      agree_tay = (sep_reject == tay_reject) # agreement indicator
    )
  }
)

# Bind into a single data frame
sens_df <- do.call(
  what = rbind,
  args = sens_results
)

# Smallest Gamma where the separable p-value is >= alpha
sens_value_sep <- min(
  sens_df$Gamma[sens_df$p_sep >= alpha]
)

# Taylor series sensitivity value:
# smallest Gamma at which Taylor margin becomes positive (non-rejection)
sens_value_tay <- min(
  sens_df$Gamma[sens_df$cvA > 0]
)

# ---- chunk ----
# Install tidyr (only run if not already installed)
# Install.packages("tidyr")

# Load tidyr for reshaping data
library(tidyr)

## Reshape results into long format for plotting ----
## We want one column for the quantity being plotted
## (separable p-value vs. Taylor series margin) and one
## column for the numeric value, so that we can facet easily.

sens_df_long <- sens_df |>
  select(Gamma, p_sep, cvA, agree_tay) |>
  pivot_longer(
    cols      = c(p_sep, cvA),   # quantities to plot
    names_to  = "quantity",      # which quantity is being shown
    values_to = "value"          # numeric value of that quantity
  ) |>
  mutate(
    # Make quantity a factor with readable labels
    quantity = factor(
      quantity,
      levels = c("p_sep",           "cvA"),
      labels = c("Separable p-value",
                 "Taylor series margin")
    )
  )

## Identify regions of disagreement across Gamma ----
## Disagreement occurs when the separable decision
## (p_sep <= alpha) differs from the Taylor decision (cvA <= 0).

disagree_df <- sens_df |>
  filter(!agree_tay)

## If there is any disagreement at all, we summarize it
## by taking the minimum and maximum Gamma over which
## disagreement occurs, and use this to define a shaded band.
## (This keeps the visualization simple and interpretable.)

if (nrow(disagree_df) > 0) {
  
  gamma_min_disagree <- min(disagree_df$Gamma)
  gamma_max_disagree <- max(disagree_df$Gamma)
  
  # Create a data frame defining the shaded region.
  # We repeat the band for each facet so it appears
  # behind both panels.
  band_df <- data.frame(
    xmin     = gamma_min_disagree,
    xmax     = gamma_max_disagree,
    quantity = factor(
      c("Separable p-value",
        "Taylor series margin"),
      levels = levels(sens_df_long$quantity)
    )
  )
  
} else {
  # If there is no disagreement anywhere, do not draw a band
  band_df <- NULL
}


## Use a colorblind-friendly palette and keep colors
## consistent with previous figures in the guide.

plot_cols <- c(
  "Separable p-value"    = "#0072B2",  # blue
  "Taylor series margin" = "#D55E00"   # vermillion
)

## Plot sensitivity results ----
ggplot(sens_df_long,
       aes(x = Gamma, y = value, color = quantity)) +
  
  # Reference line for separable p-values: alpha
  geom_hline(
    data = subset(sens_df_long, quantity == "Separable p-value"),
    aes(yintercept = alpha),
    color = "grey50"
  ) +
  
  # Reference line for Taylor series margin: zero is rejection boundary
  geom_hline(
    data = subset(sens_df_long, quantity == "Taylor series margin"),
    aes(yintercept = 0),
    color = "grey50"
  ) +
  
  ## Shaded vertical band indicating Gamma values
  ## where the separable and Taylor decisions disagree
  (if (!is.null(band_df))
    geom_rect(
      data = band_df,
      aes(xmin = xmin, xmax = xmax,
          ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = "grey80",
      alpha = 0.4
    )
  ) +
  
  ## Plot curves for each quantity
  geom_line(linewidth = 0.5) +
  
  ## Manual colors, no legend needed because facets label quantities
  scale_color_manual(values = plot_cols, guide = "none") +
  
  theme_bw() +
  
  ## Separate panels for p-values and margins
  facet_wrap(~ quantity, ncol = 1, scales = "free_y") +
  
  ## Gamma axis formatting
  scale_x_continuous(
    breaks = seq(from = 1, to = 1.5, by = 0.1)
  ) +
  
  ## Axis labels and theme tweaks
  xlab(expression(Gamma)) +
  ylab(NULL) +
  theme(
    strip.text        = element_text(face = "bold"),
    legend.position   = "none",
    panel.grid.minor.x = element_blank(),  # remove vertical minor grid lines
    panel.grid.major.x = element_blank()   # remove vertical major grid lines
  )

# ---- chunk ----
# Load the worst_case_IPW() function from GitHub repo
source(
  paste0(
    base_url, "/R/worst_case_IPW.R"
  )
)

# ---- chunk ----
# One-sided sensitivity analysis for the weak null (ATE = 0)

# Null and alternative for the weak-null test
null_ATE    <- 0                  # Null: ATE = 0

alternative <- "greater"          # Alt: ATE > 0 (upper-tail test)

# Data frame to store results for each Gamma
weak_sens_df <- data.frame(
  Gamma   = Gamma_vals,
  p_value = NA_real_
)

for (g in Gamma_vals) { # Loop over Gamma values
  
  # Worst-case IPW estimate within each matched set at Gamma = g
  set_stats <- data_matched |>
    group_by(
      fm
    ) |>
    summarise(
      n      = n(),                        # Set size n_s
      weight = n / nrow(data_matched),     # Weight n_s / n
      est    = worst_case_IPW(             # Worst-case IPW in set s
        z           = UN,                  # Treatment indicator
        y           = ldur,                # Outcome
        Gamma       = g,                   # Sensitivity parameter (Gamma)
        tau_h       = null_ATE,            # Weak null: ATE = 0
        alternative = alternative          # Direction of alternative
      ),
      .groups = "drop"
    )
  
  # Overall worst-case IPW statistic (weighted average across sets)
  wc_ipw_stat <- sum(
    set_stats$weight * set_stats$est
  )
  
  # Variance estimate for the worst-case IPW statistic
  var_hat <- fine_strat_var_est(
    strat_ns   = set_stats$n,
    strat_ests = set_stats$est
  )
  
  # Standardized worst-case IPW statistic, centered at the null
  se_hat   <- sqrt(
    var_hat
  )
  
  std_stat <- wc_ipw_stat / se_hat
  
  # One-sided p-value, determined by the alternative
  if (alternative == "greater") {
    # Upper-tail test: ATE > 0
    p_val <- 1 - pnorm(std_stat)
  } else if (alternative == "less") {
    # Lower-tail test: ATE < 0
    p_val <- pnorm(std_stat)
  } else {
    stop("Only one-sided alternatives ('greater' or 'less') are handled here.")
  }
  
  # Store result for this Gamma
  weak_sens_df$p_value[weak_sens_df$Gamma == g] <- p_val
}

# Sensitivity value: smallest Gamma where one-sided test no longer rejects
weak_sens_value <- min(
  weak_sens_df$Gamma[weak_sens_df$p_value >= alpha]
)

# ---- chunk ----
# Plot weak-null sensitivity results: Gammas vs. p-values
ggplot(data = weak_sens_df,
       mapping = aes(x = Gamma,
                     y = p_value)) +
  geom_hline(yintercept = alpha,
             linetype = "solid",
             color = "grey") +
  geom_line(color = "black") +  # P-value curve
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 1,
                                  to = 1.5,
                                  by = 0.1)) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 0.1,
                                  by = 0.01),
                     limits = c(0, 0.1)) +
  xlab(expression(Gamma)) +
  ylab("One-sided upper p-value")