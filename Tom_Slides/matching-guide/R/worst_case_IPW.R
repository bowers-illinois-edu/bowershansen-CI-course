# Worst-case IPW adjustment for a single matched set
# Args:
#   z           : 0/1 vector for one matched set (treated = 1, control = 0).
#                 Set must be "finely stratified": exactly 1 treated OR 1 control.
#   y           : numeric outcomes for same units.
#   Gamma       : Rosenbaum sensitivity parameter (>= 1).
#   tau_h       : value of weak null ATE (default 0).
#   alternative : "greater" (larger effect) or "less" (smaller effect).
#
# Returns:
#   Worst-case IPW-weighted, set-specific (diff-in-means - tau_h),
#   multiplied by 1 / |Omega| where |Omega| = choose(N, #treated).
#
# Probability bounds
#   N <- length(z)
#   lower = 1 / (Gamma * (n - 1) + 1)
#   upper = Gamma / ((n - 1) + Gamma)
#
worst_case_IPW <- function(z,
                           y,
                           Gamma,
                           tau_h = 0,
                           alternative = c("greater", "less")) {
  
  # ---- Basic checks ----
  alternative <- match.arg(alternative)
  if (!is.numeric(y) || length(y) != length(z))
    stop("y must be numeric and same length as z.")
  if (anyNA(z) || anyNA(y)) stop("z and y must not contain NAs.")
  if (!all(z %in% c(0, 1))) stop("z must be a 0/1 vector.")
  if (!is.numeric(Gamma) || length(Gamma) != 1 || Gamma < 1)
    stop("Gamma must be a numeric scalar >= 1.")
  
  n_treated <- sum(z)
  n_control <- sum(1 - z)
  N <- length(z)
  
  # Enforce finely stratified: exactly 1 treated OR exactly 1 control
  if (!(n_treated == 1L || n_control == 1L)) {
    stop("Each matched set must have exactly 1 treated OR exactly 1 control.")
  }
  
  # Number of assignments with the fixed number treated
  card_Omega <- choose(n = N, k = n_treated)
  
  # ---- Usual set-specific difference in means, centered at tau_h ----
  diff_means <- mean(y[z == 1]) - mean(y[z == 0])
  d <- (diff_means - tau_h)
  
  # ---- Uniform bounds on Pr(Z = z) for finely stratified set ----
  prob_lb <- 1 / (Gamma * (N - 1) + 1)
  prob_ub <- Gamma / ((N - 1) + Gamma)
  
  # ---- Worst-case IPW weight depending on alternative and sign of d ----
  if (alternative == "greater") {
    # Favor rejection if d > 0 -> use 1 / upper; otherwise 1 / lower
    w <- if (d > 0) 1 / prob_ub else 1 / prob_lb
  } else { # alternative == "less"
    # Favor rejection if d < 0 -> use 1 / upper; otherwise 1 / lower
    w <- if (d < 0) 1 / prob_ub else 1 / prob_lb
  }
  
  # ---- Multiply by 1 / |Omega| ----
  out <- (1 / card_Omega) * d * w
  
  # Tag output with chosen alternative
  attr(out, "alternative") <- alternative
  
  out
  
}