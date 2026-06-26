# Args:
#   strat_ns   : vector of set sizes n_s (length B, positive integers)
#   strat_ests : vector of set-specific estimates \hat{\tau}_s (length B)
# Returns:
#   Conservative variance estimator (scalar), per Fogarty (2018, 2023)

fine_strat_var_est <- function(strat_ns,
                               strat_ests){
  
  # ---- Basic checks (shape, type, positivity) ----
  if (length(strat_ns) != length(strat_ests))
    stop("strat_ns and strat_ests must have the same length.")
  
  if (anyNA(strat_ns) || anyNA(strat_ests))
    stop("strat_ns and strat_ests must not contain NA.")
  
  if (any(strat_ns <= 0))
    stop("All stratum sizes in strat_ns must be positive.")
  
  strat_ns   <- as.numeric(strat_ns)    # coerce stratum sizes to numeric
  strat_ests <- as.numeric(strat_ests)  # coerce stratum estimates to numeric
  
  N <- sum(strat_ns)    # total sample size
  B <- length(strat_ns) # number of strata (matched sets)
  
  # Fogarty (2018, p. 1040) "Let W be a B x B diagonal matrix whose
  # ith diagonal element contains the stratum weights w_i = K (n_i/N)"
  W <- diag(B * (strat_ns / N))          # here K = B
  
  # Fogarty (2018, p. 1040) "Let Q be an arbitrary B x L matrix with L < B"
  # Fogarty (2023, p. 2199) "Let Q = (Q_1, ... , Q_B)^T with Q_i = B(n_i/N)"
  Q <- as.matrix(B * (strat_ns / N))     # B x 1 matrix
  
  # solve(t(Q) %*% Q) = 1/S_w
  S_w <- sum(Q^2) # scalar; equals t(Q)Q since Q is Bx1
  
  # Fogarty (2023, p. 2199) "let H_Q = Q(Q^TQ)^{-1}Q^T be the hat matrix
  # corresponding to Q"
  H_Q <- Q %*% (1 / S_w) %*% t(Q) # B x B projection matrix
  
  # Fogarty (2018, p. 1040) "Define y_i = \hat{\tau}_i / sqrt(1 - h_{Qii)) as the
  # estimated treatment effect in stratum i divided by the square root of 1 minus
  # the leverage of  stratum i"
  # Guard against tiny numerical negatives inside sqrt (due to rounding)
  leverages <- pmin(pmax(diag(H_Q), 0), 1)             # clamp to [0,1]
  denom     <- sqrt(pmax(1 - leverages, .Machine$double.eps))
  y_Gamma   <- as.matrix(strat_ests / denom)           # B x 1 vector
  
  iden_minus_H_Q <- diag(nrow = nrow(H_Q)) - H_Q       # I_B - H_Q
  
  # Variance estimate: (1/B^2) * y^T W (I - H_Q) W y   [scalar]
  var_hat <- as.numeric((1 / B^2) *
                          (t(y_Gamma) %*% W %*% iden_minus_H_Q %*% W %*% y_Gamma))
  
  return(var_hat)
  
  # Alternatively, as in Fogarty (2023, p. 1040), we could omit W and directly
  # define y_Gamma as
  # y_Gamma = as.matrix(x = (B * (strat_ns/N) * strat_ests) / sqrt(1 - diag(H_Q)))
}