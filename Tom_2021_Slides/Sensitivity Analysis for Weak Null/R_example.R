setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

## Example from Fogarty 2020, pp. 11
N <- 3
n_T <- 1
n_C <- N - n_T
Omega <- apply(X = combn(x = N,
                         m = n_T,
                         simplify = TRUE) ,
               MARGIN = 2,
               FUN = function(x) as.integer(1:N %in% x))
y_T <- c(7.5, 1, -1.5)
y_C <- c(2, 1, 4)
bar_tau <- mean(y_T - y_C)
obs_pot_outs <- sapply(X = 1:ncol(Omega),
                       FUN = function(x) { Omega[,x] * y_T + (1 - Omega[,x]) * y_C })

deltas <- sapply(X = 1:N, FUN = function(x) { y_T[x] - mean(y_C[-x]) })

diff_means_ests <- sapply(X = 1:ncol(Omega), FUN = function(x) { sum(Omega[,x] * deltas) })
## equivalently can calculate diff-in-means via expression from Fogarty (2020), pp. 4
sapply(X = 1:ncol(Omega), FUN = function(x) { sum(Omega[,x] * y_T - (1 - Omega[,x]) * y_C / (N - 1)) })

mean_tau <- mean(deltas)

set.seed(1:5)
true_u <- round(x = runif(n = N, min = 0, max = 1), digits = 1)
Gamma <- 2
gamma <- log(Gamma)
indiv_probs <- exp(gamma * true_u) / (1 + exp(gamma * true_u))
unnorm_Omega_sens_probs <- sapply(X = 1:ncol(Omega),
                                  FUN = function(x) { exp(gamma * t(Omega[,x]) %*% true_u) })
Omega_sens_probs <- unnorm_Omega_sens_probs / sum(unnorm_Omega_sens_probs)
true_cond_indiv_probs <- t(Omega_sens_probs) %*% t(Omega)
exp(gamma * true_u) / sum(exp(gamma * true_u)) ## expression for cond_indiv_probs from Fogarty (2020), pp. 6

## IPW estimator, Fogarty (2020), pp. 10
## This estimator is unbiased under the true cond_indiv_probs
## And will always equal \bar{\tau}
IPW_ests_true_probs <- (1/N) * diff_means_ests * (1/true_cond_indiv_probs)
true_IPW_EV <- sum(IPW_ests_true_probs * Omega_sens_probs)
true_upper_p_values <- pnorm(q = (IPW_ests_true_probs - true_IPW_EV), lower.tail = FALSE)

## What if we use wrong probs?
false_cond_indiv_probs <- c(0.05, 0.4, 0.55)
IPW_ests_false_probs <- (1/N) * diff_means_ests * (1/false_cond_indiv_probs)
true_EV_IPW_false_probs <- sum(IPW_ests_false_probs * Omega_sens_probs)
## In expectation, we get a test statistic that is way too large
## And we fail to control the type I error rate asymptotically
## Notice that the distribution of the observed statistic stochastically dominates the standard Normal

## So how can we find a test statistic that is stochastically dominated by the standard Normal?
## We need to choose a test statistic whose expected value is guaranteed to be less than
## or equal to 0
## The test statistic proposed by Fogarty (2020), see pp. 10 -- 11, is when we weight a positive
## IPW estimate by the largest possible probability and a negative IPW estimate by the lowest
## possible probability
max_indiv_prob_all_u <- Gamma * n_T / (n_C + Gamma * n_T)
min_indiv_prob_all_u <- n_T / (Gamma * n_C + n_T)

## Let kappa equal 4 (see Fogarty (2020), pp. 9 -- 10)
kappa <- (0.25)^{-1}
kappa_inv <- kappa^{-1}
Gamma * kappa_inv
Ds <- diff_means_ests - ((Gamma - 1) / (1 + Gamma)) * abs(diff_means_ests)
Ks <- (1/N) * kappa * Ds
Ws <- (kappa / N) * ((1 + Gamma) / (2 * Gamma)) * Ds
worst_case_IPW_ests <- (1 / N) * (diff_means_ests / ((Gamma * kappa_inv) * (diff_means_ests >= 0) + kappa_inv * (diff_means_ests < 0)))
all.equal(Ws, worst_case_IPW_ests)

### Now check same results without interval restriction
Gamma_n_i <- Gamma * ((Gamma * (N - 1) + 1) / ((N - 1) + Gamma))
tilde_kappa <- Gamma * (N - 1) + 1
## check min probs
all.equal(tilde_kappa^{-1}, min_indiv_prob_all_u)
## check max probs
all.equal(tilde_kappa^{-1} * Gamma_n_i, max_indiv_prob_all_u)

tilde_Ds <- diff_means_ests - ((Gamma_n_i - 1) / (1 + Gamma_n_i)) * abs(diff_means_ests)
worst_case_IPW_ests <- (1 / N) * (diff_means_ests / ((tilde_kappa^{-1} * Gamma_n_i) * (diff_means_ests >= 0) + (tilde_kappa^{-1}) * (diff_means_ests < 0)))
tilde_Ws <- (tilde_kappa / N) * (1 + Gamma_n_i) / (2 * Gamma_n_i) * tilde_Ds
all.equal(tilde_Ws, worst_case_IPW_ests)

### Now focus on conservative variance estimator (Fogarty 2020, Section 7.1, pp. 14 -- 15)

## "Let Q be any B x P matrix that is constant over z \in \Omega with B > p" (Fogarty 2020, pp. 14)
## "For matched designs, a natural choice for Q would be the vector containing weights B (n_i / N) in the ith entry" (Fogarty 2020, pp. 15)
## "Let H_Q = Q(Q^TQ)^{-1}Q^T be [the] hat matrix [of Q]" (Fogarty 2020, pp. 14)
rm(list = ls())
N <- 8
y_T <- c(7.5, 1, -1.5, 4, 2, 8, 1, 6)
y_C <- c(2, 1, 4, 2, 5, 4, 8, 1, 4, 4)
mean_tau <- mean(y_T) - mean(y_C)
b <- c(1, 1, 1, 1, 1, 2, 2, 2)
z <- c(1, 0, 0, 0, 0, 1, 0, 0)
n_1 <- sum(b == 1)
n_2 <- sum(b == 2)
B <- length(unique(b))

library(ri)
Omega_b_1 <- genperms(Z = z[which(b == 1)])
Omega_b_2 <- genperms(Z = z[which(b == 2)])
Omega <- genperms(Z = z, blockvar = b)
set.seed(1:5)
true_u <- round(x = runif(n = N, min = 0, max = 1), digits = 1)
Gamma <- 2
gamma <- log(Gamma)
indiv_probs <- exp(gamma * true_u) / (1 + exp(gamma * true_u))
unnorm_Omega_sens_probs <- sapply(X = 1:ncol(Omega),
                                  FUN = function(x) { exp(gamma * t(Omega[,x]) %*% true_u) })
Omega_sens_probs <- unnorm_Omega_sens_probs / sum(unnorm_Omega_sens_probs)
true_cond_indiv_probs <- t(Omega_sens_probs) %*% t(Omega)
diff_means_b_1 <- sapply(X = 1:ncol(Omega),
                         FUN = function(x) { sum(Omega[which(b == 1), x] * y_T[which(b == 1)] -
                                                   (1 - Omega[which(b == 1), x]) * y_C[which(b == 1)] / (n_1 - 1)) })
diff_means_b_2 <- sapply(X = 1:ncol(Omega),
                         FUN = function(x) { sum(Omega[which(b == 2), x] * y_T[which(b == 2)] -
                                                   (1 - Omega[which(b == 2), x]) * y_C[which(b == 2)] / (n_1 - 1)) })
Ds_b_1 <- diff_means_b_1 - ((Gamma - 1) / (1 + Gamma)) * abs(diff_means_b_1)
Ds_b_2 <- diff_means_b_2 - ((Gamma - 1) / (1 + Gamma)) * abs(diff_means_b_2)
bar_Ds <- (n_1/N) * Ds_b_1 + (n_2 / N) * Ds_b_2
Q <- as.matrix(sapply(X = unique(b), FUN = function(x) { B * (sum(b == x) / N) }))
H_Q <- Q %*% solve(t(Q) %*% Q) %*% t(Q)
Y_Gamma_1 <- B * (n_1 / N) * Ds_b_1 / sqrt(1 - H_Q[1, 1])
Y_Gamma_2 <- B * (n_2 / N) * Ds_b_2 / sqrt(1 - H_Q[2, 2])
Y_Gammas <- rbind(Y_Gamma_1, Y_Gamma_2)
est_Vars <- sapply(X = 1:ncol(Y_Gammas),
                   FUN = function(x) { (t(Y_Gammas[,x]) %*% (diag(length(unique(b))) - H_Q) %*% Y_Gammas[,x]) / B^2 })
EV_est_vars <- sum(est_Vars * Omega_sens_probs)
true_var <- sum((bar_Ds - sum(bar_Ds * Omega_sens_probs))^2 * Omega_sens_probs)
EV_Y_Gamma_1 <- sum(Y_Gammas[1,] %*% Omega_sens_probs)
EV_Y_Gamma_2 <- sum(Y_Gammas[2,] %*% Omega_sens_probs)
EV_Y_Gamma <- as.matrix(c(EV_Y_Gamma_1, EV_Y_Gamma_2))
EV_est_vars - true_var
(1/B^2) * t(EV_Y_Gamma) %*% (diag(length(unique(b))) - H_Q) %*% EV_Y_Gamma

Gamma_1_n_i <- Gamma * ((Gamma * (n_1 - 1) + 1) / ((n_1 - 1) + Gamma))
Gamma_2_n_i <- Gamma * ((Gamma * (n_2 - 1) + 1) / ((n_2 - 1) + Gamma))
tilde_kappa_1 <- Gamma * (n_1 - 1) + 1
tilde_kappa_2 <- Gamma * (n_2 - 1) + 1
tilde_Ds_b_1 <- diff_means_b_1 - ((Gamma_1_n_i - 1) / (1 + Gamma_1_n_i)) * abs(diff_means_b_1)
tilde_Ds_b_2 <- diff_means_b_2 - ((Gamma_2_n_i - 1) / (1 + Gamma_2_n_i)) * abs(diff_means_b_2)

tilde_Q <- as.matrix(c(tilde_kappa_1 * Gamma_1_n_i, tilde_kappa_2 * Gamma_2_n_i))
tilde_H_Q <- tilde_Q %*% solve(t(tilde_Q) %*% tilde_Q) %*% t(tilde_Q)

Ks <- (1/N) * (tilde_Ds_b_1 * tilde_kappa_1 * Gamma_1_n_i + tilde_Ds_b_2 * tilde_kappa_2 * Gamma_2_n_i)
tilde_Y_Gamma_1 <- ((B/N) * tilde_kappa_1 * Gamma_1_n_i) * tilde_Ds_b_1 / sqrt(1 - H_Q[1, 1])
tilde_Y_Gamma_2 <- ((B/N) * tilde_kappa_2 * Gamma_2_n_i) * tilde_Ds_b_2 / sqrt(1 - H_Q[2, 2])
tilde_Y_Gammas <- rbind(tilde_Y_Gamma_1, tilde_Y_Gamma_2)
tilde_est_Vars <- sapply(X = 1:ncol(tilde_Y_Gammas),
                   FUN = function(x) { (t(tilde_Y_Gammas[,x]) %*% (diag(length(unique(b))) - H_Q) %*% tilde_Y_Gammas[,x]) / B^2 })
EV_tilde_est_vars <- sum(tilde_est_Vars * Omega_sens_probs)
true_tilde_var <- sum((Ks - sum(Ks * Omega_sens_probs))^2 * Omega_sens_probs)
EV_tilde_Y_Gamma_1 <- sum(tilde_Y_Gammas[1,] %*% Omega_sens_probs)
EV_tilde_Y_Gamma_2 <- sum(tilde_Y_Gammas[2,] %*% Omega_sens_probs)
EV_tilde_Y_Gamma <- as.matrix(c(EV_tilde_Y_Gamma_1, EV_tilde_Y_Gamma_2))
EV_tilde_est_vars - true_tilde_var
(1/B^2) * t(EV_tilde_Y_Gamma) %*% (diag(length(unique(b))) - H_Q) %*% EV_tilde_Y_Gamma

## Check with proportional worst-case IPW estimator
tilde_Ws_b_1 <- (tilde_kappa_1 / n_1) * (1 + Gamma_1_n_i) / (2 * Gamma_1_n_i) * tilde_Ds_b_1
tilde_Ws_b_2 <- (tilde_kappa_2 / n_2) * (1 + Gamma_2_n_i) / (2 * Gamma_2_n_i) * tilde_Ds_b_2
K_Ws <- (1/N) * (tilde_Ws_b_1 * tilde_kappa_1 * Gamma_1_n_i + tilde_Ws_b_2 * tilde_kappa_2 * Gamma_2_n_i)
tilde_W_Y_Gamma_1 <- ((B/N) * tilde_kappa_1 * Gamma_1_n_i) * tilde_Ws_b_1 / sqrt(1 - H_Q[1, 1])
tilde_W_Y_Gamma_2 <- ((B/N) * tilde_kappa_2 * Gamma_2_n_i) * tilde_Ws_b_2 / sqrt(1 - H_Q[2, 2])
tilde_W_Y_Gammas <- rbind(tilde_W_Y_Gamma_1, tilde_W_Y_Gamma_2)
tilde_W_est_Vars <- sapply(X = 1:ncol(tilde_W_Y_Gammas),
                           FUN = function(x) { (t(tilde_W_Y_Gammas[,x]) %*% (diag(length(unique(b))) - H_Q) %*% tilde_W_Y_Gammas[,x]) / B^2 })
EV_tilde_W_est_vars <- sum(tilde_W_est_Vars * Omega_sens_probs)
true_tilde_W_var <- sum((K_Ws - sum(K_Ws * Omega_sens_probs))^2 * Omega_sens_probs)
EV_tilde_W_Y_Gamma_1 <- sum(tilde_W_Y_Gammas[1,] %*% Omega_sens_probs)
EV_tilde_W_Y_Gamma_2 <- sum(tilde_W_Y_Gammas[2,] %*% Omega_sens_probs)
EV_tilde_W_Y_Gamma <- as.matrix(c(EV_tilde_W_Y_Gamma_1, EV_tilde_W_Y_Gamma_2))
EV_tilde_W_est_vars - true_tilde_W_var
(1/B^2) * t(EV_tilde_W_Y_Gamma) %*% (diag(length(unique(b))) - H_Q) %*% EV_tilde_W_Y_Gamma

## Finally figure out relationship for worst-case IPW and tilde_D
ests <- c(5, 10, 11, 9)
var_ests <- mean((ests - mean(ests))^2)
c <- 5
ests_c <- c * ests 
var_ests_c <- mean((ests_c - mean(ests_c))^2)
(ests - mean(ests)) / sqrt(var_ests)
(ests_c - mean(ests_c)) / sqrt(var_ests_c)

var((Ks - mean(Ks)) / sqrt(true_tilde_var))
var((K_Ws - mean(K_Ws)) / sqrt(true_tilde_W_var))




