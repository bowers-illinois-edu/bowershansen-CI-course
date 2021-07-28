library(dplyr)
library(magrittr)
library(ggplot2)

rm(list=ls())
options(scipen = 999)

n <- 6

n_1 <- 3

y_c <- c(20, 8, 11, 10, 14, 1)
y_t <- c(22, 12, 11, 15, 18, 4)

treated <- combn(x = n,
                 m = n_1,
                 simplify = TRUE) 
Omega <- apply(X = treated,
               MARGIN = 2,
               FUN = function(x) as.integer(1:n %in% x))

probs <- rep(x = 1/ncol(Omega), times = ncol(Omega))

obs_pot_outs <- sapply(X = 1:ncol(Omega),
                       FUN = function(x) { y_t * Omega[,x] + y_c * (1 - Omega[,x]) })

cbind(y_c, y_t)

## covariance
y_c - mean(y_c)
y_t - mean(y_t)
(y_c - mean(y_c)) * (y_t - mean(y_t))
cov_pot_outs <- mean((y_c - mean(y_c)) * (y_t - mean(y_t)))
var_y_c <- mean((y_c - mean(y_c))^2)
var_y_t <- mean((y_t - mean(y_t))^2)

## checking cauchy-schwarz inequality
2 * cov_pot_outs <= var_y_c + var_y_t

source("true_diff_means_var_fun.R")

true_var_of_diff_means_est <- true_diff_means_var(.n = n,
                                                  .n_1 = n_1,
                                                  .y_c = y_c,
                                                  .y_t = y_t)

source("diff_means_var_est_fun.R")

consv_var_ests <- sapply(X = 1:ncol(Omega),
                         FUN = function(x){ diff_means_var_est(.n = n,
                                                               .n_1 = n_1,
                                                               .z = Omega[,x],
                                                               .y = obs_pot_outs[,x]) })

## var estimator is conservative in that its expected value is greater than the true variance
exp_consv_var_est <- sum(consv_var_ests * probs)

exp_consv_var_est > true_var_of_diff_means_est$var

## What if 2 * covariance were exactly equal to sum of variances of pot outs?

y_c <- c(20, 8, 11, 10, 14, 1)
y_t <- y_c + 3
cbind(y_c, y_t)
cov_pot_outs <- mean((y_c - mean(y_c)) * (y_t - mean(y_t)))
var_y_c <- mean((y_c - mean(y_c))^2)
var_y_t <- mean((y_t - mean(y_t))^2)

(6/5) * ((var_y_c/3) + (var_y_t/3))

2 * cov_pot_outs == var_y_c + var_y_t

true_var_of_diff_means_est <- true_diff_means_var(.n = n,
                                                  .n_1 = n_1,
                                                  .y_c = y_c,
                                                  .y_t = y_t)

obs_pot_outs <- sapply(X = 1:ncol(Omega),
                       FUN = function(x) { y_t * Omega[,x] + y_c * (1 - Omega[,x]) })


var_ests <- sapply(X = 1:ncol(Omega),
                   FUN = function(x){ diff_means_var_est(.n = n,
                                                         .n_1 = n_1,
                                                         .z = Omega[,x],
                                                         .y = obs_pot_outs[,x]) })

## Now the variance estimator is unbiased
exp_var_est <- sum(var_ests * probs) 

exp_var_est == true_var_of_diff_means_est$var


####################################################################################################################################
n <- 6

n_1 <- 3

y_c <- c(20, 8, 11, 10, 14, 1)
y_t <- c(22, 12, 11, 15, 18, 4)

true_mean_tau <- mean(y_t) - mean(y_c)

treated <- combn(x = n,
                 m = n_1,
                 simplify = TRUE) 
Omega <- apply(X = treated,
               MARGIN = 2,
               FUN = function(x) as.integer(1:n %in% x))

probs <- rep(x = 1/ncol(Omega), times = ncol(Omega))

obs_pot_outs <- sapply(X = 1:ncol(Omega),
                       FUN = function(x) { y_t * Omega[,x] + y_c * (1 - Omega[,x]) })

source("diff_means_est_fun.R")

diff_means_ests <- sapply(X = 1:ncol(Omega),
                          FUN = function(x) { diff_means_est(.y = obs_pot_outs[,x],
                                                             .z = Omega[,x]) })

consv_var_ests <- sapply(X = 1:ncol(Omega),
                         FUN = function(x) { diff_means_var_est(.n = n,
                                                                .n_1 = n_1,
                                                                .z = Omega[,x],
                                                                .y = obs_pot_outs[,x]) })

z_scores <- (diff_means_ests - 0)/consv_var_ests

Normal_p_values <- sapply(X = 1:length(z_scores),
                          FUN = function(x){ (1 - pnorm(q = z_scores[x],
                                                        mean = 0,
                                                        sd = sqrt(consv_var_ests[x]))) })

CIs <- t(sapply(X = 1:ncol(Omega),
                FUN = function(x) { c(diff_means_ests[x] - 1.96 * sqrt(consv_var_ests[1]),
                                      diff_means_ests[x] + 1.96 * sqrt(consv_var_ests[1]))  }))

colnames(CIs) <- c("lower_bound", "upper_bound")
  
## Assess coverage
sum((true_mean_tau >= CIs[,"lower_bound"] & true_mean_tau <= CIs[,"upper_bound"]) * probs)

## How would we assess power?  
  











