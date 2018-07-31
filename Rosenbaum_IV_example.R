rm(list=ls())
n <- 8
n_t <- 4

treated <- combn(x = 1:n,
                 m = n_t) 

Omega <- apply(X = treated,
               MARGIN = 2,
               FUN = function(x) { as.integer(1:n %in% x) })

## HERE WE SATISFY THE RANDOM ASSIGNMENT OF THE INSTRUMENT ASSUMPTION
cra_vec_probs <- rep(x = (1/ncol(Omega)), times = ncol(Omega))

## first consider case of testing sharp null when the null is false and the alternative of a positive effect is true
set.seed(1:5)
d_c <- rbinom(n = 8, size = 1, prob = 0.3)
d_t <- rep(x = NA, times = length(d_c))
## HERE WE SATISFY THE AT LEAST ONE COMPLIER (NON-WEAK INSTRUMENT) ASSUMPTION
d_t[which(d_c != 1)] <- rbinom(n = length(which(d_c != 1)), size = 1, prob = 0.6)
## HERE WE SATISFY THE NO DEFIERS (MONOTONICITY) ASSUMPTION
d_t[which(d_c == 1)] <- rep(x = 1, times =  length(which(d_c == 1)))
cbind(d_c, d_t)
prop_comp <- length(which(d_c == 0 & d_t == 1))/n
prop_def <- length(which(d_c == 1 & d_t == 0))/n
prop_at <- length(which(d_c == 1 & d_t == 1))/n
prop_nt <- length(which(d_c == 0 & d_t == 0))/n

## HERE WE SATISFY THE EXCLUSION RESTRICTION ASSUMPTION BY LETTING y_c = y_t FOR ALL ALWAYS-TAKERS AND NEVER-TAKERS AND
## WE ALSO SATISFY THE SUTVA ASSUMPTION BY LETTING ALL UNITS HAVE ONLY TWO POT OUTS
set.seed(1:5)
y_c <- round(x = rnorm(n = 8, mean = 20, sd = 10), digits = 0)
y_t_null_false <- rep(x = NA, times = n)
y_t_null_false[which(d_c == 0 & d_t == 1)] <- y_c[which(d_c == 0 & d_t == 1)] + round(x = rnorm(n = length(which(d_c == 0 & d_t == 1)),
                                                                                                mean = 10, sd = 4),
                                                                                      digits = 0)
y_t_null_false[!(d_c == 0 & d_t == 1)] <- y_c[!(d_c == 0 & d_t == 1)] 
cbind(y_c, y_t_null_false)

obs_ys_null_false <- apply(X = Omega, MARGIN = 2, FUN = function(x) { x * y_t_null_false + (1 - x) * y_c })
obs_ds <- apply(X = Omega, MARGIN = 2, FUN = function(x) { x * d_t + (1 - x) * d_c })

## this corresponds to Rosenbaum's notion of a "positive effect"
true_effects_d_y <- y_t_null_false[d_c == 0 & d_t == 1] - y_c[d_c == 0 & d_t == 1]

## Now under each possible realizaion of data, let's test the sharp null hypothesis of no effect
## Under the exclusion restriction assumption, we can construct unobserved pot outs
## via the following function:

## test stat under null is function only of z and y
iv_test_stat <- function(.treat_vec,
                         .obs_out_vec){
  
  return(((as.numeric(t(.treat_vec) %*% .obs_out_vec) / as.numeric(t(.treat_vec) %*% .treat_vec))))
  
}

gen_p_value <- function(.z,
                        .obs_ys,
                        .obs_ds,
                        .null_tau,
                        .Omega,
                        .Omega_probs,
                        .test_stat_fun,
                        .alternative){
  
  null_y_c = .obs_ys - (.obs_ds * .null_tau)
  
  null_y_t = .obs_ys - ((1 - .obs_ds) * .null_tau)
  
  obs_test_stat = .test_stat_fun(.treat_vec = .z, .obs_out_vec = .z * null_y_t + (1 - .z) * null_y_c )
  
  obs_null_outs = apply(X = .Omega,
                        MARGIN = 2,
                        FUN = function(x) { x * null_y_t + (1 - x) * null_y_c })
  
  null_test_stat_dist = sapply(X = 1:ncol(.Omega),
                               FUN = function(x) { .test_stat_fun(.treat_vec = .Omega[,x],
                                                                  .obs_out_vec = obs_null_outs[,x]) })
  
  lower_p_val = sum((null_test_stat_dist <= obs_test_stat) * .Omega_probs)
  upper_p_val = sum((null_test_stat_dist >= obs_test_stat) * .Omega_probs)
  two_sided_p_val = min(2 * min(lower_p_val, upper_p_val), 1)
  
  p_values = cbind(lower_p_val, upper_p_val, two_sided_p_val)
  
  colnames(p_values) = c("lower_p_val", "upper_p_val", "two_sided_p_val")
  
  if(.alternative == "lesser") { return(p_values[,1]) }
  if(.alternative == "greater") { return(p_values[,2]) }
  if(.alternative == "two.sided") { return(p_values[,3]) }
  
  return(p_values)
  
}

p_values_null_false <- sapply(X = 1:ncol(Omega),
                              FUN = function(x) { gen_p_value(.z = Omega[,x],
                                                              .obs_ys = obs_ys_null_false[,x],
                                                              .obs_ds = obs_ds[,x],
                                                              .null_tau = rep(x = 0, times = n),
                                                              .Omega = Omega,
                                                              .Omega_probs = cra_vec_probs,
                                                              .test_stat_fun = iv_test_stat,
                                                              .alternative = "greater") })

## check that rejection prob is greater than alpha
sum((p_values_null_false <= 0.05) * cra_vec_probs)
## it is

## now consider case of testing sharp null when the null is true and the alternative of a positive effect is false
y_t_null_true <- rep(x = NA, times = n)
y_t_null_true[which(d_c == 0 & d_t == 1)] <- y_c[which(d_c == 0 & d_t == 1)] + rep(x = 0, times = length(which(d_c == 0 & d_t == 1)))
y_t_null_true[!(d_c == 0 & d_t == 1)] <- y_c[!(d_c == 0 & d_t == 1)] 
cbind(y_c, y_t_null_true)

obs_ys_null_true <- apply(X = Omega, MARGIN = 2, FUN = function(x) { x * y_t_null_true + (1 - x) * y_c })

p_values_null_true <- sapply(X = 1:ncol(Omega),
                             FUN = function(x) { gen_p_value(.z = Omega[,x],
                                                             .obs_ys = obs_ys_null_true[,x],
                                                             .obs_ds = obs_ds[,x],
                                                             .null_tau = rep(x = 0, times = n),
                                                             .Omega = Omega,
                                                             .Omega_probs = cra_vec_probs,
                                                             .test_stat_fun = iv_test_stat,
                                                             .alternative = "greater") })

## should be less than or equal to alpha = 0.05
sum((p_values_null_true <= 0.05) * cra_vec_probs)
## it is

## we have just shown that the probability of rejecting the sharp null when it is false and the alternative of
## a positive effect is true is greater than the probability of rejecting the sharp null when it is true
## and the alternative of a positive effect is false






