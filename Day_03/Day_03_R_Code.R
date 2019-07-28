## Calculating the expected value and variance of Z_i
exp_val_Z <- 1 * 0.75 + 0 * (1 - 0.75)

var_Z <- (1 - exp_val_Z)^2 * 0.75 + (0 - exp_val_Z)^2 * (1 - 0.75)

## Research design unser simple, individual random assignment
sra_treated <- lapply(X = 0:8,
                      FUN = function(x) { combn(x = 8,
                                                m = x) })

sra_z_vecs <- lapply(X = 1:length(sra_treated),
                     FUN = function(t) { apply(X = sra_treated[[t]],
                                               MARGIN = 2,
                                               FUN = function(x) { as.integer(1:8 %in% x) }) })

sra_z_vecs_mat <- matrix(data = unlist(sra_z_vecs),
                         nrow = 8,
                         byrow = FALSE)

indiv_probs <- rep(x = (1/2), times = 8)

sra_vec_probs <- apply(X = sra_z_vecs_mat,
                       MARGIN = 2,
                       FUN = function(x) { prod(indiv_probs^(x) * (1 - indiv_probs)^(1 - x)) })

## The number of treated units as a random variable under simple random assignment
N_1s <- apply(X = sra_z_vecs_mat,
              MARGIN = 2,
              FUN = sum)

N_1_probs <- sapply(X = 0:8,
                    FUN = function(n_1) { sum(sra_vec_probs[which(N_1s == n_1)])  })

N_1_ran_var <- cbind(0:8, N_1_probs)

colnames(N_1_ran_var) <- c("N_1", "Prob")

exp_val_n_1 <- sum(N_1_ran_var[,"N_1"] * N_1_ran_var[,"Prob"])

var_n_1 <- sum((N_1_ran_var[,"N_1"] - exp_val_n_1)^2 * N_1_ran_var[,"Prob"])

## Deriving probability dist on Omega under complete, individual assignment
cra_total_prob <- sum(sra_vec_probs[which(N_1s == 4)])

cra_vec_probs <- sra_vec_probs[which(N_1s == 4)]/cra_total_prob

n <- 8
n_1 <- 4

treated <- combn(x = 1:n,
                 m = n_1) 

Omega <- apply(X = treated,
               MARGIN = 2,
               FUN = function(x) { as.integer(1:n %in% x) })

cra_vec_probs <- rep(x = (1/ncol(Omega)), times = ncol(Omega))

## Defining different test statistics
treat_sum <- function(.Z,
                      .y_c,
                      .y_t) {
  
  y = .Z * .y_t + (1 - .Z) * .y_c
  
  return( as.integer(t(.Z) %*% y) )
  
}

treat_mean <- function(.Z, .y_c, .y_t) {
  
  y = .Z * .y_t + (1 - .Z) * .y_c
  
  return( (1/sum(.Z)) * as.integer(t(.Z) %*% y) ) 
  
}

mean_diff <- function(.Z, .y_c, .y_t) {
  
  y = .Z * .y_t + (1 - .Z) * .y_c
  
  return( (1/sum(.Z)) * as.integer(t(.Z) %*% y) - (1/sum(1 - .Z)) * as.integer(t(1 - .Z) %*% y) ) 
  
}

## Calculate p-value in lady tasting tea experiment
y_c_null <- c(rep(x = 1, times = 4), rep(x = 0, times = 4))
y_t_null <- c(rep(x = 1, times = 4), rep(x = 0, times = 4))

cbind(y_c_null, y_t_null)

null_test_stats <- apply(X = Omega,
                         MARGIN = 2,
                         FUN = function(x) { treat_sum(.Z = x,
                                                       .y_c = y_c_null,
                                                       .y_t = y_t_null) })

null_prob_dist <- cbind(0:4,
                        sapply(X = 0:4,
                               FUN = function(x) { sum((null_test_stats == x) * cra_vec_probs) }))

colnames(null_prob_dist) <- c("Test_Stat", "Prob")

null_prob_dist

## Showing properties of p-values

n <- 8
n_1 <- 4

treated <- combn(x = 1:n,
                 m = n_1) 

Omega <- apply(X = treated,
               MARGIN = 2,
               FUN = function(x) { as.integer(1:n %in% x) })

cra_vec_probs <- rep(x = (1/ncol(Omega)), times = ncol(Omega))

y_c_null_true <- c(rep(x = 1, times = 4), rep(x = 0, times = 4))
y_t_null_true <- c(rep(x = 1, times = 4), rep(x = 0, times = 4))

get_p_value <- function(.y_c,
                        .y_t,
                        .Omega,
                        .probs) {
  
  obs_outs = sapply(X = 1:ncol(.Omega),
                    FUN = function(x) { Omega[,x] * .y_t + (1 - Omega[,x]) * .y_c })
  
  obs_test_stats = sapply(X = 1:ncol(.Omega),
                          FUN = function(x){ as.integer(t(Omega[,x]) %*% obs_outs[,x]) })
  
  null_dists <- list()
  
  for(i in 1:ncol(.Omega)){
    
    null_dists[[i]] = sapply(X = 1:ncol(.Omega),
                             FUN = function(x) { as.integer(t(Omega[,x]) %*% obs_outs[,i]) })
  }
  
  p_values = sapply(X = 1:ncol(.Omega),
                    FUN = function(x) { sum((null_dists[[x]] >= obs_test_stats[x]) * .probs)  })
  
  return(list("obs_test_stats" = obs_test_stats,
              "p_values" = p_values))
  
}

p_values_null_true <- get_p_value(.y_c = y_c_null_true,
                                  .y_t = y_t_null_true,
                                  .Omega = Omega,
                                  .probs = cra_vec_probs)$p_values

## The probability of rejecting the null hypothesis when it is true
sum((p_values_null_true <= 0.05) * cra_vec_probs)



y_c_null_false <- rep(x = 0, times = 8)
y_t_null_false <- rep(x = 1, times = 8)

cbind(y_c_null_false, y_t_null_false)

get_p_value <- function(.y_c,
                        .y_t,
                        .Omega,
                        .probs) {
  
  obs_outs = sapply(X = 1:ncol(.Omega),
                    FUN = function(x) { Omega[,x] * .y_t + (1 - Omega[,x]) * .y_c })
  
  obs_test_stats = sapply(X = 1:ncol(.Omega),
                          FUN = function(x){ as.integer(t(Omega[,x]) %*% obs_outs[,x]) })
  
  null_dists <- list()
  
  for(i in 1:ncol(.Omega)){
    
    null_dists[[i]] = sapply(X = 1:ncol(.Omega),
                             FUN = function(x) { as.integer(t(Omega[,x]) %*% obs_outs[,i]) })
  }
  
  p_values = sapply(X = 1:ncol(.Omega),
                    FUN = function(x) { sum((null_dists[[x]] >= obs_test_stats[x]) * .probs)  })
  
  return(list("obs_test_stats" = obs_test_stats,
              "p_values" = p_values))
  
}

p_values_null_false <- get_p_value(.y_c = y_c_null_false,
                                   .y_t = y_t_null_false,
                                   .Omega = Omega,
                                   .probs = cra_vec_probs)$p_values

## The probability of rejecting the null hypothesis when it is true
sum((p_values_null_false <= 0.05) * cra_vec_probs)

cbind(p_values_null_true, p_values_null_false)
            

