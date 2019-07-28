## Lemma 1
n <- 8
n_1 <- 4

treated <- combn(x = 1:n,
                 m = n_1) 

Omega <- apply(X = treated,
               MARGIN = 2,
               FUN = function(x) { as.integer(1:n %in% x) })

cra_vec_probs <- rep(x = (1/ncol(Omega)),
                     times = ncol(Omega))

## The number of assignments is n choose n_1
choose(n = n,
       k = n_1)
ncol(Omega)

## The number of ways in which unit i could be in the treatment condition is:
choose(n = (n - 1),
       k = (n_1 - 1))

## For example, let i = 1. The number of assignments in Omega in which
## unit 1 is treated is:
unit_i_treated <- sum(sapply(X = 1:ncol(Omega),
                             FUN = function(x) { Omega[1,x] == 1 }))

## We could let i take on any value from 1 to 8 and we would
## get the same result. E.g., let i equal 7:
## For example, let i = 1. The number of assignments in Omega in which
## unit 1 is treated is:
sum(sapply(X = 1:ncol(Omega),
           FUN = function(x) { Omega[7,x] == 1 }))

## The proportion of assignments in which i is treated is therefore:
unit_i_treated/ncol(Omega) ## or
choose(n = n - 1, k = n_1 - 1)/choose(n = n, k = n_1)

## This proportion is the probability that unit i is treated:
sum((sapply(X = 1:ncol(Omega),
            FUN = function(x) { Omega[1,x] == 1 })) * cra_vec_probs)
    
## What about if n_1 were equal to 2 instead of 4? What is the prob that
## unit i is treated?

    
## Let's now show that the Difference-in-Means Estimator is Unbiased under
## complete, uniform random assignment
set.seed(1:5)
y_c <- round(x = rnorm(n = 8, mean = 10, sd = 2), digits = 0)
y_t <- y_c + 5

cbind(y_c, y_t)

mean(y_t) - mean(y_c)

obs_outs <- sapply(X = 1:ncol(Omega),
                   FUN = function(x) { Omega[,x] * y_t + (1 - Omega[,x]) * y_c })

diff_means <- function(.Z, .Y){
  
  return((t(.Z) %*% .Y)/(t(.Z) %*% .Z) - (t(1 - .Z) %*% .Y)/(t(1 - .Z) %*% (1-.Z)))
  
  }

estimates <- sapply(X = 1:ncol(Omega),
                    FUN = function(x) { diff_means(.Z = Omega[,x], .Y = obs_outs[,x])})

sum(estimates * cra_vec_probs)

mean(y_t) - mean(y_c)


## How about under simple random assignment?
## generate all ways in which 1 to 7 total units could be treated
## we are excluding the possibility that all 8 units could be in treatment or in control
sra_treated <- lapply(X = 1:7,
                      FUN = function(x) { combn(x = 8,
                                                m = x) })
## generate all possible assignments for each possible value of n_1
sra_z_vecs <- lapply(X = 1:length(sra_treated),
                     FUN = function(t) { apply(X = sra_treated[[t]],
                                               MARGIN = 2,
                                               FUN = function(x) { as.integer(1:8 %in% x) }) })

## combine all possible assignments into one matrix
all_sra_z_vecs_mat <- matrix(data = unlist(sra_z_vecs),
                             nrow = 8,
                             byrow = FALSE)

## generate observed outcomes for each possible assignment conditional on a value of n_1,
## where n_1 can range from 1 to 7
obs_outs <- list()

for(i in 1:length(sra_z_vecs)){
  
  obs_outs[[i]] = sapply(X = 1:ncol(sra_z_vecs[[i]]),
                             FUN = function(x) { sra_z_vecs[[i]][,x] * y_t + (1 - sra_z_vecs[[i]][,x]) * y_c })
  
}


## generate difference-in-means estimates for each possible assignment conditional on a value of n_1,
## where n_1 can range from 1 to 7
estimates <- list()

for(i in 1:length(sra_z_vecs)){
  
  estimates[[i]] = sapply(X = 1:ncol(sra_z_vecs[[i]]),
                          FUN = function(x) { diff_means(.Z = sra_z_vecs[[i]][,x], .Y = obs_outs[[i]][,x])})
  
}

## get probabilities of each assignment conditional on a value of n_1

probs <- lapply(X = 1:length(sra_z_vecs),
                FUN = function(x) { rep(x = 1/ncol(sra_z_vecs[[x]]), times = ncol(sra_z_vecs[[x]])) })

## calculated expected values of estimator conditional on each value of n_1
exp_values_est <- lapply(X = 1:length(estimates),
                         FUN = function(x) { sum(estimates[[x]] * probs[[x]]) })

## calculate probabilities associated with each possible value that n_1 could take on
probs_n_1 <- lapply(X = 1:length(sra_z_vecs),
                    FUN = function(x) { ncol(sra_z_vecs[[x]])/ncol(all_sra_z_vecs_mat)   })

sum(unlist(probs_n_1))

sum(unlist(exp_values_est) * unlist(probs_n_1))

## or equivalently
unique(unlist(exp_values_est)) * sum(unlist(probs_n_1))




