library(dplyr)
library(magrittr)
library(knitr)

rm(list = ls())
n <- 8
n_1 <- 4

set.seed(1:5)
d_c <- rbinom(n = n, size = 1, prob = 0.3)
d_t <- rep(x = NA, times = length(d_c))
## HERE WE SATISFY THE AT LEAST ONE COMPLIER (NON-WEAK INSTRUMENT) ASSUMPTION
d_t[which(d_c != 1)] <- rbinom(n = length(which(d_c != 1)), size = 1, prob = 0.6)
## HERE WE SATISFY THE NO DEFIERS (MONOTONICITY) ASSUMPTION
d_t[which(d_c == 1)] <- rep(x = 1, times = length(which(d_c == 1)))
cbind(d_c, d_t)
prop_comp <- length(which(d_c == 0 & d_t == 1)) / n
prop_def <- length(which(d_c == 1 & d_t == 0)) / n
prop_at <- length(which(d_c == 1 & d_t == 1)) / n
prop_nt <- length(which(d_c == 0 & d_t == 0)) / n

## HERE WE SATISFY THE EXCLUSION RESTRICTION ASSUMPTION BY LETTING y_c = y_t FOR ALL ALWAYS-TAKERS AND NEVER-TAKERS AND
## WE ALSO SATISFY THE SUTVA ASSUMPTION BY LETTING ALL UNITS HAVE ONLY TWO POT OUTS
set.seed(1:5)
y_c <- round(x = rnorm(n = 8, mean = 20, sd = 10), digits = 0)
y_t_null_false <- rep(x = NA, times = n)
y_t_null_false[which(d_c == 0 & d_t == 1)] <- y_c[which(d_c == 0 & d_t == 1)] +
  round(x = rnorm(n = length(which(d_c == 0 & d_t == 1)),
                  mean = 10,
                  sd = 4),
        digits = 0)
y_t_null_false[!(d_c == 0 & d_t == 1)] <- y_c[!(d_c == 0 & d_t == 1)]
cbind(y_c, y_t_null_false)

true_data <- data.frame(y_t = y_t_null_false,
                        y_c = y_c,
                        d_t = d_t,
                        d_c = d_c,
                        tau = y_t_null_false - y_c)

true_data %<>% mutate(type = NA,
                      type = ifelse(test = d_c == 0 & d_t == 0, yes = "never_taker", no = type),
                      type = ifelse(test = d_c == 0 & d_t == 1, yes = "complier", no = type),
                      type = ifelse(test = d_c == 1 & d_t == 0, yes = "defier", no = type),
                      type = ifelse(test = d_c == 1 & d_t == 1, yes = "always_taker", no = type))
kable(true_data)

Omega <- apply(X = combn(x = 1:n,
                         m = n_1),
               MARGIN = 2,
               FUN = function(x) { as.integer(1:n %in% x) })

assign_vec_probs <- rep(x = (1/70), times = ncol(Omega))

set.seed(1:5)
obs_z <- Omega[,sample(x = 1:ncol(Omega), size = 1)]

#obs_ys <- apply(X = Omega, MARGIN = 2, FUN = function(x) { x * y_t_null_false + (1 - x) * y_c })

#obs_ds <- apply(X = Omega, MARGIN = 2, FUN = function(x) { x * d_t + (1 - x) * d_c }) 

obs_y <- obs_z * true_data$y_t + (1 - obs_z) * true_data$y_c
obs_d <- obs_z * true_data$d_t + (1 - obs_z) * true_data$d_c

cbind(obs_z, obs_d, obs_y)

tau <- 0

null_y_c <- obs_y - obs_d * tau
null_y_t <- obs_y + (1 - obs_d) * tau

obs_diff_means <- as.numeric((t(obs_z) %*% obs_y) / (t(obs_z) %*% obs_z) -
                               (t(1 - obs_z) %*% obs_y) / (t(1 - obs_z) %*% (1 - obs_z)))

coef(lm(formula = obs_y ~ obs_z))[["obs_z"]]

obs_null_pot_outs <- sapply(X = 1:ncol(Omega),
                            FUN = function(x) { Omega[,x] * null_y_t + (1 - Omega[,x]) * null_y_c })

null_test_stat_dist <- sapply(X = 1:ncol(Omega),
                              FUN = function(x) { mean(obs_null_pot_outs[,x][which(Omega[,x] == 1)]) -
                                  mean(obs_null_pot_outs[,x][which(Omega[,x] == 0)])})

null_test_stats_data <- data.frame(null_test_stat = null_test_stat_dist,
                                   prob = assign_vec_probs)
library(ggplot2)
null_dist_plot <- ggplot(data = null_test_stats_data,
                         mapping = aes(x = null_test_stat,
                                       y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = obs_diff_means,
             color = "red",
             linetype = "dashed") +
  xlab(label = "Null Test Statistics") +
  ylab(label = "Probability")

ggsave(plot = null_dist_plot,
       file = "null_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)