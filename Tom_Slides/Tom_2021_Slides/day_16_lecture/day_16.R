setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
library(dplyr)
library(magrittr)
library(ggplot2)
library(latex2exp)

rm(list=ls())
options(scipen = 999)

n <- 6

n_t <- 3

treated <- combn(x = n,
                 m = n_t,
                 simplify = TRUE) 
Omega <- apply(X = treated,
               MARGIN = 2,
               FUN = function(x) as.integer(1:n %in% x))

y_c <- c(20, 8, 11, 10, 14, 1)
y_t <- c(22, 12, 11, 15, 18, 4)

obs_pot_outs <- sapply(X = 1:ncol(Omega),
                       FUN = function(x) { y_t * Omega[,x] + y_c * (1 - Omega[,x]) })

source("diff_means_est.R")

obs_test_stats <- sapply(X = 1:ncol(Omega),
                         FUN = function(x) { diff_means_est(.V = obs_pot_outs[,x],
                                                            .Z = Omega[,x]) })

### example with z_8
obs_test_stat <- obs_test_stats[8]
null_test_stats <- sapply(X = 1:ncol(Omega),
                          FUN = function(x){ diff_means_est(.V = obs_pot_outs[,8],
                                                            .Z = Omega[,x]) })
mean(null_test_stats >= obs_test_stat)
null_dist_data <- data.frame(null_test_stat = sort(null_test_stats))

null_dist_data %<>% mutate(greater_obs = ifelse(test = null_test_stat >= obs_test_stat,
                                                yes = "yes",
                                                no = "no"))

null_dist_z_8_plot <- ggplot(data = null_dist_data,
                             mapping = aes(x = null_test_stat,
                                           y = (..count..)/sum(..count..))) +
  geom_histogram(data = null_dist_data,
                 binwidth = (1/3)) +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  ylim(0, 0.20) +
  theme_bw() +
  xlab(label = "Null test statistics") +
  ylab(label = "Probability")

#ggsave(plot = null_dist_z_8_plot,
#       file = "null_dist_z_8_plot.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)

## Sensitivity Analysis
## there is a vector, u, of length n, and we want to find vector of u that
## maximizes p-value

u_combs <- lapply(X = 0:n,
                  FUN = function(x) { combn(x = n,
                                            m = x,
                                            simplify = TRUE) })

for(i in 1:length(u_combs)){
  
  u_combs[[i]] = apply(X = u_combs[[i]],
                       MARGIN = 2,
                       FUN = function(x) as.integer(1:n %in% x))
  
}
data <- data.frame(z =Omega[,8],
                   y = obs_pot_outs[,8])

data <- dplyr::arrange(.data = data, desc(y))

source("sens_probs_fun.R")

sens_Omega_probs <- sapply(X = 1:ncol(u_combs),
                           FUN = function(x) { sens_probs(.Gamma = 2,
                                                          .Omega = Omega,
                                                          .u = u_combs[,x]) })

p_values <- sapply(X = 1:ncol(sens_Omega_probs),
                   FUN = function(x) { sum((null_test_stats >= obs_test_stat) * sens_Omega_probs[,x]) })

u_combsp[,which.max(p_values)]



sens_null_dist_data <- data.frame(null_test_stat = rep(x = null_test_stats, times = 2),
                                  prob = c(rep(x = 1/ncol(Omega), times = ncol(Omega)), sens_Omega_probs[,30]),
                                  Gamma = as.factor(c(rep(x = 1, times = ncol(Omega)), rep(x = 2, times = ncol(Omega)))),
                                  greater_obs = rep(x = ifelse(test = null_test_stats >= obs_test_stat,
                                                               yes = "yes",
                                                               no = "no"), times = 2))

levels(sens_null_dist_data$Gamma) <- c(TeX('$\\Gamma = 1$'), TeX('$\\Gamma = 2$'))

sens_null_dist_z_8 <- ggplot(data = sens_null_dist_data,
                             mapping = aes(x = null_test_stat,
                                           y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  ylim(0, 0.20) +
  scale_x_continuous(limits = c(-15, 15), breaks = c(-10, 0, 10)) +
  theme_bw() +
  xlab(label = "Null test Statistics") +
  ylab(label = "Probability") +
  facet_wrap(facets =  ~ Gamma,
             labeller = label_parsed,
             nrow = 2,
             ncol = 1)

#ggsave(plot = sens_null_dist_z_8,
#       file = "sens_null_dist_z_8.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)
