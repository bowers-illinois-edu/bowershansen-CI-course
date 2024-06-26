setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

acorn_data <- read.csv("acorn03.csv")

acorn_data <- dplyr::select(.data = acorn_data, unit, size, z, vote03, v_g2002)

acorn_data <- dplyr::mutate(.data = acorn_data,
                            pre_post = (vote03 - v_g2002),
                            vote03 = vote03,
                            pre_post = pre_post * 100,
                            vote03 = vote03 * 100)

#acorn_Omega <- apply(X = combn(x = 1:nrow(acorn_data),
#                               m = sum(acorn_data$z),
#                               simplify = TRUE) ,
#                     MARGIN = 2,
#                     FUN = function(x) as.integer(1:nrow(acorn_data) %in% x))

#save(acorn_Omega, file = "acorn_Omega.RData")
#load(file = "acorn_Omega.RData")
set.seed(1:5)
acorn_Omega <- replicate(n = 10^3, expr = sample(x = acorn_data$z))

null_diff_means_no_effect_y <- sapply(X = 1:ncol(acorn_Omega),
                                    FUN = function(x) { mean(acorn_data$vote03[which(acorn_Omega[,x] == 1)]) -
                                        mean(acorn_data$vote03[which(acorn_Omega[,x] == 0)]) })

null_diff_means_no_effect_y_gain <- sapply(X = 1:ncol(acorn_Omega),
                                           FUN = function(x) { mean(acorn_data$pre_post[which(acorn_Omega[,x] == 1)]) -
                                               mean(acorn_data$pre_post[which(acorn_Omega[,x] == 0)]) })


null_dist_data_no_effect <- data.frame(null_test_stat = c(null_diff_means_no_effect_y, null_diff_means_no_effect_y_gain),
                                       outcome = c(rep(x = "Original outcome", times = length(null_diff_means_no_effect_y)),
                                                   rep(x = "Rescaled outcome", times = length(null_diff_means_no_effect_y_gain))))

bw_no_effect <- 2 * IQR(null_dist_data_no_effect$null_test_stat) / length(null_dist_data_no_effect$null_test_stat)^(1/3)
library(ggplot2)
null_dist_no_effect_plot <- ggplot(data = null_dist_data_no_effect,
                                   mapping = aes(x = null_test_stat ,
                                                 y = (..count..)/sum(..count..))) +
  geom_histogram(data = null_dist_data_no_effect,
                 binwidth = bw_no_effect) +
  theme_bw() +
  xlab(label = "Null test statistic") +
  ylab(label = "Probability") +
  scale_x_continuous(limits = c(-10, 10)) +
  facet_wrap(facets = .~ outcome,
             nrow = 1,
             ncol = 2,
             scales = "fixed") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

null_dist_no_effect_plot

ggsave(plot = null_dist_no_effect_plot,
       file = "null_dist_no_effect_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)


# Blocked assignment ------------------------------------------------------

rm(list=ls())
diff_means_est <- function(.V,
                           .Z) {
  
  return(mean(.V[.Z == 1]) - mean(.V[.Z == 0])) }

y_c <- c(20, 8, 11, 10, 14, 1)
y_t <- c(22, 12, 11, 15, 18, 4)
cov <- c(1, 1, 0, 1, 1, 0)
z <- c(1, 1, 1, 0, 0, 0)
n <- 6
n_t <- 3

### cov = 0 block
y_c_cov_0 <- y_c[which(cov == 0)]
mean_y_c_cov_0 <- mean(y_c_cov_0)
var_y_c_cov_0 <- mean((y_c_cov_0 - mean_y_c_cov_0)^2)
y_t_cov_0 <- y_t[which(cov == 0)]
mean_y_t_cov_0 <- mean(y_t_cov_0)
var_y_t_cov_0 <- mean((y_t_cov_0 - mean_y_t_cov_0)^2)
covar_y_c_y_t_cov_0 <- mean((y_c_cov_0 - mean_y_c_cov_0) * (y_t_cov_0 - mean_y_t_cov_0))
mean_tau_cov_0 <- mean(y_t_cov_0 - y_c_cov_0)
var_tau_cov_0 <- mean(((y_t_cov_0 - y_c_cov_0) - mean_tau_cov_0)^2)
n_cov_0 <- length(y_c[which(cov == 0)])
n_t_cov_0 <- 1
n_c_cov_0 <- 1

## cov = 1 block
y_c_cov_1 <- y_c[which(cov == 1)]
mean_y_c_cov_1 <- mean(y_c_cov_1)
var_y_c_cov_1 <- mean((y_c_cov_1 - mean_y_c_cov_1)^2)
y_t_cov_1 <- y_t[which(cov == 1)]
mean_y_t_cov_1 <- mean(y_t_cov_1)
var_y_t_cov_1 <- mean((y_t_cov_1 - mean_y_t_cov_1)^2)
covar_y_c_y_t_cov_1 <- mean((y_c_cov_1 - mean_y_c_cov_1) * (y_t_cov_1 - mean_y_t_cov_1))
mean_tau_cov_1 <- mean(y_t_cov_1 - y_c_cov_1)
var_tau_cov_1 <- mean(((y_t_cov_1 - y_c_cov_1) - mean_tau_cov_1)^2)
n_cov_1 <- length(y_c[which(cov == 1)])
n_t_cov_1 <- 2
n_c_cov_1 <- 2

Omega_cov_0 <- apply(X = combn(x = n_cov_0,
                               m = n_t_cov_0,
                               simplify = TRUE),
                     MARGIN = 2,
                     FUN = function(x) as.integer(1:n_cov_0 %in% x))

Omega_cov_1 <- apply(X = combn(x = n_cov_1,
                               m = n_t_cov_1,
                               simplify = TRUE),
                     MARGIN = 2,
                     FUN = function(x) as.integer(1:n_cov_1 %in% x))

Omega <- apply(X = combn(x = n,
                         m = n_t,
                         simplify = TRUE),
               MARGIN = 2,
               FUN = function(x) as.integer(1:n %in% x))

block_Omega <- Omega[,apply(X = Omega,
                            MARGIN = 2, 
                            FUN = function(x) { 
                              all(do.call(what = "c",
                                          args = lapply(X = split(x = x,
                                                                  f = cov),
                                                        FUN = sum)) == 
                                    do.call(what = "c",
                                            args = lapply(X = split(x = z,
                                                                    f = cov),
                                                          FUN = sum))) })]

obs_pot_outs_cov_0 <- sapply(X = 1:ncol(block_Omega),
                             FUN = function(x) { y_t[which(cov == 0)] * block_Omega[which(cov == 0), x] + y_c[which(cov == 0)] * (1 - block_Omega[which(cov == 0), x]) })

obs_pot_outs_cov_1 <- sapply(X = 1:ncol(block_Omega),
                             FUN = function(x) { y_t[which(cov == 1)] * block_Omega[which(cov == 1), x] + y_c[which(cov == 1)] * (1 - block_Omega[which(cov == 1), x]) })

diff_means_ests_cov_0 <- sapply(X = 1:ncol(block_Omega),
                                FUN = function(x) { diff_means_est(.V = obs_pot_outs_cov_0[,x],
                                                                   .Z = block_Omega[which(cov == 0),x]) })

diff_means_est_mean_cov_0 <- mean(diff_means_ests_cov_0)
var_diff_means_est_mean_cov_0 <- mean((diff_means_ests_cov_0 - diff_means_est_mean_cov_0)^2)

diff_means_ests_cov_1 <- sapply(X = 1:ncol(block_Omega),
                                FUN = function(x) { diff_means_est(.V = obs_pot_outs_cov_1[,x],
                                                                   .Z = block_Omega[which(cov == 1), x]) })
diff_means_est_mean_cov_1 <- mean(diff_means_ests_cov_1)
var_diff_means_est_mean_cov_1 <- mean((diff_means_ests_cov_1 - diff_means_est_mean_cov_1)^2)

overall_block_diff_means_ests <- sapply(X = 1:ncol(block_Omega),
                                        FUN = function(x) { (diff_means_ests_cov_0[x] * (n_cov_0/n)) + (diff_means_ests_cov_1[x] * n_cov_1/n) })

mean(overall_block_diff_means_ests)
mean((overall_block_diff_means_ests - mean(overall_block_diff_means_ests))^2)


## Imbens Rubin, p. 275
var_diff_means_est_mean_cov_0
var_diff_means_est_mean_cov_1


################################

obs_pot_outs <- sapply(X = 1:ncol(Omega),
                       FUN = function(x) { y_t * Omega[,x] + y_c * (1 - Omega[,x]) })

obs_test_stats <- sapply(X = 1:ncol(Omega),
                         FUN = function(x) { diff_means_est(.V = obs_pot_outs[,x],
                                                            .Z = Omega[,x]) })

intersect(Omega, block_Omega)

matches <- list()

for(i in 1:ncol(Omega)){
  
  matches[[i]] = sapply(X = 1:ncol(block_Omega),
                        FUN = function(x) { all.equal(block_Omega[,x], Omega[,i]) })
}

excluded_assigns <- which(!sapply(X = 1:length(matches), FUN = function(x) { "TRUE" %in% matches[[x]] }))
included_assigns <- which(sapply(X = 1:length(matches), FUN = function(x) { "TRUE" %in% matches[[x]] }))

plot_data <- data.frame(z = rep(x = 1:20, times = 2),
                        est = rep(x = obs_test_stats, times = 2),
                        prob = c(rep(x = (1/20), times = 20), rep(x = NA, times = 20)),
                        block = factor(x = c(rep(x = "Unblocked", times = 20), rep(x = "Blocked", times = 20)),
                                       levels = c("Unblocked", "Blocked")),
                        excluded = 0)

plot_data$prob[which(plot_data$z %in% excluded_assigns & plot_data$block == "Blocked")] <- 0
plot_data$prob[which(plot_data$z %in% included_assigns & plot_data$block == "Blocked")] <- (1/12)
plot_data$excluded[which(plot_data$z %in% excluded_assigns)] <- 1

plot_data %<>% mutate(excluded = ifelse(test = excluded == 1, yes = "Yes", no = "No"),
                      excluded = as.factor(excluded))

blocked_assign_plot <- ggplot(data = plot_data,
                              mapping = aes(x = est,
                                            y = prob,
                                            fill = excluded)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("grey", "black")) +
  geom_vline(xintercept = mean(plot_data$est),
             linetype = "dashed") +
  ylim(0, 0.20) +
  scale_x_continuous(limits = c(-15, 15), breaks = c(-10, 0, 10)) +
  labs(fill = "Excluded assignment") +
  xlab(label = "Difference-in-Means estimates") +
  ylab(label = "Probability") +
  theme_bw() +
  theme(legend.title = element_text(size = 8)) +
  facet_wrap(facets =  ~ block,
             nrow = 2,
             ncol = 1)

ggsave(plot = blocked_assign_plot,
       file = "blocked_assign_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

y_0 <- c(24, 8, 14, 12, 10, 13)
y_1 <- y_0 + 5
cov <- c(1, 1, 0, 1, 1, 0)
z <- c(1, 1, 1, 0, 0, 0)
n <- 6
n_1 <- 3


obs_pot_outs <- sapply(X = 1:ncol(Omega),
                       FUN = function(x) { y_1 * Omega[,x] + y_0 * (1 - Omega[,x]) })

obs_test_stats <- sapply(X = 1:ncol(Omega),
                         FUN = function(x) { diff_means_est(.V = obs_pot_outs[,x],
                                                            .Z = Omega[,x]) })
mean((obs_test_stats - mean(obs_test_stats))^2)
mean((obs_test_stats - mean(obs_test_stats))^2)

obs_pot_outs_cov_0 <- sapply(X = 1:ncol(block_Omega),
                             FUN = function(x) { y_1[which(cov == 0)] * block_Omega[which(cov == 0), x] + y_0[which(cov == 0)] * (1 - block_Omega[which(cov == 0), x]) })

obs_pot_outs_cov_1 <- sapply(X = 1:ncol(block_Omega),
                             FUN = function(x) { y_1[which(cov == 1)] * block_Omega[which(cov == 1), x] + y_0[which(cov == 1)] * (1 - block_Omega[which(cov == 1), x]) })

diff_means_ests_cov_0 <- sapply(X = 1:ncol(block_Omega),
                                FUN = function(x) { diff_means_est(.V = obs_pot_outs_cov_0[,x],
                                                                   .Z = block_Omega[which(cov == 0),x]) })
diff_means_ests_cov_1 <- sapply(X = 1:ncol(block_Omega),
                                FUN = function(x) { diff_means_est(.V = obs_pot_outs_cov_1[,x],
                                                                   .Z = block_Omega[which(cov == 1), x]) })
overall_block_diff_means_ests <- sapply(X = 1:ncol(block_Omega),
                                        FUN = function(x) { (diff_means_ests_cov_0[x] * (n_cov_0/n)) + (diff_means_ests_cov_1[x] * n_cov_1/n) })

mean((overall_block_diff_means_ests - mean(overall_block_diff_means_ests))^2)



# Regression --------------------------------------------------------------

library(fabricatr)
library(randomizr)
data <- fabricate(N = 40,
                  x1 = rnorm(N, mean = 2.3),
                  x2 = rpois(N, lambda = 2),
                  x3 = runif(N),
                  y0 = rnorm(N) + x1,
                  y1 = rnorm(N) + x1 + 0.35)
data$z <- complete_ra(N = nrow(data))
data$y <- ifelse(test = data$z == 1,
                 yes = data$y1,
                 no = data$y0)

unadj_mod <- lm(formula = y ~ z,
                data = data)

library(estimatr)
lm_lin(y ~ z, covariates = ~ x1 + x2, data = data)

library(tidyverse)
data <- mutate(.data = data,
               x1_cent = x1 - mean(x1),
               x2_cent = x2 - mean(x2),
               x3_cent = x3 - mean(x3))

lin_mod <- lm(formula = y ~ z + x1_cent + x2_cent + z * x1_cent + z * x2_cent,
              data = data)

library(sandwich)

diag(vcovHC(x = unadj_mod, type = "HC2"))["z"]

var(data$y[data$z == 1])/sum(data$z) + var(data$y[data$z == 0])/sum(1 - data$z)








