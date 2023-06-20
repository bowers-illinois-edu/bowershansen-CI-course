# Setup -------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ## set wd to where file is saved
rm(list = ls())

# Fisher's Lady Tasting Tea experiment ------------------------------------

## Total of N = 8 cups
N <- 8

## Generate all 2^8 = 256 possible assignments
expand.grid(replicate(n = N,
                      expr = 0:1,
                      simplify = FALSE))

## To generate set of all allowable assignments, set number of milk-first (treated)
## cups to n_1 = 4
n_1 <- 4
n_0 <- N - n_1 ## Remaining N - n_1 cups are tea-first (control) cups

## Generate set of allowable assignments
Omega <- apply(X = combn(x = N,
                         m = n_1,
                         simplify = TRUE),
               MARGIN = 2,
               FUN = function(x) as.integer(1:N %in% x))
## Each assignment has equal probability
Omega_probs <- rep(x = 1/ncol(Omega),
                   times = ncol(Omega))

## Suppose realized assignment is
z <- Omega[,1]

## Also suppose "lady" correctly judges all cups
y <- z

## Write function to calculate Fisher's test statistic 
fisher_test_stat <- function(z,
                             y){
  
  return(as.numeric(t(z) %*% y))
  
}

## Calculate test-statisic on observed data
obs_test_stat <- fisher_test_stat(z = z,
                                  y = y)

## Generate distribution over Omega holding y fixed
test_stats_no_disc <- apply(X = Omega,
                            MARGIN = 2,
                            FUN = function(x) { fisher_test_stat(z = x,
                                                                 y = y) })

space_test_stats_no_disc <- sort(unique(test_stats_no_disc))
probs_no_disc <- sapply(X = space_test_stats_no_disc,
                        FUN = function(x) { sum((test_stats_no_disc == x) * Omega_probs) })

## Plot distribution of null test-stat

## Use ggplot2 and all tidyverse packages
# install.packages("tidyverse")
library(tidyverse)

null_test_stats <- data.frame(null_test_stat = space_test_stats_no_disc,
                              prob = probs_no_disc,
                              null = "No discrimination")

## Use Freedman-Diaconis rule for binwidth
#bw <- 2 * IQR(null_test_stats$null_test_stat) / length(null_test_stats$null_test_stat)^(1/3)

null_dist_no_discrim_plot <- ggplot(data = null_test_stats,
                                    mapping = aes(x = null_test_stat ,
                                                  y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  theme_bw() +
  xlab(label = "Test statistic") +
  ylab(label = "Probability")
null_dist_no_discrim_plot

#ggsave(plot = null_dist_no_discrim_plot,
#       file = "null_dist_no_discrim_plot.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)

test_stats_perf_disc <- apply(X = Omega,
                              MARGIN = 2,
                              FUN = function(x) { fisher_test_stat(z = x,
                                                                   y = x) })

space_test_stats_perf_disc <- sort(unique(test_stats_perf_disc))
probs_perf_disc <- sapply(X = space_test_stats_perf_disc,
                          FUN = function(x) { sum((test_stats_perf_disc == x) * Omega_probs) })


null_test_stats <- data.frame(null_test_stat = c(space_test_stats_no_disc,
                                                 space_test_stats_perf_disc),
                              prob = c(probs_no_disc, probs_perf_disc),
                              null = c(rep(x = "No discrimination",
                                           times = length(space_test_stats_no_disc)),
                                       rep(x = "Perfect discrimination",
                                           times = length(space_test_stats_perf_disc))))

null_dists_discrim_plot <- ggplot(data = null_test_stats,
                                    mapping = aes(x = null_test_stat ,
                                                  y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = "Test statistic") +
  ylab(label = "Probability") +
  facet_wrap(facets = . ~ null,
             nrow = 2,
             ncol = 1,
             scales = "fixed")
null_dists_discrim_plot

#ggsave(plot = null_dists_discrim_plot,
#       file = "null_dists_discrim_plot.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)




