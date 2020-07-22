setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

## Complete random assignment
n <- 8
n_1_cra <- 4

## Don't worry about understanding this line of code
## Just note that it generates all 70 possible assignments
Omega_cra <- apply(X = combn(x = n,
                             m = n_1_cra,
                             simplify = TRUE),
                   MARGIN = 2,
                   FUN = function(x) as.integer(1:n %in% x))

probs_cra <- rep(x = 1/ncol(Omega_cra), times = ncol(Omega_cra))

## Simple random assignment
## n_1 can now range from 0 to 8
n_1_sra <- 0:8

## Again, don't worry about the code to follow
Omega_sra <- lapply(X = n_1_sra,
                    FUN = function(x) { apply(X = combn(x = n,
                                                        m = x,
                                                        simplify = TRUE),
                                              MARGIN = 2,
                                              FUN = function(x) as.integer(1:n %in% x)) })

Omega_sra <- do.call(what = "cbind", args = Omega_sra)
         
probs_sra <- rep(x = 1/ncol(Omega_sra), times = ncol(Omega_sra))


## Now calculate p-value under complete random assignments (CRA)
z_obs <- Omega_cra[,64]
y_obs <- c(0, 0, 1, 0, 1, 0, 1, 1)

obs_test_stat <- t(z_obs) %*% y_obs

null_test_stats <- sapply(X = 1:ncol(Omega_cra),
                          FUN = function(x) { t(Omega_cra[,x]) %*% y_obs })

null_dist_probs <- sapply(X = 0:4,
                          FUN = function(x) { sum((null_test_stats == x) * probs_cra) })

null_dist <- data.frame(null_test_stat = 0:4,
                        prob = null_dist_probs)

library(ggplot2)
null_dist_plot <- ggplot(data = null_dist,
                         mapping = aes(x = null_test_stat,
                                       y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 4,
             linetype = "dashed") +
  theme_bw() +
  xlab(label = "Null test Statistics") +
  ylab(label = "Probability")

null_dist_plot

ggsave(plot = null_dist_plot,
       file = "null_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

## what if we assumed alternative of perfect discrimination
alt_test_stats <- sapply(X = 1:ncol(Omega_cra),
                         FUN = function(x) { t(Omega_cra[,x]) %*% Omega_cra[,x] })

alt_dist_probs <- sapply(X = 0:4,
                         FUN = function(x) { sum((alt_test_stats == x) * probs_cra) })

alt_dist <- data.frame(alt_test_stat = 0:4,
                       prob = alt_dist_probs)

library(ggplot2)
alt_dist_plot <- ggplot(data = alt_dist,
                        mapping = aes(x = alt_test_stat,
                                      y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 4,
             linetype = "dashed") +
  theme_bw() +
  xlab(label = "Null test Statistics") +
  ylab(label = "Probability")

alt_dist_plot

ggsave(plot = alt_dist_plot,
       file = "alt_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

dev.off()
