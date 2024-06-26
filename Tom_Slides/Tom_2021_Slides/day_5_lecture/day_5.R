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
                                       outcome = c(rep(x = "Outcome", times = length(null_diff_means_no_effect_y)),
                                                   rep(x = "Rescaled outcome", times = length(null_diff_means_no_effect_y_gain))))

bw_no_effect <- 2 * IQR(null_dist_data_no_effect$null_test_stat) / length(null_dist_data_no_effect$null_test_stat)^(1/3)
library(ggplot2)
null_dist_no_effect_plot <- ggplot(data = null_dist_data_no_effect,
                                   mapping = aes(x = null_test_stat ,
                                                 y = (..count..)/sum(..count..))) +
  geom_histogram(data = null_dist_data_no_effect,
                 binwidth = bw_no_effect) +
  theme_bw() +
  xlab(label = "Null test statistics") +
  ylab(label = "Probability") +
  facet_wrap(facets = .~ outcome,
             nrow = 1,
             ncol = 2,
             scales = "free") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(plot = null_dist_no_effect_plot,
       file = "null_dist_no_effect_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

################################################################################################################################################################################

tau_h <- .025 * 100

y_c_unif <- acorn_data$vote03 - acorn_data$z * tau_h

unif_obs_test_stat <- mean(y_c_unif[which(acorn_data$z == 1)]) - mean(y_c_unif[which(acorn_data$z == 0)])

null_unif_diff_means <- sapply(X = 1:ncol(acorn_Omega),
                               FUN = function(x) { mean(y_c_unif[which(acorn_Omega[,x] == 1)]) -
                                   mean(y_c_unif[which(acorn_Omega[,x] == 0)]) })

mean(null_unif_diff_means >= unif_obs_test_stat)

null_dist_data <- data.frame(null_test_stat = null_unif_diff_means,
                             greater_obs = ifelse(test = null_unif_diff_means >= unif_obs_test_stat,
                                                  yes = "yes",
                                                  no = "no"))

bw <- 2 * IQR(null_dist_data$null_test_stat) / length(null_dist_data$null_test_stat)^(1/3)

null_unif_plot <- ggplot(data = null_dist_data,
                         mapping = aes(x = null_test_stat,
                                       y = (..count..)/sum(..count..),
                                       fill = greater_obs)) +
  geom_histogram(data = null_dist_data,
                 binwidth = bw) +
  geom_vline(mapping = aes(xintercept = unif_obs_test_stat)) +
  scale_fill_manual(values = c("grey", "black")) +
  guides(fill = "none") +
  theme_bw() +
  xlab(label = "Null test statistic") +
  ylab(label = "Probability")

#ggsave(plot = null_unif_plot,
#       file = "null_unif_plot.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)

## Generate confidence set
nulls <- seq(from = -0.05, to = .15, by = .001) * 100

unif_hyp_tests <- function(.obs_y,
                           .z,
                           .null_hyp,
                           .Omega){
  
  y_unif = .obs_y - .z * .null_hyp
  
  obs_stat = mean(y_unif[which(.z == 1)]) - mean(y_unif[which(.z == 0)])
  
  null_stats = sapply(X = 1:ncol(.Omega),
                      FUN = function(x) { mean(y_unif[which(.Omega[,x] == 1)]) -
                          mean(y_unif[which(.Omega[,x] == 0)]) })
  
  lower_p_value = mean(null_stats <= obs_stat)
  upper_p_value = mean(null_stats >= obs_stat)
  
  return(list("lower_p_value" = lower_p_value,
              "upper_p_value" = upper_p_value))
  
}

upper_p_values <- sapply(X = nulls,
                         FUN = function(x) { unif_hyp_tests(.obs_y = acorn_data$vote03,
                                                            .z = acorn_data$z,
                                                            .null_hyp = x,
                                                            .Omega = acorn_Omega)$upper_p_value })

upper_conf_set <- nulls[which(upper_p_values >= 0.05)]

lower_p_values <- sapply(X = nulls,
                         FUN = function(x) { unif_hyp_tests(.obs_y = acorn_data$vote03,
                                                            .z = acorn_data$z,
                                                            .null_hyp = x,
                                                            .Omega = acorn_Omega)$lower_p_value })

lower_conf_set <- nulls[which(lower_p_values >= 0.05)]

two_sided_conf_set <- intersect(lower_conf_set, upper_conf_set)
