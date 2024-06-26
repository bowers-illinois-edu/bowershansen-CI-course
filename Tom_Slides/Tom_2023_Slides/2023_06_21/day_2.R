##' ---
##' output: github_document
##' ---
##' # Setup -------------------------------------------------------------------

##' <!-- Run this file using `rmarkdown::render()` (or `knitr::spin()`) -->
##+ eval=TRUE, echo=FALSE
if (!exists("saveplots_")) saveplots_ <- FALSE

##' <!-- If running interactively in RStudio, may set this eval to TRUE -> 
##+ eval=FALSE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

##+
acorn_data <- read.csv("../../data/acorn03.csv")

acorn_data <- dplyr::select(.data = acorn_data, unit, size, z, vote03, v_g2002)

acorn_data <- dplyr::mutate(.data = acorn_data,
                            pre_post = (vote03 - v_g2002),
                            vote03 = vote03,
                            pre_post = pre_post * 100,
                            vote03 = vote03 * 100)
## Generate all assignments; too computationaly intensive, although possible
#acorn_Omega <- apply(X = combn(x = 1:nrow(acorn_data),
#                               m = sum(acorn_data$z),
#                               simplify = TRUE) ,
#                     MARGIN = 2,
#                     FUN = function(x) as.integer(1:nrow(acorn_data) %in% x))

#save(acorn_Omega, file = "acorn_Omega.RData")
#load(file = "acorn_Omega.RData")

## Instead randomly sample from all possible assignments
set.seed(1:5)
acorn_Omega <- replicate(n = 10^3, expr = sample(x = acorn_data$z))

## Calculate observed difference-in-means
obs_test_stat <- mean(acorn_data$vote03[which(acorn_data$z == 1)]) - mean(acorn_data$vote03[which(acorn_data$z == 0)])

## Calculate difference-in-means under sharp null of no effect
null_diff_means_no_effect <- sapply(X = 1:ncol(acorn_Omega),
                                    FUN = function(x) { mean(acorn_data$vote03[which(acorn_Omega[,x] == 1)]) -
                                        mean(acorn_data$vote03[which(acorn_Omega[,x] == 0)]) })

null_dist_data_no_effect <- data.frame(null_test_stat = null_diff_means_no_effect)

null_dist_data_no_effect <- dplyr::mutate(.data = null_dist_data_no_effect,
                                          greater_obs = ifelse(test = null_test_stat >= obs_test_stat,
                                                               yes = "yes",
                                                               no = "no"))

bw_no_effect <- 2 * IQR(null_dist_data_no_effect$null_test_stat) / length(null_dist_data_no_effect$null_test_stat)^(1/3)
library(ggplot2)
null_dist_no_effect_plot <- ggplot(data = null_dist_data_no_effect,
                                   mapping = aes(x = null_test_stat ,
                                                 y = (..count..)/sum(..count..),
                                                 fill = greater_obs)) +
  geom_histogram(data = null_dist_data_no_effect,
                 binwidth = bw_no_effect) +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  scale_fill_manual(values = c("grey", "black")) +
  guides(fill = "none") +
  theme_bw() +
  xlab(label = "Null test statistics") +
  ylab(label = "Probability")

##+ eval=saveplots_
ggsave(plot = null_dist_no_effect_plot,
       file = "null_dist_no_effect_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

################################################################################################################################################################################

## sharp null hypothesis for every unit
##'
tau_h <- .025 * 100

## Adjusted outcomes
y_c_unif <- acorn_data$vote03 - acorn_data$z * tau_h

## Observed test-stat on adjusted outcomes
unif_obs_test_stat <- mean(y_c_unif[which(acorn_data$z == 1)]) - mean(y_c_unif[which(acorn_data$z == 0)])

## Diff-in-Means over all assignments holding adjusted outcomes fixed
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

##+ eval=saveplots_
ggsave(plot = null_unif_plot,
       file = "null_unif_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

## Null hypotheses we will test
##+ 
nulls <- seq(from = -0.05, to = .15, by = .001) * 100

## Function to calculate p-values (lower and upper) for each null hypothesis
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

## Upper p-values for each null hypothesis
upper_p_values <- sapply(X = nulls,
                         FUN = function(x) { unif_hyp_tests(.obs_y = acorn_data$vote03,
                                                            .z = acorn_data$z,
                                                            .null_hyp = x,
                                                            .Omega = acorn_Omega)$upper_p_value })

## Null hypotheses we fail to reject (i.e., null hypotheses with upper p-value above 0.05)
upper_conf_set <- nulls[which(upper_p_values >= 0.05)]

## Lower p-values for each null hypothesis
lower_p_values <- sapply(X = nulls,
                         FUN = function(x) { unif_hyp_tests(.obs_y = acorn_data$vote03,
                                                            .z = acorn_data$z,
                                                            .null_hyp = x,
                                                            .Omega = acorn_Omega)$lower_p_value })

## Null hypotheses we fail to reject (i.e., null hypotheses with lower p-value above 0.05)
lower_conf_set <- nulls[which(lower_p_values >= 0.05)]

## Collection of nulls we fail to reject for lower and upper tests
two_sided_conf_set <- intersect(lower_conf_set, upper_conf_set)
