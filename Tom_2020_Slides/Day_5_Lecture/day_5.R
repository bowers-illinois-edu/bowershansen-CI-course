setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

acorn_data <- read.csv("acorn03.csv")

acorn_data <- dplyr::select(.data = acorn_data, unit, size, z, vote03)

#acorn_Omega <- apply(X = combn(x = 1:nrow(acorn_data),
#                               m = sum(acorn_data$z),
#                               simplify = TRUE) ,
#                     MARGIN = 2,
#                     FUN = function(x) as.integer(1:nrow(acorn_data) %in% x))

#save(acorn_Omega, file = "acorn_Omega.RData")
#load(file = "acorn_Omega.RData")
set.seed(1:5)
acorn_Omega <- replicate(n = 10^3, expr = sample(x = acorn_data$z))


obs_test_stat <- mean(acorn_data$vote03[which(acorn_data$z == 1)]) - mean(acorn_data$vote03[which(acorn_data$z == 0)])

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
  guides(fill = FALSE) +
  theme_bw() +
  xlab(label = "Null test statistics") +
  ylab(label = "Probability")

ggsave(plot = null_dist_no_effect_plot,
       file = "null_dist_no_effect_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

## How would we test another sharp null other than 0 
## Let's test tau = .05 for all 28 precincts
tau <- 0.05
y_c_null <- acorn_data$vote03 - acorn_data$z * tau
y_t_null <- acorn_data$vote03 + (1 - acorn_data$z) * tau

null_obs_outs <- sapply(X = 1:ncol(acorn_Omega),
                        FUN = function(x) { acorn_Omega[,x] * y_t_null + (1 - acorn_Omega[,x]) * y_c_null  })

cbind(acorn_data$z, null_obs_outs[,2])

################################################################################################################################################################################
cbind(acorn_data$z, acorn_data$vote03)

p <- rep(x = .02, times = nrow(acorn_data))

y_c_null <- acorn_data$vote03 - acorn_data$z * p

y_t_null <- acorn_data$vote03 + (1 - acorn_data$z) * p

cbind(y_c_null, y_t_null)

acorn_Omega <- replicate(n = 10^3, expr = sample(acorn_data$z))


null_obs_outs <- sapply(X = 1:ncol(acorn_Omega),
                   FUN = function(x) { acorn_Omega[,x] * y_t_null + (1 - acorn_Omega[,x]) * y_c_null })
dim(null_obs_outs)


null_obs_outs[,c(1:5)]

null_test_stats <- sapply(X = 1:ncol(acorn_Omega),
                          FUN = function(x) { mean(null_obs_outs[,x][which(acorn_Omega[,x] == 1)]) -
                              mean(null_obs_outs[,x][which(acorn_Omega[,x] == 0)]) })


mean(null_test_stats >= obs_test_stat)

################################################################################################################################################################################




null_diff_means <- sapply(X = 1:ncol(acorn_Omega),
                          FUN = function(x) { mean(null_obs_outs[,x][which(acorn_Omega[,x] == 1)]) -
                              mean(null_obs_outs[,x][which(acorn_Omega[,x] == 0)]) })


y_c_unif <- acorn_data$vote03 - acorn_data$z * tau

unif_obs_test_stat <- mean(y_c_unif[which(acorn_data$z == 1)]) - mean(y_c_unif[which(acorn_data$z == 0)])

null_unif_diff_means <- sapply(X = 1:ncol(acorn_Omega),
                               FUN = function(x) { mean(y_c_unif[which(acorn_Omega[,x] == 1)]) -
                                   mean(y_c_unif[which(acorn_Omega[,x] == 0)]) })

mean(null_unif_diff_means >= unif_obs_test_stat)

null_dist_data <- data.frame(null_test_stat = c(null_diff_means, null_unif_diff_means),
                             unif = rep(x = c("Non uniformity trial", "Uniformity trial"),
                                        each = ncol(acorn_Omega)))

levels(null_dist_data$unif) <- c("Non uniformity trial",
                                 "Uniformity trial")

vline_data <- data.frame(unif = levels(null_dist_data$unif),
                         vl = c(obs_test_stat, unif_obs_test_stat)) 

bw <- 2 * IQR(null_dist_data$null_test_stat) / length(null_dist_data$null_test_stat)^(1/3)

null_plots <- ggplot(data = null_dist_data,
                     mapping = aes(x = null_test_stat,
                                   y = (..count..)/sum(..count..))) +
  geom_histogram(data = subset(x = null_dist_data,
                               subset = unif == "Non uniformity trial"),
                 binwidth = bw) +
  geom_histogram(data = subset(x = null_dist_data,
                               subset = unif == "Uniformity trial"),
                 binwidth = bw) +
  geom_vline(mapping = aes(xintercept = vl),
             linetype = "dashed",
             data = subset(x = vline_data,
                           subset = unif == "Non uniformity trial")) +
  geom_vline(mapping = aes(xintercept = vl),
             linetype = "dashed",
             data = subset(x = vline_data,
                           subset = unif == "Uniformity trial")) +
  theme_bw() +
  xlab(label = "Null test statistic") +
  ylab(label = "Probability") +
  theme(axis.text.x = element_text(size = 7.5),
        axis.text.y = element_text(size = 7.5)) +
  facet_wrap(facets = .~ unif,
             nrow = 1,
             ncol = 2) 

ggsave(plot = null_plots,
       file = "null_plots.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)


bw <- 2 * IQR(null_dist_data$null_test_stat[which(null_dist_data$unif == "Non uniformity trial")]) / length(null_dist_data$null_test_stat[which(null_dist_data$unif == "Non uniformity trial")])^(1/3)
null_dist_plot <- ggplot(data = subset(x = null_dist_data,
                                       subset = unif == "Non uniformity trial"),
                         mapping = aes(x = null_test_stat ,
                                       y = (..count..)/sum(..count..))) +
  geom_histogram(data = subset(x = null_dist_data,
                               subset = unif == "Non uniformity trial"),
                 binwidth = bw) +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  guides(fill = FALSE) +
  theme_bw() +
  xlab(label = "Null test statistics") +
  ylab(label = "Probability")

ggsave(plot = null_dist_plot,
       file = "null_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

## Hodes-Lehmann estimate
HL_y_c_unif <- acorn_data$vote03 - acorn_data$z * obs_test_stat

HL_unif_obs_test_stat <- mean(HL_y_c_unif[which(acorn_data$z == 1)]) - mean(HL_y_c_unif[which(acorn_data$z == 0)])

HL_null_unif_diff_means <- sapply(X = 1:ncol(acorn_Omega),
                                  FUN = function(x) { mean(HL_y_c_unif[which(acorn_Omega[,x] == 1)]) -
                                      mean(HL_y_c_unif[which(acorn_Omega[,x] == 0)]) })

HL_null_dist_data <- data.frame(null_test_stat = HL_null_unif_diff_means)

bw <- 2 * IQR(HL_null_dist_data$null_test_stat) / length(HL_null_dist_data$null_test_stat)^(1/3)

HL_null_dist_plot <- ggplot(data = HL_null_dist_data,
                         mapping = aes(x = null_test_stat ,
                                       y = (..count..)/sum(..count..))) +
  geom_histogram(data = HL_null_dist_data,
                 binwidth = bw) +
  geom_vline(xintercept = HL_unif_obs_test_stat,
             linetype = "dashed") +
  guides(fill = FALSE) +
  theme_bw() +
  xlab(label = "Null test statistics") +
  ylab(label = "Probability")

ggsave(plot = HL_null_dist_plot,
       file = "HL_null_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

###################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################
## Generate confidence set
nulls <- seq(from = -0.05, to = .15, by = .001)

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

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################

## Assess power of test of sharp null when true effect is 0.025
y_t <- acorn_data$vote03 + (1 - acorn_data$z) * 0
y_c <- acorn_data$vote03 - acorn_data$z * 0

cbind(y_t, y_c)

obs_outs <- sapply(X = 1:ncol(acorn_Omega),
                   FUN = function(x) { acorn_Omega[,x] * y_t + (1 - acorn_Omega[,x]) * y_c })

obs_test_stats <- sapply(X = 1:ncol(acorn_Omega),
                         FUN = function(x) { mean(obs_outs[,x][which(acorn_Omega[,x] == 1)]) -
                             mean(obs_outs[,x][which(acorn_Omega[,x] == 0)]) })

null_dists <- list()


sapply(X = 1:ncol(acorn_Omega),
       FUN = function(x) { mean(obs_outs[,1][which(acorn_Omega[,x] == 1)]) -
           mean(obs_outs[,1][which(acorn_Omega[,x] == 0)]) })


mean(obs_outs[,][which(acorn_Omega[,3] == 1)]) -
  mean(obs_outs[,1][which(acorn_Omega[,3] == 0)])


for(i in 1:ncol(obs_outs)){
  
  null_dists[[i]] = sapply(X = 1:ncol(acorn_Omega),
                           FUN = function(x) { mean(obs_outs[,i][which(acorn_Omega[,x] == 1)]) -
                               mean(obs_outs[,i][which(acorn_Omega[,x] == 0)]) })
  }

upper_p_values <- sapply(X = 1:ncol(acorn_Omega),
                         FUN = function(x) { mean(null_dists[[x]] >= obs_test_stats[x]) })

## power
mean(upper_p_values <= 0.05)


