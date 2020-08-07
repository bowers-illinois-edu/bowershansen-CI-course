setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

acorn_data <- read.csv("acorn03.csv")

sum(acorn_data$size * acorn_data$contact)/sum(acorn_data$size[which(acorn_data$z == 1)])


acorn_data <- dplyr::select(.data = acorn_data, unit, size, z, vote03)



treat_group_mean <- function(.Z,
                             .y){
  
  n_t = sum(.Z == 1)
  
  test_stat = as.numeric(n_t^{-1} * t(.Z) %*% .y)
  
  return(test_stat)
  
}

obs_test_stat <- treat_group_mean(.Z = acorn_data$z,
                                  .y = acorn_data$vote03)

round(x = obs_test_stat, digits = 4)

set.seed(1:5)
null_test_stats <- replicate(n = 10^3,
                             expr = treat_group_mean(.Z = sample(acorn_data$z),
                                                     .y = acorn_data$vote03))

null_test_stat_dist <- data.frame(null_test_stat = null_test_stats)

bw <- 2 * IQR(null_test_stats) / length(null_test_stats)^(1/3)

library(ggplot2)
ggplot(data = null_test_stat_dist,
       mapping = aes(x = null_test_stat, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = bw) +
  geom_vline(xintercept = obs_test_stat,
             linetype = "dashed") +
  xlab(label = "Null test statistic") +
  ylab(label = "Probability") +
  theme_bw()

mean(null_test_stats >= obs_test_stat)

null_dist_plot <- ggplot(data = null_test_stat_dist,
                         mapping = aes(x = null_test_stat)) +
  geom_histogram(bins = 100) +
  geom_histogram(data = subset(x = null_test_stat_dist,
                               subset = null_test_stat >= obs_test_stat),
                 colour = "black",
                 fill = "red",
                 bins = 100) +
  geom_vline(xintercept = obs_test_stat,
             color = "red",
             linetype = "dashed") +
  xlab("Null Test Statistics") +
  ylab("Count") +
  ggtitle("Distribution of Test Statistic under Sharp Null") +
  theme(plot.title = element_text(hjust = 0.5))

null_dist_plot

round(x = mean(null_test_stats), digits = 4)
round(x = sum((acorn_data$vote03 - mean(acorn_data$vote03))^2), digits = 4)

round(x = mean((null_test_stats - mean(null_test_stats))^2), digits = 4)

round(x = mean(acorn_data$vote03), digits = 4)


round(x = sqrt((1/14) * (1/2) * var(acorn_data$vote03)), digits = 4)

round(x = 
        t(acorn_data$z) %*% acorn_data$vote03, digits = 4)
      
n <- 6
n_t <- 3
apply(X = combn(x = n,
                m = n_t,
                simplify = TRUE),
      MARGIN = 2,
      FUN = function(x) as.integer(1:n %in% x))


(n_t/n) * (1/n_t)

1/n

