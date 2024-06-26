setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

## Complete random assignment
n <- 7
n_1_cra <- 2

y_C <- c(10, 15, 20, 20, 10, 15, 15)
y_T <- c(15, 15, 30, 15, 20, 15, 30)
tau <- y_T - y_C
ATE <- mean(tau)

round(x = mean((y_C - mean(y_C)) * (y_T - mean(y_T))), digits = 2)

round(x = mean((y_C - mean(y_C))^2), digits = 2)

round(x = mean((y_T - mean(y_T))^2), digits = 2)

## Don't worry about understanding this line of code
## Just note that it generates all 21 possible assignments
Omega_cra <- apply(X = combn(x = n,
                             m = n_1_cra,
                             simplify = TRUE),
                   MARGIN = 2,
                   FUN = function(x) as.integer(1:n %in% x))

## Assumes uniform random assignment
probs_cra <- rep(x = 1/ncol(Omega_cra), times = ncol(Omega_cra))

## generate observed outcome for each of 21 possible assignments
obs_outs_cra <- sapply(X = 1:ncol(Omega_cra),
                       FUN = function(x) Omega_cra[,x] * y_T + (1 - Omega_cra[,x]) * y_C )

## calculate estimate for each of 21 possible realizations of data
ests_cra <- sapply(X = 1:ncol(Omega_cra),
               FUN = function(x) { mean(obs_outs_cra[,x][which(Omega_cra[,x] == 1)]) - mean(obs_outs_cra[,x][which(Omega_cra[,x] == 0)]) })

obs_outs_cra[,1]
Omega_cra[,1]


## unbiased
ests_cra_EV <- sum(ests_cra * probs_cra)

ests_cra_Var <- mean((ests_cra - ests_cra_EV)^2)

round(x = ests_cra_Var, digits = 2)

## analytic expression for variance
var_y_C <- mean((y_C - mean(y_C))^2)
round(x = var_y_C, digits = 2)
var_y_T <- mean((y_T - mean(y_T))^2)
round(x = var_y_T, digits = 2)
cov_y_C_y_T <- mean((y_C - mean(y_C)) * (y_T - mean(y_T)))
round(x = cov_y_C_y_T, digits = 2)

diff_means_var <- function(.n,
                           .n_t,
                           .y_C,
                           .y_T) {
  
  
  
  var_y_C = mean((.y_C - mean(.y_C))^2)
  
  var_y_T = mean((.y_T - mean(.y_T))^2)
  
  cov_y_C_y_T = mean((.y_C - mean(.y_C)) * (.y_T - mean(.y_T)))
  
  var = (1/(.n - 1)) * ((.n_t * var_y_C) / (.n - .n_t) +
                          
                          (((.n - .n_t) * var_y_T) / .n_t) +
                          
                          (2 * cov_y_C_y_T))
  
  return(var)
  
}


true_var_diff_means_est_cra <- diff_means_var(.n = n,
                                              .n_t = n_1_cra,
                                              .y_C = y_C,
                                              .y_T = y_T)

diff_means_var_est <- function(.n,
                               .n_1,
                               .z,
                               .y) {
  
  obs_y_C = .y[which(.z == 0)]
  obs_y_T = .y[which(.z == 1)]
  
  est_mean_y_C = mean(obs_y_C)
  est_mean_y_T = mean(obs_y_T)
  
  est_var_y_C = ((.n - 1)/(.n * ((.n - .n_1) - 1))) * sum((obs_y_C - est_mean_y_C)^2)
  
  est_var_y_T = ((.n - 1)/(.n * (.n_1 - 1))) * sum((obs_y_T - est_mean_y_T)^2)
  
  return((.n/(.n - 1)) * ( (est_var_y_C/(.n - .n_1)) + (est_var_y_T/.n_1)))
  
  
}

var_ests_cra <- sapply(X = 1:ncol(Omega_cra),
                       FUN = function(x) { diff_means_var_est(.n = n,
                                                              .n_1 = n_1_cra,
                                                              .z = Omega_cra[,x],
                                                              .y = obs_outs_cra[,x]) })
var_ests_cra_EV <- sum(var_ests_cra * probs_cra)
## create dataframe for plot
cra_dist_var_est_data <- data.frame(var_est = var_ests_cra, 
                                    prob = probs_cra)

library(ggplot2)
cra_var_est_dist_plot <- ggplot(data = cra_dist_var_est_data,
                                mapping = aes(x = var_est,
                                              y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = var_ests_cra_EV,
             linetype = "dashed") +
  geom_vline(xintercept = true_var_diff_means_est_cra,
             linetype = "solid") +
  theme_bw() +
  xlab(label = "Variance estimates") +
  ylab(label = "Probability")

cra_var_est_dist_plot

ggsave(plot = cra_var_est_dist_plot,
       file = "cra_var_est_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

asymp_ests_exact <- function(.y_C,
                             .y_T,
                             .prop_T,
                             .h){
  
  y_C = c(.y_C, rep(x = .y_C, times = .h - 1))
  
  y_T = c(.y_T, rep(x = .y_T, times = .h - 1))
  
  Omega = apply(X = combn(x = length(y_C),
                          m = length(y_C) * .prop_T,
                          simplify = TRUE),
                MARGIN = 2,
                FUN = function(x) as.integer(1:(length(y_C)) %in% x))
  
  obs_pot_outs = sapply(X = 1:ncol(Omega),
                        FUN = function(x) { y_T * Omega[,x] + y_C * (1 - Omega[,x]) })
  
  diff_means_ests = sapply(X = 1:ncol(Omega),
                           FUN = function(x) { mean(obs_pot_outs[,x][Omega[,x] == 1]) -
                               mean(obs_pot_outs[,x][Omega[,x] == 0]) })
  
  return(diff_means_ests)
  
}


## Show asymptotic properties
asymp_ests_approx <- function(.y_C,
                              .y_T,
                              .prop_T,
                              .h){
  
  y_C = c(.y_C, rep(x = .y_C, times = .h - 1))
  
  y_T = c(.y_T, rep(x = .y_T, times = .h - 1))
  
  true_EV_diff_means_est = mean(y_T) - mean(y_C)
  
  true_var_diff_means_est = diff_means_var(.n = length(y_C),
                                           .n_t = length(y_C) * .prop_T,
                                           .y_C = y_C,
                                           .y_T = y_T)
  
  return(rnorm(n = 10^3,
               mean = true_EV_diff_means_est,
               sd = sqrt(true_var_diff_means_est)))
  
}

ests_plot_data <- data.frame(est = c(ests_cra,
                                     asymp_ests_exact(.y_C = y_C, .y_T = y_T, .prop_T = (2/7), .h = 3),
                                     asymp_ests_approx(.y_C = y_C, .y_T = y_T, .prop_T = (2/7), .h = 15),
                                     asymp_ests_approx(.y_C = y_C, .y_T = y_T, .prop_T = (2/7), .h = 150)),
                             N = c(rep(x = "N = 7", times = length(ests_cra)),
                                   rep(x = "N = 21", times = choose(21, 6)),
                                   rep(x = "N = 100", times = 10^3),
                                   rep(x = "N = 1000", times = 10^3)))
ests_plot_data$N <- factor(x = ests_plot_data$N, levels = c("N = 7", "N = 21", "N = 100", "N = 1000"))


asymp_ests_plot <- ggplot(data = ests_plot_data,
                              mapping = aes(x = est)) +
  geom_histogram() +
  xlab(label = "Diff-in-Means estimates") +
  ylab(label = "Probability") +
  theme_bw() +
  facet_wrap(facets = .~ N,
             nrow = 1,
             ncol = 4,
             scales = "free_y") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


#ggsave(plot = asymp_ests_plot,
#       file = "asymp_ests_plot.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)


y_C = c(.y_C, rep(x = .y_C, times = .h - 1))

y_T = c(.y_T, rep(x = .y_T, times = .h - 1))

true_EV_diff_means_est = mean(y_T) - mean(y_C)

true_var_diff_means_est = diff_means_var(.n = length(y_C),
                                         .n_t = length(y_C) * .prop_T,
                                         .y_C = y_C,
                                         .y_T = y_T)

stand_ests_plot_data <- data.frame(est = c( (ests_cra - 5) / sqrt(21.19048),
                                     (asymp_ests_exact(.y_C = y_C, .y_T = y_T, .prop_T = (2/7), .h = 3) - 5) / sqrt(6.357143),
                                     (asymp_ests_approx(.y_C = y_C, .y_T = y_T, .prop_T = (2/7), .h = 15) - 5) / sqrt(0.4049136),
                                     (asymp_ests_approx(.y_C = y_C, .y_T = y_T, .prop_T = (2/7), .h = 150) - 5) / sqrt(0.002690911)),
                             N = c(rep(x = "N = 7", times = length(ests_cra)),
                                   rep(x = "N = 21", times = choose(21, 6)),
                                   rep(x = "N = 100", times = 10^3),
                                   rep(x = "N = 1000", times = 10^3)))
stand_ests_plot_data$N <- factor(x = stand_ests_plot_data$N, levels = c("N = 7", "N = 21", "N = 100", "N = 1000"))


asymp_stand_ests_plot <- ggplot(data = stand_ests_plot_data,
                          mapping = aes(x = est)) +
  geom_histogram() +
  xlab(label = "Standardized Diff-in-Means estimates") +
  ylab(label = "Probability") +
  theme_bw() +
  facet_wrap(facets = .~ N,
             nrow = 1,
             ncol = 4,
             scales = "free") +
  scale_x_continuous(labels = 0, breaks = 1) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
ggsave(plot = asymp_stand_ests_plot,
       file = "asymp_stand_ests_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)



var_ests_EVs <- sapply(X = 1:3,
                       FUN = function(x) { mean(asymp_ests_h_1_3[[x]][[6]]) })

true_vars <- c(asymp_ests_h_1_3[[1]][[3]], asymp_ests_h_1_3[[2]][[3]], asymp_ests_h_1_3[[3]][[3]])

asym_var_ests_data <- data.frame(var_ests = c(asymp_ests_h_1_3[[1]][[6]], asymp_ests_h_1_3[[2]][[6]], asymp_ests_h_1_3[[3]][[6]]),
                                 h = as.factor(c(rep(x = 1, times = length(asymp_ests_h_1_3[[1]][[6]])),
                                                 rep(x = 2, times = length(asymp_ests_h_1_3[[2]][[6]])),
                                                 rep(x = 3, times = length(asymp_ests_h_1_3[[3]][[6]])))))

levels(asym_var_ests_data$h) <- c("h = 1",
                              "h = 2",
                              "h = 3")

vline_data <- data.frame(h = levels(asym_var_ests_data$h),
                         solid_vl = true_vars,
                         dashed_vl = var_ests_EVs) 

asymp_var_ests_plot <- ggplot(data = asym_var_ests_data,
                              mapping = aes(x = var_ests, y = (..count..)/sum(..count..))) +
  geom_histogram(data = subset(x = asym_var_ests_data,
                               subset = h == "h = 1"),
                 binwidth = 1) +
  geom_histogram(data = subset(x = asym_var_ests_data,
                               subset = h == "h = 2"),
                 binwidth = 1) +
  geom_histogram(data = subset(x = asym_var_ests_data,
                               subset = h == "h = 3"),
                 binwidth = 1) +
  geom_vline(mapping = aes(xintercept = solid_vl), linetype = "solid", data = vline_data) +
  geom_vline(mapping = aes(xintercept = dashed_vl), linetype = "dashed", data = vline_data) +
  xlab(label = "Variance estimates") +
  ylab(label = "Probability") +
  theme_bw() +
  facet_wrap(facets = .~ h,
             nrow = 1,
             ncol = 3) 

ggsave(plot = asymp_var_ests_plot,
       file = "asymp_var_ests_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

