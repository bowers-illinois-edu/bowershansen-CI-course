setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

## Complete random assignment
n <- 7
n_1_cra <- 2

y_c <- c(10, 15, 20, 20, 10, 15, 15)
y_t <- c(15, 15, 30, 15, 20, 15, 30)
tau <- y_t - y_c
ATE <- mean(tau)

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
                       FUN = function(x) Omega_cra[,x] * y_t + (1 - Omega_cra[,x]) * y_c )

## calculate estimate for each of 21 possible realizations of data
ests_cra <- sapply(X = 1:ncol(Omega_cra),
               FUN = function(x) { mean(obs_outs_cra[,x][which(Omega_cra[,x] == 1)]) - mean(obs_outs_cra[,x][which(Omega_cra[,x] == 0)]) })

## unbiased
ests_cra_EV <- sum(ests_cra * probs_cra)

ests_cra_Var <- mean((ests_cra - ests_cra_EV)^2)

round(x = ests_cra_Var, digits = 2)

## analytic expression for variance
var_y_c <- mean((y_c - mean(y_c))^2)
round(x = var_y_c, digits = 2)
var_y_t <- mean((y_t - mean(y_t))^2)
round(x = var_y_t, digits = 2)
cov_y_c_y_t <- mean((y_c - mean(y_c)) * (y_t - mean(y_t)))
round(x = cov_y_c_y_t, digits = 2)

diff_means_var <- function(.n,
                           .n_t,
                           .y_c,
                           .y_t) {
  
  
  
  var_y_c = mean((.y_c - mean(.y_c))^2)
  
  var_y_t = mean((.y_t - mean(.y_t))^2)
  
  cov_y_c_y_t = mean((.y_c - mean(.y_c)) * (.y_t - mean(.y_t)))
  
  var = (1/(.n - 1)) * ((.n_t * var_y_c) / (.n - .n_t) +
                          
                          (((.n - .n_t) * var_y_t) / .n_t) +
                          
                          (2 * cov_y_c_y_t))
  
  return(var)
  
}


true_var_diff_means_est_cra <- diff_means_var(.n = n,
                                              .n_t = n_1_cra,
                                              .y_c = y_c,
                                              .y_t = y_t)

diff_means_var_est <- function(.n,
                               .n_1,
                               .z,
                               .y) {
  
  obs_y_c = .y[which(.z == 0)]
  obs_y_t = .y[which(.z == 1)]
  
  est_mean_y_c = mean(obs_y_c)
  est_mean_y_t = mean(obs_y_t)
  
  est_var_y_c = ((.n - 1)/(.n * ((.n - .n_1) - 1))) * sum((obs_y_c - est_mean_y_c)^2)
  
  est_var_y_t = ((.n - 1)/(.n * (.n_1 - 1))) * sum((obs_y_t - est_mean_y_t)^2)
  
  return((.n/(.n - 1)) * ( (est_var_y_c/(.n - .n_1)) + (est_var_y_t/.n_1)))
  
  
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

## Show asymptotic properties
asymp_ests <- function(.y_c,
                       .y_t,
                       .prop_t,
                       .h){
  
  y_c = c(.y_c, rep(x = .y_c, times = .h - 1))
  
  y_t = c(.y_t, rep(x = .y_t, times = .h - 1))
  
  Omega = apply(X = combn(x = length(y_c),
                          m = length(y_c) * .prop_t,
                          simplify = TRUE),
                MARGIN = 2,
                FUN = function(x) as.integer(1:(length(y_c)) %in% x))
  
  true_EV_diff_means_est = mean(y_t) - mean(y_c)
  
  true_var_diff_means_est = diff_means_var(.n = length(y_c),
                                           .n_t = length(y_c) * .prop_t,
                                           .y_c = y_c,
                                           .y_t = y_t)
  
  obs_pot_outs = sapply(X = 1:ncol(Omega),
                        FUN = function(x) { y_t * Omega[,x] + y_c * (1 - Omega[,x]) })
  
  diff_means_ests = sapply(X = 1:ncol(Omega),
                           FUN = function(x) { mean(obs_pot_outs[,x][Omega[,x] == 1]) -
                               mean(obs_pot_outs[,x][Omega[,x] == 0]) })
  
  var_ests = sapply(X = 1:ncol(Omega),
                    FUN = function(x) { diff_means_var_est(.n = length(y_c),
                                                           .n_1 = length(y_c) * .prop_t,
                                                           .z = Omega[,x],
                                                           .y = obs_pot_outs[,x]) })
  
  return(list(Omega, true_EV_diff_means_est, true_var_diff_means_est, obs_pot_outs, diff_means_ests, var_ests))
  
}

asymp_ests_h_1_3 <- lapply(X = 1:3,
                           FUN = function(x) { asymp_ests(.y_c = y_c,
                                                          .y_t = y_t,
                                                          .prop_t = (2/7),
                                                          .h = x)})

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

## Assess Normal approximation
asym_stand_ests_data <- data.frame(stand_ests = c((asymp_ests_h_1_3[[1]][[5]] - asymp_ests_h_1_3[[1]][[2]])/sqrt(asymp_ests_h_1_3[[1]][[3]]),
                                                  (asymp_ests_h_1_3[[2]][[5]] - asymp_ests_h_1_3[[2]][[2]])/sqrt(asymp_ests_h_1_3[[2]][[3]]),
                                                  (asymp_ests_h_1_3[[3]][[5]] - asymp_ests_h_1_3[[3]][[2]])/sqrt(asymp_ests_h_1_3[[3]][[3]])),
                                   h = as.factor(c(rep(x = 1, times = length((asymp_ests_h_1_3[[1]][[5]] - asymp_ests_h_1_3[[1]][[2]])/sqrt(asymp_ests_h_1_3[[1]][[3]]) )),
                                                   rep(x = 2, times = length((asymp_ests_h_1_3[[2]][[5]] - asymp_ests_h_1_3[[2]][[2]])/sqrt(asymp_ests_h_1_3[[2]][[3]]) )),
                                                   rep(x = 3, times = length((asymp_ests_h_1_3[[3]][[5]] - asymp_ests_h_1_3[[3]][[2]])/sqrt(asymp_ests_h_1_3[[3]][[3]]))))))

levels(asym_stand_ests_data$h) <- c("h = 1",
                                    "h = 2",
                                    "h = 3")

asym_stand_ests_plot <- ggplot(data = asym_stand_ests_data,
                               mapping = aes(x = stand_ests, y = (..count..)/sum(..count..))) +
  geom_histogram(data = subset(x = asym_stand_ests_data,
                               subset = h == "h = 1"),
                 binwidth = 1/3) +
  geom_histogram(data = subset(x = asym_stand_ests_data,
                               subset = h == "h = 2"),
                 binwidth = 1/3) +
  geom_histogram(data = subset(x = asym_stand_ests_data,
                               subset = h == "h = 3"),
                 binwidth = 1/3) +
  xlab(label = "Standardized estimates") +
  ylab(label = "Probability") +
  theme_bw() +
  facet_wrap(facets = .~ h,
             nrow = 1,
             ncol = 3,
             scales = "free") 

ggsave(plot = asym_stand_ests_plot,
       file = "asym_stand_ests_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)












