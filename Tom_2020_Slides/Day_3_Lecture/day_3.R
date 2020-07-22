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
sum(ests_cra * probs_cra)

## create dataframe for plot
cra_dist_est_data <- data.frame(est = ests_cra, 
                                prob = probs_cra)

library(ggplot2)
cra_est_dist_plot <- ggplot(data = cra_dist_est_data,
                            mapping = aes(x = est,
                                          y = prob)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = sum(ests_cra * probs_cra),
             linetype = "dashed") +
  theme_bw() +
  xlab(label = "Difference-in-Means estimates") +
  ylab(label = "Probability")

cra_est_dist_plot

ggsave(plot = cra_est_dist_plot,
       file = "cra_est_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

## Simple random assignment
## n_1 can now range from 0 to 7
n_1_sra <- 0:7

## Again, don't worry about the code to follow
Omega_sra <- lapply(X = n_1_sra,
                    FUN = function(x) { apply(X = combn(x = n,
                                                        m = x,
                                                        simplify = TRUE),
                                              MARGIN = 2,
                                              FUN = function(x) as.integer(1:n %in% x)) })

Omega_sra <- do.call(what = "cbind", args = Omega_sra)

## assumes uniform random assignment
probs_sra <- rep(x = 1/ncol(Omega_sra), times = ncol(Omega_sra))

## generate observed outcomes for each possible assignment
obs_outs_sra <- sapply(X = 1:ncol(Omega_sra),
                       FUN = function(x) Omega_sra[,x] * y_t + (1 - Omega_sra[,x]) * y_c )

## generate estimates for each realization of data
ests_sra <- sapply(X = 1:ncol(Omega_sra),
               FUN = function(x) { mean(obs_outs_sra[,x][which(Omega_sra[,x] == 1)]) - mean(obs_outs_sra[,x][which(Omega_sra[,x] == 0)]) })

## why are we getting an error?
sum(ests_sra * probs_sra)

## Check number of treated and control units under each possible assignment
n_1s <- apply(X = Omega_sra, MARGIN = 2, FUN = sum)
n_0s <- apply(X = 1 - Omega_sra, MARGIN = 2, FUN = sum)

## remove assignments in which 0 treated units or 0 control units
cond_Omega_sra <- Omega_sra[,-which(n_1s == 0 | n_0s == 0)]
cond_probs_sra <- rep(x = 1/ncol(cond_Omega_sra), times = ncol(cond_Omega_sra)) ## alternatively, probs_sra[-which(n_1s == 0 | n_0s == 0)]/sum(probs_sra[-which(n_1s == 0 | n_0s == 0)])

## generate observed outcomes over possible assignments conditional
## on there being at least one treated and control unit
cond_obs_outs_sra <- sapply(X = 1:ncol(cond_Omega_sra),
                            FUN = function(x) cond_Omega_sra[,x] * y_t + (1 - cond_Omega_sra[,x]) * y_c )

## calculate estimate for each possible realization of data
cond_ests_sra <- sapply(X = 1:ncol(cond_Omega_sra),
                   FUN = function(x) { mean(cond_obs_outs_sra[,x][which(cond_Omega_sra[,x] == 1)]) - mean(cond_obs_outs_sra[,x][which(cond_Omega_sra[,x] == 0)]) })

## Now unbiased
sum(cond_ests_sra * cond_probs_sra)

## Consistency
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
  
  obs_pot_outs = sapply(X = 1:ncol(Omega),
                        FUN = function(x) { y_t * Omega[,x] + y_c * (1 - Omega[,x]) })
  
  diff_means_ests = sapply(X = 1:ncol(Omega),
                           FUN = function(x) { mean(obs_pot_outs[,x][Omega[,x] == 1]) -
                               mean(obs_pot_outs[,x][Omega[,x] == 0]) })
  
  return(list(Omega, obs_pot_outs, diff_means_ests))
  
}

asymp_ests_h_1_3 <- lapply(X = 1:3,
                           FUN = function(x) { asymp_ests(.y_c = y_c,
                                                          .y_t = y_t,
                                                          .prop_t = (2/7),
                                                          .h = x)})

asym_ests_data <- data.frame(ests = c(asymp_ests_h_1_3[[1]][[3]], asymp_ests_h_1_3[[2]][[3]], asymp_ests_h_1_3[[3]][[3]]),
                             h = as.factor(c(rep(x = 1, times = length(asymp_ests_h_1_3[[1]][[3]])),
                                             rep(x = 2, times = length(asymp_ests_h_1_3[[2]][[3]])),
                                             rep(x = 3, times = length(asymp_ests_h_1_3[[3]][[3]])))))

levels(asym_ests_data$h) <- c("h = 1",
                              "h = 2",
                              "h = 3",
                              "h = 4")

asymp_ests_plot <- ggplot(data = asym_ests_data,
                          mapping = aes(x = ests, y = (..count..)/sum(..count..))) +
  geom_histogram(data = subset(x = asym_ests_data,
                               subset = h == "h = 1"),
                 binwidth = (1/3)) +
  geom_histogram(data = subset(x = asym_ests_data,
                               subset = h == "h = 2"),
                 binwidth = (1/3)) +
  geom_histogram(data = subset(x = asym_ests_data,
                               subset = h == "h = 3"),
                 binwidth = (1/3)) +
  geom_vline(xintercept = mean(y_t) - mean(y_c),
             linetype = "dashed") +
  xlab(label = "Difference-in-Means estimates") +
  ylab(label = "Probability") +
  theme_bw() +
  facet_wrap(facets = .~ h,
             nrow = 1,
             ncol = 4,
             scales = "free") 

ggsave(plot = asymp_ests_plot,
       file = "asymp_ests_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)