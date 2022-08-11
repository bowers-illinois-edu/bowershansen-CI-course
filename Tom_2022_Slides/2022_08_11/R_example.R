setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
library(tidyverse)
library(magrittr)
library(optmatch)

# Neymanian hypothesis testing --------------------------------------------

acorn_data <- read.csv("acorn03.csv")

diff_means <- mean(acorn_data$vote03[acorn_data$z == 1]) -
  mean(acorn_data$vote03[acorn_data$z == 0])

consv_var <- var(acorn_data$vote03[acorn_data$z == 1]) +
  var(acorn_data$vote03[acorn_data$z == 0])

stand_test_stat <- (diff_means - 0) / sqrt(consv_var)

acorn_stand_norm_null <- ggplot(data = data.frame(null_stat = c(-3, 3)),
                                mapping = aes(x = null_stat)) +
  stat_function(fun = dnorm,
                n = 101,
                args = list(mean = 0, sd = 1)) +
  labs(x = "Null standardized Diff-in-Means", 
       y = "Probability density") +
  scale_y_continuous(breaks = NULL) +
  theme_bw() +
  geom_vline(xintercept = stand_test_stat,
             linetype = "dashed")

#ggsave(plot = acorn_stand_norm_null,
#       file = "acorn_stand_norm_null.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)


# Sensitivity analysis for weak nulls -------------------------------------
rm(list = ls())
load(url("http://jakebowers.org/Data/meddat.rda"))
meddat <- mutate(meddat,
                 HomRate03 = (HomCount2003 / Pop2003) * 1000,
                 HomRate08 = (HomCount2008 / Pop2008) * 1000
)
row.names(meddat) <- meddat$nh

## Make one of the covariates have missing data to
## demonstrate how to match on it

covs <- c("nhPopD", "nhAboveHS","HomRate03")
balfmla <- reformulate(termlabels = covs,
                       response = "nhTrt")

mhdist <- match_on(x = balfmla,
                   data = meddat,
                   method = "rank_mahalanobis")

psmod <- arm::bayesglm(formula = balfmla,
                       data = meddat,
                       family = binomial(link = "logit"))
stopifnot(any(abs(coef(psmod)) < 10))
psdist <- match_on(x = psmod,
                   data = meddat)
## Make a scalar distance
tmp <- meddat$HomRate03
names(tmp) <- rownames(meddat)
absdist <- match_on(x = tmp,
                    z = meddat$nhTrt,
                    data = meddat)

## Inspect the distance matrices to choose calipers if desired
quantile(x = as.vector(x = psdist),
         probs = seq(from = 0,
                     to = 1,
                     by = 0.1))

quantile(x = as.vector(x = mhdist),
         probs = seq(from = 0,
                     to = 1,
                     by = 0.1))
quantile(x = as.vector(x = absdist),
         probs = seq(from = 0,
                     to = 1,
                     by = 0.1))
## Match and require no more than 3 treated per control, and no more than 5 control per treated
fmMh <- fullmatch(x = psdist + caliper(x = psdist,
                                       width = 5) + caliper(x = absdist,
                                                            width = 2) + caliper(x = mhdist,
                                                                                 width = 52),
                  min.controls = 0, ## 1/3
                  max.controls = Inf,
                  data = meddat,
                  tol = .00001)
summary(object = fmMh,
        min.controls = 0,
        max.controls = Inf,
        propensity.model = psmod)
meddat$fmMh <- factor(x = fmMh)
meddat$nhTrtF <- factor(x = meddat$nhTrt)
meddat <- filter(.data = meddat,
                 !is.na(fmMh))

var_est <- function(y,
                    z,
                    Gamma,
                    block,
                    data){
  
  data[,"y"] = data[,y]
  data[,"z"] = data[,z]
  data[,"block"] = data[,block]
  
  data = select(.data = data,
                y, z, block)
  
  data = arrange(.data = data,
                 block)
  
  N = length(data$y)
  
  B = length(unique(data$block))
  
  blocks = sort(unique(data$block))
  
  Q = as.matrix(sapply(X = blocks,
                       FUN = function(x) { B * (sum(blocks == x) / N) }))
  
  H_Q = Q %*% solve(t(Q) %*% Q) %*% t(Q)
  
  H_Q_diag = diag(H_Q)
  
  block_stats <- dplyr::group_by(.data = data,
                                 block) %>% summarize(n_prop = n()/N,
                                                      Ds = (mean(y[z == 1]) -
                                                              mean(y[z == 0])) -
                                                        ((Gamma - 1) / (1 + Gamma)) * abs((mean(y[z == 1]) -
                                                                                             mean(y[z == 0]))))
  
  Y_Gammas = B * block_stats$n_prop * block_stats$Ds / sqrt(1 - H_Q_diag)
  
  est_var = (t(Y_Gammas) %*% (diag(B) - H_Q) %*% Y_Gammas) / B^2
  
  return(as.numeric(est_var))
  
  }


worst_case_IPW <- function(y,
                           z,
                           Gamma,
                           block,
                           data){
  
  data[,"y"] = data[,y]
  data[,"z"] = data[,z]
  data[,"block"] = data[,block]
  
  data = select(.data = data,
                y, z, block)
  
  data = arrange(.data = data,
                 block)
  
  N = length(data$y)
  
  block_stats <- dplyr::group_by(.data = data,
                                 block) %>% summarize(n = n(),
                                                      n_prop = n()/N,
                                                      n_treat = sum(z),
                                                      n_treat_prop = sum(z)/n(),
                                                      block_Omega_size = choose(n = n(),
                                                                                k = sum(z)),
                                                      diff_means = mean(y[z == 1]) -
                                                              mean(y[z == 0]),
                                                      lb_Gamma = 1 / (Gamma * (block_Omega_size - 1) + 1),
                                                      ub_Gamma = Gamma / ((block_Omega_size - 1) + Gamma),
                                                      worst_case_IPW = (1/block_Omega_size) *
                                                        (diff_means / ifelse(test = diff_means >= 0,
                                                                             yes = ub_Gamma,
                                                                             no = lb_Gamma)))
  worst_case_IPW = sum(block_stats$n_prop * block_stats$worst_case_IPW)
  
  return(worst_case_IPW) }

library(blkvar)
obs_diff_means <- block_estimator(Yobs = HomRate03,
                                  Z = nhTrt,
                                  B = fmMh,
                                  data = meddat,
                                  method = "hybrid_p" )$ATE_hat

Gammas <- seq(from = 1, to = 2, by = 0.001)

var_ests <- sapply(X = Gammas,
                   FUN = function(x) { var_est(y = "HomRate03",
                                               z = "nhTrt",
                                               Gamma = x,
                                               block = "fmMh",
                                               data = meddat) })

worst_case_IPW_ests <- sapply(X = Gammas,
                              FUN = function(x) { worst_case_IPW(y = "HomRate03",
                                                                 z = "nhTrt",
                                                                 Gamma = x,
                                                                 block = "fmMh",
                                                                 data = meddat) })

upper_p_values <- pnorm(q = worst_case_IPW_ests/var_ests,
                        mean = 0,
                        sd = 1,
                        lower.tail = FALSE)

sens_plot_data <- data.frame(Gamma = Gammas,
                             p_value = upper_p_values)

ggplot(data = sens_plot_data,
       mapping = aes(x = Gamma,
                     y = p_value)) +
  geom_line() +
  theme_bw() +
  #scale_x_continuous(breaks = seq(from = 1, to = 2, by = 0.1)) +
  #scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  xlab(label = expression(Gamma)) +
  ylab(label = "Upper bound of p-value")




library(sensitivitymv)
lead_150 <- data(lead150) # 1:1 pair matching
lead_250 <- data(lead250) # 1:5 matching

