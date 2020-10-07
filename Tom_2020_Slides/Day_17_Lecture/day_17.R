#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#rm(list = ls())
library(here)
library(dplyr)
library(tidyverse)
library(magrittr)
library(pbapply)
library(parallel)
library(haven)
library(optmatch)
library(RItools)
library(blkvar)

z <- c(1, 0, 0, 1)
n <- 4
n_1 <- sum(z)

x_1 <- c(3, 3, 3, 3)
x_2 <- c(1, 1, 1, 1)

beta_1 <- 0.25
beta_2 <- 0.1

beta_vec <- as.matrix(c(beta_1, beta_2))
x_vec <- as.matrix(cbind(x_1, x_2))
x_vec %*% beta_vec

## Individual assignment probs as a logit function of x_1
probs <- exp(x_vec %*% beta_vec)/(1 + exp(x_vec %*% beta_vec))

Omega <- do.call(what = "cbind",
                 args = pblapply(X = 0:n,
                                 FUN = function(n_1) { apply(X = combn(x = 1:n,
                                                                       m = n_1),
                                                             MARGIN = 2,
                                                             FUN = function(x) { as.integer(1:n %in% x) }) },
                                 cl = detectCores()))

Omega_probs <- pbsapply(X = 1:ncol(Omega),
                        FUN = function(z) { prod(ifelse(test = Omega[,z] == 1,
                                                        yes = probs,
                                                        no = 1 - probs)) },
                        cl = detectCores())

Omega_probs/sum(Omega_probs)

beta_1 <- 0.25
beta_2 <- 0.1

## Individual assignment probs as a logit function of x_1
probs <- exp(x_vec %*% beta_vec)/(1 + exp(x_vec %*% beta_vec))

## The distribution on assignments with conditioning on n_1
Omega_n_1 <- apply(X = combn(x = 1:n,
                             m = n_1),
                   MARGIN = 2,
                   FUN = function(x) { as.integer(1:n %in% x) })

Omega_n_1_probs <- pbsapply(X = 1:ncol(Omega_n_1),
                            FUN = function(z) { prod(ifelse(test = Omega_n_1[,z] == 1,
                                                            yes = probs,
                                                            no = 1 - probs)) },
                            cl = detectCores())
Omega_n_1_probs/sum(Omega_n_1_probs)

########################################################################################################################################################################################################################
#install.packages("optmatch")
#install.packages("RItools")
#install.packages("tidyverse")
library("optmatch")
library("RItools")
library("tidyverse")
library("pbapply")
library("parallel")
library("haven")
library("optmatch")
rm(list = ls())
options(scipen = 999)
## make fake data
z <- rep(x = 0:1, times = 10)
y <- c(11, 20, 5, 15, 0, 6, 50, 2, 3, 40,
       55, 87, 64, 50, 48, 93, 51, 52, 15, 37)
data <- data.frame(z = z, 
                   y = y)

data <- arrange(.data = data, desc(y))

n <- length(z)
n_1 <- sum(z)
n_0 <- n - n_1

## generate set of all possible assignments
Omega <- pbapply(X = combn(x = n,
                           m = n_1,
                           simplify = TRUE),
                 MARGIN = 2,
                 FUN = function(x) { as.integer(1:n %in% x) },
                 cl = detectCores())

## generate set of Us
u_combs <- matrix(data = 0,
                  nrow = n,
                  ncol = n - 1)
u_combs[!lower.tri(u_combs)] <- 1

obs_diff_means <- mean(data$y[which(data$z == 1)]) - mean(data$y[which(data$z == 0)])

null_diff_means <- pbsapply(X = 1:ncol(Omega),
                            FUN = function(z) { mean(data$y[which(Omega[,z] == 1)]) - mean(data$y[which(Omega[,z] == 0)]) },
                            cl = detectCores())

p_value <- mean(null_diff_means >= obs_diff_means)

omega_k_index <- which(null_diff_means >= obs_diff_means)

Gammas <- seq(from = 1.1, to = 6, by = 0.1)

z_t_us <- pbsapply(X = 1:ncol(u_combs),
                   FUN = function(u){ pbsapply(X = 1:ncol(Omega),
                                               FUN = function(z) { t(Omega[,z]) %*% u_combs[,u]  },
                                               cl = detectCores()) },
                   cl = detectCores())

## each row is a z \in \Omega
## each column is a u \in U^+
max_sens_p_values <- pbsapply(X = Gammas,
                              FUN = function(G) { max(colSums(x = (exp(log(G) * z_t_us))[omega_k_index,])/colSums(x = exp(log(G) * z_t_us))) })

max_us <- pbsapply(X = Gammas,
                   FUN = function(G) { which.max(colSums(x = (exp(log(G) * z_t_us))[omega_k_index,])/colSums(x = exp(log(G) * z_t_us))) })
length(unique(max_us))

######################
## How does simulation-based approach compare?
set.seed(1:5)
n_sims <- 10^3
Omega_sim <- replicate(n = n_sims, expr = sample(x = data$z))

null_diff_means_sim <- pbsapply(X = 1:ncol(Omega_sim),
                                FUN = function(z) { mean(data$y[which(Omega_sim[,z] == 1)]) - mean(data$y[which(Omega_sim[,z] == 0)]) },
                                cl = detectCores())

p_value_sim <- mean(null_diff_means_sim >= obs_diff_means)

omega_k_index_sim <- which(null_diff_means_sim >= obs_diff_means)

z_t_us_sim <- pbsapply(X = 1:ncol(u_combs),
                       FUN = function(u){ pbsapply(X = 1:ncol(Omega_sim),
                                                   FUN = function(z) { t(Omega_sim[,z]) %*% u_combs[,u]  },
                                                   cl = detectCores()) },
                       cl = detectCores())

## each row is a z \in \Omega
## each column is a u \in U^+
max_sens_p_values_sim <- pbsapply(X = Gammas,
                                  FUN = function(G) { max(colSums(x = (exp(log(G) * z_t_us_sim))[omega_k_index_sim,])/colSums(x = exp(log(G) * z_t_us_sim))) })

max_us_sim <- pbsapply(X = Gammas,
                       FUN = function(G) { which.max(colSums(x = (exp(log(G) * z_t_us_sim))[omega_k_index_sim,])/colSums(x = exp(log(G) * z_t_us_sim))) })
length(unique(max_us_sim))

## How does asymptotic Rosenbaum approach compare?
h <- (n_0^{-1} + n_1^{-1})^{-1}
data$q <- y/h - sum(y)/(n_0 * n_1)
data <- arrange(.data = data, desc(q))

library(senstrat)
p_value_rk <- sen2sample(sc = data$q,
                         z = data$z,
                         gamma = 1,
                         alternative = "greater",
                         method = "RK")$pval
p_values_rk <- pbsapply(X = Gammas,
                        FUN = function(G) { sen2sample(sc = data$q,
                                                       z = data$z,
                                                       gamma = G,
                                                       alternative = "greater",
                                                       method = "RK")$pval })
## Plot results
sens_data <- data.frame(Gamma = c(1, Gammas, 1, Gammas, 1, Gammas),
                        p_value = c(p_value, max_sens_p_values, p_value_sim, max_sens_p_values_sim, p_value_rk, p_values_rk),
                        type = rep(x = c("exact", "sim", "RK"), each = (length(Gammas) + 1)))

sens_plot <- ggplot(data = sens_data,
                    mapping = aes(x = Gamma,
                                  y = p_value)) +
  geom_line(data = subset(x = sens_data, subset = type == "exact"), linetype = "solid") +
  geom_line(data = subset(x = sens_data, subset = type == "sim"), linetype = "dashed") +
  geom_line(data = subset(x = sens_data, subset = type == "RK"), linetype = "dotted") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  xlab(label = expression(Gamma)) +
  ylab(label = "p-value") + theme_bw()

ggsave(plot = sens_plot,
       file = "sens_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

########################################################################################################################################################################################################################
############ RDD
rm(list=ls())

rdd_data <- read_dta("http://jakebowers.org/Matching/RDReplication.dta") %>%
  filter(Use == 1) ## Use is indicator for whether unit is included in RD incumbency advantage sample

covs <- matrix(c('DWinPrv', 'Dem Win t - 1',
                 'DPctPrv', 'Dem % t - 1',
                 'DifDPPrv', 'Dem % Margin t - 1',
                 'IncDWNOM1', 'Inc\'s D1 NOMINATE',
                 'DemInc', 'Dem Inc in Race',
                 'NonDInc', 'Rep Inc in Race',
                 'PrvTrmsD', 'Dem\'s # Prev Terms',
                 'PrvTrmsO', 'Rep\'s # Prev Terms',
                 'RExpAdv', 'Rep Experience Adv',
                 'DExpAdv', 'Dem Experience Adv',
                 'ElcSwing', 'Partisan Swing',
                 'CQRating3', 'CQ Rating {-1, 0, 1}',
                 'DSpndPct', 'Dem Spending %',
                 'DDonaPct', 'Dem Donation %',
                 'SoSDem', 'Dem Sec of State',
                 'GovDem', 'Dem Governor',
                 'DifPVDec', 'Dem Pres % Margin', ## average over decade
                 'DemOpen', 'Dem-held Open Seat',
                 'NonDOpen', 'Rep-held Open Seat',
                 'OpenSeat', 'Open Seat',
                 'VtTotPct', 'Voter Turnout %',
                 'GovWkPct', 'Pct Gov\'t Worker',
                 'UrbanPct', 'Pct Urban',
                 'BlackPct', 'Pct Black',
                 'ForgnPct', 'Pct Foreign Born'),
               ncol = 2,
               byrow = TRUE)

dimnames(covs) <- list(seq(from = 1,
                           to = 25,
                           by = 1),
                       c("Covariate", "Description"))

bal_fmla <- reformulate(covs[1:25], response = "DemWin")

candidate_bands <- seq(from = 1,
                       to = 0.1,
                       by = -(1/168))

## use symmetric bandwidth
cov_bals <- pbsapply(X = 1:length(candidate_bands),
                     FUN = function(b) { xBalance(fmla = bal_fmla,
                                                  data = filter(.data = rdd_data, DifDPct > -candidate_bands[b] & DifDPct < candidate_bands[b]),
                                                  report = "chisquare.test")$overall[["p.value"]] },
                     cl = detectCores())

## select larges bandwidth with p-value >= 0.7
band_data <- filter(.data = rdd_data, DifDPct > -candidate_bands[which(cov_bals >= 0.7)[1]] &
                      DifDPct < candidate_bands[which(cov_bals >= 0.7)[1]])

###
dvs <- matrix(c('DWinNxt', 'Dem Win t + 1',
                'DPctNxt', 'Dem % t + 1',
                'DifDPNxt', 'Dem % Margin t + 1'),
              ncol = 2,
              byrow = TRUE)

band_data <- filter(.data = band_data, !is.na(DWinNxt), !is.na(DPctNxt), !is.na(DifDPNxt))

band_data <- rename(.data = band_data,
                    z = DemWin,
                    y = DPctNxt)

band_data <- arrange(.data = band_data, desc(y))

n <- nrow(band_data)
n_1 <- sum(band_data$z)

obs_diff_means <- mean(band_data$y[which(band_data$z == 1)]) -
  mean(band_data$y[which(band_data$z == 0)])

#Omega_rdd <- pbapply(X = combn(x = n,
#                               m = n_1),
#                     MARGIN = 2,
#                     FUN = function(x) { as.integer(1:n %in% x) },
#                     cl = detectCores())
#save(Omega_rdd, file = "Omega_rdd.RData")

set.seed(123456)
n_sims <- 10^5
Omega_sim <- cbind(band_data$z, replicate(n = n_sims, expr = sample(x = band_data$z)))

null_diff_means_sim <- pbsapply(X = 1:ncol(Omega_sim),
                                FUN = function(z) { mean(band_data$y[which(Omega_sim[,z] == 1)]) -
                                    mean(band_data$y[which(Omega_sim[,z] == 0)]) },
                                cl = detectCores())

p_value_sim <- mean(null_diff_means_sim >= obs_diff_means)

omega_k_index_sim <- which(null_diff_means_sim >= obs_diff_means)

Gammas <- seq(from = 1.1, to = 12, by = 0.1)
## generate set of Us
u_combs <- matrix(data = 0,
                  nrow = nrow(band_data),
                  ncol = nrow(band_data) - 1)
u_combs[!lower.tri(u_combs)] <- 1

z_t_us_sim <- pbsapply(X = 1:ncol(u_combs),
                       FUN = function(u){ pbsapply(X = 1:ncol(Omega_sim),
                                                   FUN = function(z) { t(Omega_sim[,z]) %*% u_combs[,u]  },
                                                   cl = detectCores()) },
                       cl = detectCores())


max_sens_p_values_sim <- pbsapply(X = Gammas,
                                  FUN = function(G) { max(colSums(x = (exp(log(G) * z_t_us_sim))[omega_k_index_sim,])/colSums(x = exp(log(G) * z_t_us_sim))) })

max_us_sim <- pbsapply(X = Gammas,
                       FUN = function(G) { which.max(colSums(x = (exp(log(G) * z_t_us_sim))[omega_k_index_sim,])/colSums(x = exp(log(G) * z_t_us_sim))) })
length(unique(max_us_sim))

## How does asymptotic Rosenbaum approach compare?
n_0 <- sum(1 - band_data$z)
n_1 <- sum(band_data$z)
h <- (n_0^{-1} + n_1^{-1})^{-1}
band_data$q <- band_data$y/h - sum(band_data$y)/(n_0 * n_1)
data <- arrange(.data = band_data, desc(q))

library(senstrat)
p_value_rk <- sen2sample(sc = band_data$q,
                         z = band_data$z,
                         gamma = 1,
                         alternative = "greater",
                         method = "RK")$pval
p_values_rk <- pbsapply(X = Gammas,
                        FUN = function(G) { sen2sample(sc = band_data$q,
                                                       z = band_data$z,
                                                       gamma = G,
                                                       alternative = "greater",
                                                       method = "RK")$pval })
alpha <- 0.1
## Plot results
sens_data <- data.frame(Gamma = c(1, Gammas, 1, Gammas),
                        p_value = c(p_value_sim, max_sens_p_values_sim, p_value_rk, p_values_rk),
                        type = rep(x = c("sim", "RK"), each = (length(Gammas) + 1)))

rdd_sens_plot <- ggplot(data = sens_data,
                        mapping = aes(x = Gamma,
                                      y = p_value)) +
  geom_line(data = subset(x = sens_data, subset = type == "sim"), linetype = "dashed") +
  geom_line(data = subset(x = sens_data, subset = type == "RK"), linetype = "dotted") +
  geom_vline(xintercept = c(Gammas[which(max_sens_p_values_sim >= alpha)[1]],
                            Gammas[which(p_values_rk >= alpha)[1]]),
             linetype = c("dashed", "dotted")) +
  geom_hline(yintercept = alpha,
             linetype = "solid") +
  scale_x_continuous(breaks = 1:12) +
  xlab(label = expression(Gamma)) +
  ylab(label = "p-value") +
  theme_bw()

ggsave(plot = rdd_sens_plot,
       file = "rdd_sens_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

########################################################################################################################################################################################################################
############ Matching

data <- read_dta(here::here("Tom_2020_Slides/Day_17_Lecture","peace_pre_match.dta"))
covs <- c("lwdeaths", "lwdurat", "milper", "pop", "ethfrac",
          "lmtnest", "bwplty2", "ssafrica", "asia", "lamerica")  

reformulate(termlabels = covs, response = "DemWin")


psm <- glm(formula = UN ~ lwdeaths + lwdurat + milper +
             pop + ethfrac + lmtnest + bwplty2 + ssafrica + asia + lamerica,
           family = binomial(link = "logit"),
           data = data)

data$p_score <- psm$linear.predictors

em_geog <- exactMatch(x = UN ~ ssafrica + asia + lamerica, data = data)

ps_dist_mat <- match_on(x = UN ~ p_score, data = data)

ps_dist_cal_0.2 <- ps_dist_mat + caliper(x = ps_dist_mat, width = 0.2)

dist_mat_rank_mah <- match_on(x = UN ~ lwdeaths + lwdurat + milper + pop + ethfrac +
                                lmtnest + bwplty2,
                              data = data,
                              method = "rank_mahalanobis")

fm_UN <- fullmatch(x = dist_mat_rank_mah + ps_dist_cal_0.2 + em_geog,
                   data = data)

summary(fm_UN)

## check balance
xBalance(fmla = UN ~ lwdeaths + lwdurat + milper + pop + ethfrac +
           lmtnest + bwplty2 + ssafrica + asia + lamerica,
         strata = list(unstrat = NULL,
                       fm = ~ fm_UN),
         data = data,
         report = c("adj.means", "std.diffs"))

xBalance(fmla = UN ~ lwdeaths + lwdurat + milper + pop + ethfrac +
           lmtnest + bwplty2 + ssafrica + asia + lamerica,
         strata = list(fm = ~ fm_UN),
         data = data,
         report = "chisquare.test")

data$fm <- fm_UN

match_data <- filter(.data = data, !is.na(fm))
match_data <- arrange(.data = match_data, fm, desc(dur))
match_data <- select(.data = match_data, UN, dur, fm)

obs_diff_means <- block_estimator(Yobs = dur,
                                  Z = UN,
                                  B = fm,
                                  data = match_data)$ATE_hat

library(ri)
set.seed(123456)
Omega <- genperms(Z = match_data$UN,
                  blockvar = match_data$fm,
                  maxiter = 10^3)

null_diff_means <- pbapply::pbsapply(X = 1:ncol(Omega),
                                     FUN = function(x) {
                                       match_data$Z_rand = Omega[,x]
                                       est_star = block_estimator(Yobs = dur,
                                                                  Z = Z_rand,
                                                                  B = fm,
                                                                  data = match_data)
                                       est_star$ATE_hat },
                                     cl = detectCores())

p_value <- mean(null_diff_means >= obs_diff_means)
p_value

omega_k_index <- which(null_diff_means >= obs_diff_means)

strata <- sort(match_data$fm)

stratum_sizes <- 
  table(strata)

within_stratum_us <- 
  stratum_sizes %>% 
  unique %>% 
  lapply(., function(stratum_sizes) {
    mat <- matrix(data = 0,
                  nrow = stratum_sizes,
                  ncol = stratum_sizes - 1)
    mat[!lower.tri(mat)] <- 1
    return(mat)
  }) %>% 
  `names<-`(as.character(unique(stratum_sizes)))

u_combs <- 
  pbapply::pbreplicate(n = 10^3, {
    lapply(X = stratum_sizes,
           FUN = function(sts) {
             within_stratum_us[[as.character(sts)]] %>% 
               {.[,sample(x = 1:(sts - 1), size = 1)]}
           }) %>% 
      unname %>% 
      do.call("c", args = .)
  }, cl = detectCores()) %>% 
  { .[, !duplicated(t(.))] } %>% 
  as.data.frame() %>% 
  bind_cols(stratum = strata, .)

z_t_us_sim <- pbsapply(X = 2:ncol(u_combs),
                       FUN = function(u){ pbsapply(X = 1:ncol(Omega),
                                                   FUN = function(z) { t(Omega[,z]) %*% u_combs[,u]  },
                                                   cl = detectCores()) },
                       cl = detectCores())

Gammas <- seq(from = 1.1, to = 6, by = 0.1)

max_sens_p_values_sim <- pbsapply(X = Gammas,
                                  FUN = function(G) { max(colSums(x = (exp(log(G) * z_t_us_sim))[omega_k_index,])/colSums(x = exp(log(G) * z_t_us_sim))) })

max_us_sim <- pbsapply(X = Gammas,
                       FUN = function(G) { which.max(colSums(x = (exp(log(G) * z_t_us_sim))[omega_k_index,])/colSums(x = exp(log(G) * z_t_us_sim))) })
length(unique(max_us_sim))

match_data <- match_data %>% group_by(fm) %>% mutate(fm_n = n(),
                                                     fm_n_0 = sum(1 - UN),
                                                     fm_n_1 = sum(UN),
                                                     fm_sum_dur = sum(dur),
                                                     fm_h = (sum(1 - UN)^{-1} + sum(UN)^{-1})^{-1})
match_data <- mutate(.data = match_data,
                     dur_resc = dur/fm_h - fm_sum_dur/(fm_n_0 * fm_n_1))

library(senstrat)
p_value_rk <- senstrat(sc = match_data$dur_resc,
                       z = match_data$UN,
                       st = match_data$fm,
                       gamma = 1,
                       alternative = "greater",
                       method = "RK")$Result[1]

p_values_rk <- pbsapply(X = Gammas,
                        FUN = function(G) { senstrat(sc = match_data$dur_resc,
                                                     z = match_data$UN,
                                                     st = match_data$fm,
                                                     gamma = G,
                                                     alternative = "greater",
                                                     method = "RK")$Result[1] })
## Plot results
sens_data <- data.frame(Gamma = c(1, Gammas, 1, Gammas),
                        p_value = c(p_value, max_sens_p_values_sim, p_value_rk, p_values_rk),
                        type = rep(x = c("sim", "RK"), each = (length(Gammas) + 1)))

match_sens_plot <- ggplot(data = sens_data,
                          mapping = aes(x = Gamma,
                                        y = p_value)) +
  geom_line(data = subset(x = sens_data, subset = type == "sim"), linetype = "dashed") +
  geom_line(data = subset(x = sens_data, subset = type == "RK"), linetype = "dotted") +
  scale_x_continuous(breaks = 1:6) +
  xlab(label = expression(Gamma)) +
  ylab(label = "p-value") +
  theme_bw()

ggsave(plot = match_sens_plot,
       file = "match_sens_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)
