setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(pbapply)
library(RItools)
library(parallel)
library(ri)
rm(list = ls())
options(scipen = 999)


rdd_data <- read_dta("http://jakebowers.org/Matching/RDReplication.dta") %>%
  filter(Use == 1) ## Use is indicator for whether unit is included in RD incumbency advantage sample

treatment <- matrix(c('DemWin', 'Democrat Wins Election'),
                    ncol = 2,
                    byrow = TRUE)

dimnames(treatment) <- list(1, c("Treatment", "Description"))

table(rdd_data$DemWin)

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

## remove missing outcomes
rdd_data <- filter(.data = rdd_data, !is.na(DPctNxt))

## choose bandwidth
bal_fmla <- reformulate(covs[1:25], response = "DemWin")

cand_bands <- seq(from = 1,
                  to = .03,
                  by = -.01)

band_p_values <- pbsapply(X = cand_bands,
                     FUN = function(x) {
                       
                       xBalance(fmla = bal_fmla,
                                data = filter(.data = rdd_data,
                                              rdd_data$DifDPct > -x  & rdd_data$DifDPct  < x),
                                report = "chisquare.test")$overall[[3]]
                       })

## choose greatest bandwidth with p-value above 0.2
band <- cand_bands[which(band_p_values > 0.4)][1]

band_data <- filter(.data = rdd_data,
                    DifDPct > -band  & rdd_data$DifDPct  < band)

## order outcomes from highest to lowest
band_data <- arrange(.data = band_data, desc(DPctNxt))

obs_diff_means <- mean(band_data$DPctNxt[which(band_data$DemWin == 1)]) -
  mean(band_data$DPctNxt[which(band_data$DemWin == 0)])

set.seed(123456)
Omega <- genperms(Z = band_data$DemWin, maxiter = 10^5)

sharp_null_diff_means <- pbsapply(X = 1:ncol(Omega),
                                  FUN = function(x) { 
                                    
                                    mean(band_data$DPctNxt[which(Omega[,x] == 1)]) -
                                      mean(band_data$DPctNxt[which(Omega[,x] == 0)])
                                    
                                  })

p_value <- mean(sharp_null_diff_means >= obs_diff_means)
p_value

n <- nrow(band_data)

u_combs <- matrix(data = NA,
                  nrow = n,
                  ncol = n - 1)
u_combs[lower.tri(u_combs)] <- 0
u_combs[!lower.tri(u_combs)] <- 1

source("sens_probs_fun.R")

gammas <- seq(from = 0, to = 6, by = 0.1)

sens_Omega_probs <- pblapply(X = gammas,
                             FUN = function(g) { 
                               
                               pbsapply(X = 1:ncol(u_combs),
                                        FUN = function(u) {
                                          
                                          sens_probs(.gamma = g,
                                                     .Omega = Omega,
                                                     .u = u_combs[,u]) },
                                        cl = detectCores()) },
                             cl = detectCores())

sens_p_values <- pblapply(X = 1:length(gammas),
                          FUN = function(g) { 
                            
                            pbsapply(X = 1:ncol(sens_Omega_probs[[g]]),
                                     FUN = function(u) {  sum((sharp_null_diff_means >= obs_diff_means) * sens_Omega_probs[[g]][,u]) },
                                     cl = detectCores()) },
                          cl = detectCores())

sens_max_p_values <- pbsapply(X = 1:length(sens_p_values),
                              FUN = function(p) { max(sens_p_values[[p]]) },
                              cl = detectCores())

## check if p-values monotone increasing
all(sens_max_p_values == cummax(sens_max_p_values))

max_us <- pbsapply(X = 2:length(sens_p_values),
                   FUN = function(p) { u_combs[,which.max(sens_p_values[[p]])] },
                   cl = detectCores())

sens_plot_data <- data.frame(gamma = gammas,
                             p_value = sens_max_p_values)

ggplot(data = sens_plot_data,
       mapping = aes(x = gamma,
                     y = p_value)) +
  geom_line() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  xlab(label = expression(gamma)) +
  ylab(label = "p-value")

####################
## Now compare to Rosenbaum senstrat package
library(senstrat)

## rescale outcome so sum statistic from senstrat package equivalent to
## diff in means
n_0 <- sum(1 - band_data$DemWin)
n_1 <- sum(band_data$DemWin)
h <- (n_0^{-1} + n_1^{-1})^{-1}

sum(band_data$DPctNxt[which(band_data$DemWin == 1)])/h - sum(band_data$DPctNxt)/n_0
obs_diff_means

band_data$DPctNxt_resc <- (band_data$DPctNxt/h - ((sum(band_data$DPctNxt)/(n_1 * n_0))))

senstrat_p_values <- pbsapply(X = seq(from = 1, to = 60, by = 1),
                              FUN = function(g) { sen2sample(sc = band_data$DPctNxt_resc,
                                                             z = band_data$DemWin,
                                                             gamma = g,
                                                             alternative = "greater")$pval },
                              cl = detectCores())

senstrat_plot_data <- data.frame(gamma = seq(from = 1, to = 60, by = 1),
                                 p_value = senstrat_p_values)

ggplot(data = senstrat_plot_data,
       mapping = aes(x = gamma,
                     y = p_value)) +
  geom_line() +
  xlab(label = expression(gamma)) +
  ylab(label = "p-value")






