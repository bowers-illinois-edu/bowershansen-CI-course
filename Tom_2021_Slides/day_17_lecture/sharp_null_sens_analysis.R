setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
library(tidyverse)
library(senstrat)

load(file = "meddat.RData")
meddat <- filter(.data = meddat, !is.na(fm1))

meddat <- meddat %>%
  group_by(fm1) %>%
  mutate(fm1_n = n(),
         fm1_n_0 = sum(1 - nhTrt),
         fm1_n_1 = sum(nhTrt),
         fm_sum_HomRate08 = sum(HomRate08),
         fm1_h = (1/fm1_n_0 + 1/fm1_n_1)^{-1})
meddat <- mutate(.data = meddat,
                 HomRate08_resc = (HomRate08/fm1_h - fm_sum_HomRate08/(fm1_n_0 * fm1_n_1)) * (fm1_n/nrow(meddat)))

senstrat(sc = meddat$HomRate08_resc,
         z = meddat$nhTrt,
         st = meddat$fm1,
         gamma = 1)$Result["Statistic"]

library(blkvar)
block_estimator(Yobs = HomRate08,
                Z = nhTrt,
                B = fm1,
                data = meddat,
                method = "hybrid_p")$ATE_hat

block_weights_ests <- meddat %>% group_by(fm1) %>% summarise(prop_n = n()/nrow(meddat),
                                                             ate_est = mean(HomRate08[nhTrt == 1]) - mean(HomRate08[nhTrt == 0]))
sum(block_weights_ests$prop_n * block_weights_ests$ate_est)

senstrat_p_values <- sapply(X = seq(from = 1, to = 3, by = 0.001),
                            FUN = function(x) {
                              senstrat(sc = meddat$HomRate08_resc,
                                       z = meddat$nhTrt,
                                       st = meddat$fm1,
                                       gamma = x,
                                       alternative = "less")$Result[1] })
senstrat_plot_data <- data.frame(Gamma = seq(from = 1, to = 3, by = 0.001),
                                 p_value = senstrat_p_values)
min(senstrat_plot_data$Gamma[which(senstrat_plot_data$p_value >= 0.05)])

alpha <- 0.05
ggplot(data = senstrat_plot_data,
       mapping = aes(x = Gamma,
                     y = p_value)) +
  geom_line() +
  geom_hline(yintercept = alpha) +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 1, to = 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 0.2, by = 0.01),
                     labels = seq(from = 0, to = 0.2, by = 0.01)) +
  xlab(label = expression(Gamma)) +
  ylab(label = "p-value")

