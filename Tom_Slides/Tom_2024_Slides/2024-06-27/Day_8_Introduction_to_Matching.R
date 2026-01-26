# Setup -------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
library(tidyverse)
library(arm)
library(optmatch)
load(file = "med_data.RData")

# Matching on full Medellin data ------------------------------------------

covs <- unique(c(names(med_data)[c(5:7, 9:24)], "HomRate03"))
bal_fmla <- reformulate(covs, response = "nhTrt")
ps_mod <- arm::bayesglm(formula = bal_fmla,
                        data = med_data,
                        family = binomial(link = "logit"))
med_data <- mutate(.data = med_data,
                   est_ps = ps_mod$fitted.values)

ps_dist <- match_on(x = ps_mod, data = med_data)
matches <- fullmatch(x = ps_dist, data = med_data)
med_data$fm <- matches

med_data_matched <- filter(.data = med_data, !is.na(fm))

set_size_ests <- group_by(.data = med_data_matched, fm ) %>%
  summarise( n = n(),
             diff_in_means = mean(HomRate08[nhTrt == 1]) - mean(HomRate08[nhTrt == 0]))

obs_diff_means <- sum(set_size_ests$n/sum(set_size_ests$n) * set_size_ests$diff_in_means)
round(x = obs_diff_means, digits = 2)

med_data_matched <- med_data_matched %>%
  group_by(fm) %>%
  mutate(fm_n = n(),
         fm_n_0 = sum(1 - nhTrt),
         fm_n_1 = sum(nhTrt),
         fm_sum_HomRate08 = sum(HomRate08),
         fm_h = (1/fm_n_0 + 1/fm_n_1)^{-1})
med_data_matched <- mutate(.data = med_data_matched,
                           HomRate08_resc = (HomRate08/fm_h - fm_sum_HomRate08/(fm_n_0 * fm_n_1)) * (fm_n/nrow(med_data_matched)))

med_data_matched <- arrange(.data = med_data_matched, fm)

med_data_matched %>%
  dplyr::select(fm, nhTrt, HomRate08) %>%
  print(n = nrow(med_data_matched))

