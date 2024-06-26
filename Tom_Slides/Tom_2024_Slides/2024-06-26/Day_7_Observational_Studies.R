# Setup -------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
library(tidyverse)


# Rosenbaum Table 5.1 -----------------------------------------------------

age <- c(rep(x = "old", times = 10^5),
         rep(x = "old", times = 10^5),
         rep(x = "young", times = 10^5),
         rep(x = "young", times = 10^5))
sex <- c(rep(x = "male", times = 10^5),
         rep(x = "female", times = 10^5),
         rep(x = "male", times = 10^5),
         rep(x = "female", times = 10^5))
z <- c(rep(x = 1, times = 79828),
       rep(x = 0, times = 20172),
       rep(x = 1, times = 79779),
       rep(x = 0, times = 20221),
       rep(x = 1, times = 20021),
       rep(x = 0, times = 79979),
       rep(x = 1, times = 19798),
       rep(x = 0, times = 80202))
y <- c(rep(x = 1, times = 31868),
       rep(x = 0, times = 47960),
       rep(x = 1, times = 8132),
       rep(x = 0, times = 12040),
       rep(x = 1, times = 23983),
       rep(x = 0, times = 55796),
       rep(x = 1, times = 6017),
       rep(x = 0, times = 14204),
       rep(x = 1, times = 3993),
       rep(x = 0, times = 16028),
       rep(x = 1, times = 16007),
       rep(x = 0, times = 63972),
       rep(x = 1, times = 2021),
       rep(x = 0, times = 17777),
       rep(x = 1, times = 7979),
       rep(x = 0, times = 72223))

data <- as.data.frame(cbind(age, sex, z, y))

data <- mutate(.data = data,
               age = as.factor(age),
               sex = as.factor(sex),
               z = as.numeric(z),
               y = as.numeric(y),
               prob = ifelse(test = age == "old", yes = 0.8, no = 0.2))

Omega <- obtain_permutation_matrix(declaration = declare_ra(N = nrow(data),
                                                            m = sum(data$z)))
unnorm_Omega_probs <- apply(X = Omega,
                            MARGIN = 2,
                            FUN = function(x) { prod(ifelse(test = x == 1,
                                                            yes = data$prob,
                                                            no = (1 - data$prob))) })
Omega_probs <- unnorm_Omega_probs / sum(unnorm_Omega_probs)

library(pbapply)
library(parallel)
diff_in_means <- pbsapply(X = 1:ncol(Omega),
                          FUN = function(x) { mean(data$y[Omega[,x] == 1]) - mean(data$y[Omega[,x] == 1]) },
                          cl = (detectCores() - 1))
sum(diff_in_means * Omega_probs)


library(arm)
library(optmatch)
load(file = "med_data.RData")


# Simple matching example with 4 units and 1 covariate --------------------
data <- data.frame(neighborhood = paste("Neighborhood", c("A", "B", "C", "D"), sep = "_"),
                   metrocable = c(1, 1, 0, 0),
                   emply_rate = round(x = c(med_data$nhEmp[med_data$nhTrt == 1][13:14],
                                            med_data$nhEmp[med_data$nhTrt == 0][1:2]),
                                      digits = 2))

sqrt((data$emply_rate[1] - data$emply_rate[3])^2) + sqrt((data$emply_rate[1] - data$emply_rate[4])^2) +
  sqrt((data$emply_rate[2] - data$emply_rate[3])^2) + sqrt((data$emply_rate[2] - data$emply_rate[4])^2)

sqrt((data$emply_rate[1] - data$emply_rate[3])^2) + sqrt((data$emply_rate[2] - data$emply_rate[4])^2)

sqrt((data$emply_rate[1] - data$emply_rate[4])^2) + sqrt((data$emply_rate[2] - data$emply_rate[3])^2)


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
length(unique(med_data$fm))

set_size_ests <- group_by(.data = med_data, fm ) %>%
  summarise( n = n(),
             diff_in_means = mean(HomRate08[nhTrt == 1]) - mean(HomRate08[nhTrt == 0]))

round(x = sum(set_size_ests$n/sum(set_size_ests$n) * set_size_ests$diff_in_means), digits = 2)

