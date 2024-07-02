
# Preliminaries -----------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
library(tidyverse)
library(haven)
library(RItools)
library(magrittr)

rdd_data <- read_dta("http://jakebowers.org/Matching/RDReplication.dta") %>%
  filter(Use == 1) ## Use is indicator for whether unit is included in RD incumbency advantage sample


# Treatment assignment mechanism demonstration ----------------------------

cutoff <- 0

treatment_plot <- ggplot(data = rdd_data,
                         mapping = aes(x = DifDPct,
                                       y = DemWin)) +
  geom_line(data = filter(.data = rdd_data,
                          DemWin == 0),
            colour = "red") +
  geom_line(data = filter(.data = rdd_data,
                          DemWin == 1),
            color = "blue") +
  geom_vline(xintercept = cutoff,
             linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(title = "Deterministic Treatment Assignment Mechanism:",
       subtitle = "Close election RDDs",
       x ="Margin of victory (%)",
       y = "Treatment (Win or lose election)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(plot = treatment_plot,
       file = "figures/treatment_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)



# Bandwidth selection -----------------------------------------------------

chi_squared_balance <- function(i,
                                running_var,
                                bal_fmla,
                                data) {
  
  lower_bound <- seq(from = -5, to = -0.1, by = 0.1)
  
  upper_bound <- seq(from = 0.1, to = 5, by = 0.1) %>%
    sort(decreasing = TRUE)
  
  data <- dplyr::filter(.data = data,
                        running_var > lower_bound[i] & running_var < upper_bound[i])
  
  # Effective Sample Size
  ess <- nrow(data)
  
  p_value <- xBalance(
    fmla = bal_fmla,
    data = data,
    report = "chisquare.test"
  )$overall[[3]]
  
  bands <- cbind(ess, p_value, lower_bound[i], upper_bound[i])
  
  return(bands)
}

is <- seq(from = 1,
          to = length(seq(from = -5,
                          to = -0.1,
                          by = 0.1)),
          by = 1)

covs <- matrix(c("DWinPrv", "Dem Win t - 1",
                 "DPctPrv", "Dem % t - 1",
                 "DifDPPrv", "Dem % Margin t - 1",
                 "IncDWNOM1", "Inc's D1 NOMINATE",
                 "DemInc", "Dem Inc in Race",
                 "NonDInc", "Rep Inc in Race",
                 "PrvTrmsD", "Dem's # Prev Terms",
                 "PrvTrmsO", "Rep's # Prev Terms",
                 "RExpAdv", "Rep Experience Adv",
                 "DExpAdv", "Dem Experience Adv",
                 "ElcSwing", "Partisan Swing",
                 "CQRating3", "CQ Rating {-1, 0, 1}",
                 "DSpndPct", "Dem Spending %",
                 "DDonaPct", "Dem Donation %",
                 "SoSDem", "Dem Sec of State",
                 "GovDem", "Dem Governor",
                 "DifPVDec", "Dem Pres % Margin", ## average over decade
                 "DemOpen", "Dem-held Open Seat",
                 "NonDOpen", "Rep-held Open Seat",
                 "OpenSeat", "Open Seat",
                 "VtTotPct", "Voter Turnout %",
                 "GovWkPct", "Pct Gov't Worker",
                 "UrbanPct", "Pct Urban",
                 "BlackPct", "Pct Black",
                 "ForgnPct", "Pct Foreign Born"),
               ncol = 2,
               byrow = TRUE)

dimnames(covs) <- list(seq(
  from = 1,
  to = 25,
  by = 1),
  c("Covariate", "Description"))

bal_fmla <- reformulate(covs[1:25],
                        response = "DemWin")

band_df <- data.frame(t(sapply(X = is,
                               FUN = function(x) { chi_squared_balance(i = x,
                                                                       running_var = rdd_data$DifDPct,
                                                                       bal_fmla = bal_fmla,
                                                                       data = rdd_data) }))) %>%
  rename(ess = X1,
         p_value = X2,
         bandwidth = X4) %>%
  select(ess, p_value, bandwidth)

optimal_band <- max(band_df$bandwidth[band_df$p_value >= 0.5])
cov_bal_bandwidth <- ggplot(data = band_df,
                            mapping = aes(x = bandwidth,
                                          y = p_value)) +
  geom_point() +
  theme_bw() +
  geom_vline(xintercept = optimal_band,
             linetype = "dashed") +
  labs(title = "P-values for tests of covariate balance",
       x ="Bandwidth",
       y = "P-value") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(plot = cov_bal_bandwidth,
       file = "figures/cov_bal_bandwidth.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)


rdd_opt_band_data <- filter(.data = rdd_data,
                            DifDPct > -optimal_band & DifDPct < optimal_band)

rdd_opt_band_data %>% nrow


# Outcome modeling --------------------------------------------------------

tmp_lm <- lm(DPctNxt ~ DifDPct,
             data = rdd_opt_band_data,
             subset = DifDPct < 0)

rdd_opt_band_data <- mutate(.data = rdd_opt_band_data,
                            pred_DPctNxt = predict(object = tmp_lm,
                                                   newdata = rdd_opt_band_data),
                            resid_DPctNxt = rdd_opt_band_data$DPctNxt - pred_DPctNxt)

rdd_plot_data <- data.frame(outcome = c(rdd_opt_band_data$pred_DPctNxt,
                                        rdd_opt_band_data$resid_DPctNxt,
                                        rdd_opt_band_data$DPctNxt),
                            type = c(rep(x = "Predicted",
                                       times = nrow(rdd_opt_band_data)),
                                     rep(x = "Residual",
                                         times = nrow(rdd_opt_band_data)),
                                     rep(x = "Outcome", times = nrow(rdd_opt_band_data))),
                            run_var = c(rdd_opt_band_data$DifDPct,
                                        rdd_opt_band_data$DifDPct,
                                        rdd_opt_band_data$DifDPct))

out_mod_rdd <- ggplot(data = rdd_plot_data,
                      mapping = aes(x = run_var,
                                    y = outcome)) +
  geom_point(data = filter(.data = rdd_plot_data,
                           type == "Outcome")) +
  geom_line(data = filter(.data = rdd_plot_data,
                          type == "Predicted" & run_var < 0),
            linetype = "solid") +
  geom_line(data = filter(.data = rdd_plot_data,
                          type == "Predicted" & run_var > 0),
            linetype = "dotted") +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab("Democratic Margin of Victory (Running Variable)") +
  ylab("Democratic Vote Percentage in Next Election (Outcome)") +
  theme_bw() +
  theme(axis.title = element_text(size = 10))

ggsave(plot = out_mod_rdd,
       file = "figures/out_mod_rdd.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

out_mod_resid_rdd <- ggplot(data = rdd_plot_data,
                      mapping = aes(x = run_var,
                                    y = outcome)) +
  geom_point(data = filter(.data = rdd_plot_data,
                           type == "Residual")) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab("Dem. Margin of Victory (Running Variable)") +
  ylab("Detrended Dem. Vote Percentage in Next Election (Outcome)") +
  theme_bw() +
  theme(axis.title = element_text(size = 8))

ggsave(plot = out_mod_resid_rdd,
       file = "figures/out_mod_resid_rdd.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)