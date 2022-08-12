setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyverse)
library(magrittr)
library(pbapply)
library(parallel)
library(haven)

DID_data <- data.frame(mean_outcome = c(NA, NA, 70, 80,
                                        50, 60, 70, 100,
                                        10, 20, 30, 40),
                       Treatment = rep(x = c("Treated counterfactual trend", "Treated", "Control"), each = 4),
                       Period = rep(x = c(-1, 0, 1, 2), times = 3))
DID_data$Treatment <- factor(x =DID_data$Treatment,
                             levels = c("Treated", "Control", "Treated counterfactual trend"))

DID_plot <- ggplot(data = filter(.data = DID_data,
                                 Treatment != "Treated counterfactual trend"),
                      mapping = aes(x = Period,
                                    y = mean_outcome,
                                    group = Treatment)) +
  geom_point(data = subset(x = DID_data,
                           subset = Treatment %in% c("Treated",
                                                     "Control"))) +
  geom_line(mapping = aes(linetype = Treatment)) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  xlab("Time") +
  ylab("Mean outcome") +
  scale_x_continuous(breaks = -1:2, labels = c("Before", "Before", "Before", "After")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right")

ggsave(plot = DID_pt_plot,
       file = "DID_pt_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

DID_pt_plot <- ggplot(data = DID_data,
                       mapping = aes(x = Period,
                                     y = mean_outcome,
                                     group = Treatment)) +
  geom_point(data = subset(x = DID_data,
                           subset = Treatment %in% c("Treated",
                                                     "Control"))) +
  geom_line(mapping = aes(linetype = Treatment)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  xlab("Time") +
  ylab("Mean outcome") +
  scale_x_continuous(breaks = -1:2, labels = c("Before", "Before", "Before", "After")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right")

ggsave(plot = DID_pt_plot,
       file = "DID_pt_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)


ATT <- exp(4) - exp(3.75)
round(x = ATT, digits = 2)
DID_est <- exp(4) - exp(3.5) - (exp(.76) - exp(.51))
round(x = DID_est, digits = 2)
ATT - DID_est


standard_scale_DID_data <- data.frame(mean_outcome = c(NA, NA, exp(3.5), exp(3.75),
                                                       exp(3), exp(3.25), exp(3.5), exp(4),
                                                       exp(.1), exp(.26), exp(.51), exp(.76)),
                                       Treatment = rep(x = c("Treated counterfactual trend", "Treated", "Control"), each = 4),
                                       Period = rep(x = c(-1, 0, 1, 2), times = 3))
standard_scale_DID_data$Treatment <- factor(x = standard_scale_DID_data$Treatment,
                                            levels = c("Treated", "Control", "Treated counterfactual trend"))

standard_scale_DID_plot <- ggplot(data = standard_scale_DID_data,
                             mapping = aes(x = Period,
                                           y = mean_outcome,
                                           group = Treatment)) +
  geom_point(data = subset(x = standard_scale_DID_data,
                           subset = Treatment %in% c("Treated",
                                                     "Control"))) +
  geom_line(mapping = aes(linetype = Treatment)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  xlab("Time") +
  ylab("Mean outcome") +
  scale_x_continuous(breaks = -1:2, labels = c("Before", "Before", "Before", "After")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60)) +
  theme_bw() +
  ggtitle(label = "Standard scale") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right")

#ggsave(plot = standard_scale_DID_plot,
#       file = "standard_scale_DID_plot.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)

log_scale_ATT <- log(exp(4)) - log(exp(3.75))
round(x = log_scale_ATT, digits = 2)
log_scale_DID_est <- log(exp(4)) - log(exp(3.5)) - (log(exp(.76)) - log(exp(.51)))
round(x = log_scale_DID_est, digits = 2)
log_scale_ATT - log_scale_DID_est

log_scale_DID_data <- data.frame(mean_outcome = c(NA, NA, log(exp(3.5)), log(exp(3.75)),
                                                  log(exp(3)), log(exp(3.25)), log(exp(3.5)), log(exp(4)),
                                                  log(exp(.1)), log(exp(.26)), log(exp(.51)), log(exp(.76))),
                                 Treatment = rep(x = c("Treated counterfactual trend", "Treated", "Control"), each = 4),
                                 Period = rep(x = c(-1, 0, 1, 2), times = 3))
log_scale_DID_data$Treatment <- factor(x = log_scale_DID_data$Treatment,
                                       levels = c("Treated", "Control", "Treated counterfactual trend"))

log_scale_DID_plot <- ggplot(data = log_scale_DID_data,
                             mapping = aes(x = Period,
                                           y = mean_outcome,
                                           group = Treatment)) +
  geom_point(data = subset(x = log_scale_DID_data,
                           subset = Treatment %in% c("Treated",
                                                     "Control"))) +
  geom_line(mapping = aes(linetype = Treatment)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  xlab("Time") +
  ylab("Mean outcome") +
  scale_x_continuous(breaks = -1:2, labels = c("Before", "Before", "Before", "After")) +
  theme_bw() +
  ggtitle(label = "Log scale") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right")

#ggsave(plot = log_scale_DID_plot,
#       file = "log_scale_DID_plot.pdf",
#       width = 6,
#       height = 4,
#       units = "in",
#       dpi = 600)
