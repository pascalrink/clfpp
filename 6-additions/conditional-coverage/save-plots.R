
rm(list = ls())
graphics.off()
cat("\014")

library(patchwork)

source("4-plots/myggsave-function.R")

alpha_level <- 0.05
cvg_lvl <- 1-alpha_level
delta_cvg <- sqrt(alpha_level * (1-alpha_level) / 1000)

# regular
reg_cvg <- readRDS("6-additions/conditional-coverage/plots/conditional-regular-coverage.RDS")
reg_mabt <- readRDS("6-additions/conditional-coverage/plots/conditional-regular-scatter-mabt.RDS")
reg_bbncv <- readRDS("6-additions/conditional-coverage/plots/conditional-regular-scatter-bbcncv.RDS")

# amplified
amp_cvg <- readRDS("6-additions/conditional-coverage/plots/conditional-amplified-coverage.RDS")
amp_mabt <- readRDS("6-additions/conditional-coverage/plots/conditional-amplified-scatter-mabt.RDS")
amp_bbncv <- readRDS("6-additions/conditional-coverage/plots/conditional-amplified-scatter-bbcncv.RDS")

(reg_cvg) | (amp_cvg)
.myggsave("6-additions/conditional-coverage/conditional-coverage-plot.png", last_plot())

(reg_mabt) / (amp_mabt)
.myggsave("6-additions/conditional-coverage/conditional-scatter-mabt-plot.png", last_plot())

(reg_bbncv) / (amp_bbncv)
.myggsave("6-additions/conditional-coverage/conditional-scatter-bbcnv-plot.png", last_plot())
