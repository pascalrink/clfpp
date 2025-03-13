
rm(list = ls())
graphics.off()
cat("\014")


library(ggplot2)
library(ggpattern)
library(dplyr)
library(patchwork)


source("4-plots/myggsave-function.R")


df <- readRDS("8-sim-results/sim-results-df-250304-1549.RDS")
df$method <- case_match(df$method, 
                        "clopper-pearson" ~ "CP", 
                        "clopper-pearson_sidak" ~ "CP w/ Sidak", 
                        "mabt" ~ "MABT", 
                        "wald" ~ "Wald", 
                        "wald_sidak" ~ "Wald w/ Sidak", 
                        "wilson" ~ "Wilson", 
                        "wilson_sidak" ~ "Wilson w/ Sidak")

# all coverage
sink("8-sim-results/results-out/all-coverage.txt")
full_alpha <- 0.05
acceptable_coverage <- (1-full_alpha) - sqrt(full_alpha * (1-full_alpha) / 5000)
acceptable_coverage
conservative_coverage <- (1-full_alpha) + sqrt(full_alpha * (1-full_alpha) / 5000)
conservative_coverage
coverage_tbl <- aggregate(
  covers ~ method + n_eval + rule + classifier + transf, df, mean) %>% 
  rename(coverage = covers)
coverage_tbl$method_raw <- coverage_tbl$method %>% 
  stringr::str_replace(" w/ Sidak", "")
df <- merge(df, coverage_tbl)
df$too_liberal <- df$coverage < acceptable_coverage
df$too_conservative <- df$coverage < conservative_coverage
coverage_tbl$acceptable <- coverage_tbl$coverage > acceptable_coverage
coverage_tbl$conservative <- coverage_tbl$coverage > conservative_coverage
coverage_tbl$coverage <- round(coverage_tbl$coverage, 4)
coverage_tbl
sink()


# all lower bound
sink("8-sim-results/results-out/all-lower-bounds.txt")
bound_tbl <- subset(df, covers == TRUE) %>%
  aggregate(
    bound ~ method + n_eval + rule + classifier + transf, ., median)
bound_tbl$bound <- round(bound_tbl$bound, 3)
bound_tbl
sink()


# all true performance
sink("8-sim-results/results-out/all-true-performance.txt")
truth_tbl <- aggregate(
  groundtruth ~ method + n_eval + rule + classifier + transf, df, median)
truth_tbl$groundtruth <- round(truth_tbl$groundtruth, 3)
truth_tbl
sink()


# all tightness
sink("8-sim-results/results-out/all-tightness.txt")
tightn_tbl <- subset(df, covers == TRUE) %>% 
  aggregate(
    tightn ~ method + n_eval + rule + classifier + transf, ., median)
tightn_tbl$tightn <- round(tightn_tbl$tightn, 3)
tightn_tbl
sink()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# transformation comparison lasso ecdf vs pnorm ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sink("8-sim-results/results-out/transf-comp-ecdf-vs-pnorm.txt")
df_transf <- subset(df, classifier == "lasso" & method == "MABT")

# overall 
aggregate(covers ~ transf + n_eval, df_transf, mean)

subset(df_transf, covers == TRUE) %>% 
  aggregate(bound ~ transf + n_eval, ., summary, digits = 3)

# n_eval_f_labels <- c("50" = "50 evaluation samples", 
#                      "100" = "100 evaluation samples")
n_eval_f_labels <- c("100" = "100 evaluation samples")
df_transf$n_eval_f <- factor(df_transf$n_eval)
ecdf_v_pnorm_bounds_plot <- subset(df_transf, covers == TRUE) %>% 
  ggplot(aes(x = transf, y = bound)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.55, 0.815)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.02)) + 
  scale_x_discrete(labels = c("Empirical", "Normal")) + 
  # facet_wrap(~ n_eval_f, scales = "free", ncol = 2, labeller = labeller(n_eval_f = n_eval_f_labels)) +
  theme_minimal() + 
  labs(x = element_blank(), y = "Lower limit") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        # axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 13),
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/transf-comp-ecdf-vs-pnorm-overall-cover-bounds.eps", last_plot())


df_transf_ecdf <- subset(df_transf, transf == "ecdf") %>% 
  select(c(bound, n_eval, seed, tightn, covers)) %>% 
  arrange(n_eval, as.numeric(seed))
df_transf_pnorm <- subset(df_transf, transf == "pnorm") %>% 
  select(c(bound, n_eval, seed, tightn, covers)) %>% 
  arrange(n_eval, as.numeric(seed))

names(df_transf_ecdf) <- names(df_transf_ecdf) %>% paste0("_1")
names(df_transf_pnorm) <- names(df_transf_pnorm) %>% paste0("_2")

# df_transf_ecdf <- rename(df_transf_ecdf, seed = seed_1)
# df_transf_pnorm <- rename(df_transf_pnorm, seed = seed_2)
# df_combined <- merge(df_transf_ecdf, df_transf_pnorm)

if (min(df_transf_ecdf$n_eval_1 == df_transf_pnorm$n_eval_2) == 1 & 
    min(df_transf_ecdf$seed_1 == df_transf_pnorm$seed_2) == 1) {
  df_transf <- cbind(df_transf_ecdf, df_transf_pnorm)
} else {
  stop("Sorting failed")
}

######## ecdf covers ----
# subs_df <- subset(df_transf, covers_1 == TRUE)
# subs_df[subs_df$covers_2 == FALSE, ]$bound_2 <- NROW(subs_df[subs_df$covers_2 == FALSE, ]) %>% rep(0, .)
# 
# subs_df$diff <- subs_df$bound_1 - subs_df$bound_2
# aggregate(diff > 0 ~ n_eval_1, subs_df, mean)
# aggregate(diff >= 0 ~ n_eval_1, subs_df, mean)
# 
# ######## normal covers -----
# subs_df <- subset(df_transf, covers_2 == TRUE)
# subs_df[subs_df$covers_1 == FALSE, ]$bound_1 <- NROW(subs_df[subs_df$covers_1 == FALSE, ]) %>% rep(0, .)
# 
# subs_df$diff <- subs_df$bound_1 - subs_df$bound_2
# aggregate(diff > 0 ~ n_eval_1, subs_df, mean)
# aggregate(diff >= 0 ~ n_eval_1, subs_df, mean)

# all bounds comparison
df_transf$diff <- df_transf$bound_1 - df_transf$bound_2

# subset(df_transf, covers_1 == TRUE & covers_2 == TRUE) %>% 
#   aggregate(diff > 0 ~ n_eval_1, ., mean)
# 
# subset(df_transf, covers_1 == TRUE & covers_2 == TRUE) %>% 
#   aggregate(diff >= 0 ~ n_eval_1, ., mean)

# n_eval_1_labels <- c("50" = "50 evaluation samples", 
#                      "100" = "100 evaluation samples")
n_eval_1_labels <- c("100" = "100 evaluation samples")
df_transf$n_eval_1 <- factor(df_transf$n_eval_1)

# subset(df_transf, covers_1 == TRUE & covers_2 == TRUE) %>%
# ggplot(df_transf, aes(y = diff)) + 
#   geom_boxplot() + 
#   coord_cartesian(ylim = c(-0.003, 0.03)) + 
#   scale_y_continuous(breaks = seq(-0.02, 0.12, 0.01)) + 
#   facet_wrap(~ n_eval_1, scales = "free", ncol = 2, labeller = labeller(n_eval_1 = n_eval_1_labels)) +
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         axis.text.x = element_blank(), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/transf-comp-ecdf-vs-pnorm-all-bounds.eps", last_plot())

# only covering comparison
df_transf <- subset(df_transf, covers_1 == TRUE & covers_2 == TRUE)

aggregate(diff > 0 ~ n_eval_1, df_transf, mean)
aggregate(diff >= 0 ~ n_eval_1, df_transf, mean)

aggregate(diff ~ n_eval_1, df_transf, summary, digits = 3)
aggregate(diff ~ n_eval_1, df_transf, summary, digits = 3)

ecdf_v_pnorm_diff_plot <- ggplot(df_transf, aes(x = factor(1), y = diff)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(-0.003, 0.021)) + 
  scale_y_continuous(breaks = seq(-0.02, 0.08, 0.01), minor_breaks = seq(-0.02, 0.08, 0.002)) + 
  scale_x_discrete(labels = c(expression("Empirical" ~ -~ "normal"))) + 
  # facet_wrap(~ n_eval_1, scales = "free", ncol = 2, labeller = labeller(n_eval_1 = n_eval_1_labels)) +
  theme_minimal() + 
  labs(x = element_blank(), y = "Lower limits difference") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        # axis.text.x = element_blank(),
        axis.title.y = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/transf-comp-ecdf-vs-pnorm-covering-bounds.eps", last_plot())
sink()

# ecdf_v_pnorm_bounds_plot
# ecdf_v_pnorm_diff_plot

(ecdf_v_pnorm_bounds_plot | ecdf_v_pnorm_diff_plot)
.myggsave("8-sim-results/results-out/ecdf-v-pnorm-plots.png", last_plot())

rm(ecdf_v_pnorm_bounds_plot, ecdf_v_pnorm_diff_plot)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# lasso results ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

lasso_cvg_tbl <- subset(coverage_tbl, classifier == "lasso" & transf != "pnorm")
lasso_cvg_tbl <- lasso_cvg_tbl %>%
  mutate(facet = paste(rule, n_eval, sep = "-"))
facet_order <- c("best-50", "se-50", "best-100", "se-100")
lasso_cvg_tbl$facet <- factor(lasso_cvg_tbl$facet, levels = facet_order)
lasso_cvg_tbl_summary <- lasso_cvg_tbl %>% dplyr::count(facet)
lasso_cvg_tbl_filtered <- lasso_cvg_tbl %>%
  dplyr::semi_join(lasso_cvg_tbl_summary %>% filter(n > 0), by = "facet")

# facet_labels <- c("best-50" = "default pipeline, 50 evaluation samples",
#                   "se-50" = "proposed pipeline, 50 evaluation samples",
#                   "best-100" = "default pipeline, 100 evaluation samples",
#                   "se-100" = "proposed pipeline, 100 evaluation samples")
facet_labels <- c("best-100" = "default pipeline, 100 evaluation samples",
                  "se-100" = "proposed pipeline, 100 evaluation samples")

# ggplot(lasso_cvg_tbl, aes(x = method_raw, y = coverage)) +
#   geom_bar(stat = "identity") + 
#   coord_cartesian(ylim = c(0.92, 1)) +
#   scale_y_continuous(breaks = seq(0, 1, 0.05), minor_breaks = seq(0, 1, 0.01)) +
#   facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
#   geom_text(aes(label = substr(round(coverage, 3), 2, 5)), vjust = -1, size = 5) + 
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/lasso-barplot-coverage.eps", last_plot())

lasso_sim_coverage_plot <- ggplot(lasso_cvg_tbl, aes(x = method_raw, y = coverage, shape = facet)) + 
  geom_rect(aes(ymin = acceptable_coverage, ymax = conservative_coverage, xmin = -Inf, xmax = Inf), 
            fill = "lightgrey", alpha = 0.2) + 
  geom_point(size = 4) + 
  scale_shape_manual(values = c(17, 16)) + 
  # geom_hline(yintercept = conservative_coverage, linetype = 2) + 
  # geom_hline(yintercept = acceptable_coverage, linetype = 2) + 
  scale_y_continuous(breaks = seq(0, 2, 0.01), minor_breaks = seq(0, 2, 0.01)) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "Coverage probability") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
  # geom_text(aes(label = substr(round(coverage, 3), 2, 5)), hjust = -.15, vjust = 1.5, size = 5) 
# lasso_sim_coverage_plot


lasso_df <- subset(df, classifier == "lasso" & transf != "pnorm") %>% 
  mutate(facet = paste(rule, n_eval, sep = "-"))
lasso_df$facet <- factor(lasso_df$facet, levels = facet_order)
lasso_df_summary <- lasso_df %>% dplyr::count(facet)
lasso_df_filtered <- lasso_df %>% dplyr::semi_join(lasso_df_summary %>% filter(n > 0), by = "facet")


# subset(lasso_df_filtered, covers == TRUE) %>% 
#   ggplot(aes(x = method_raw, y = bound)) + 
#   geom_boxplot() + 
#   coord_cartesian(ylim = c(0.5, 0.81)) +
#   scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.02)) +
#   facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/lasso-boxplot-bound.eps", last_plot())

lasso_sim_bounds_plot <- subset(lasso_df_filtered, covers == TRUE) %>% 
  ggplot(aes(x = method_raw, y = bound, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  scale_pattern_manual(values = c("best" = "none", 
                                  "se" = "circle")) + 
  coord_cartesian(ylim = c(0.5, 0.81)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.02)) +
  # facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "Lower limit") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        axis.title.y = element_text(size = 13), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
# lasso_sim_bounds_plot

sink("8-sim-results/results-out/lasso-bound.txt")
subset(lasso_df_filtered, covers == TRUE) %>% 
  aggregate(bound ~ method_raw + rule + n_eval, ., summary, digits = 4)
sink()


# subset(lasso_df_filtered, covers == TRUE) %>% 
#   ggplot(aes(x = method_raw, y = tightn)) + 
#   geom_boxplot() + 
#   coord_cartesian(ylim = c(0, 0.3)) +
#   scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.02)) +
#   facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
#   theme_minimal() +
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/lasso-boxplot-tightness.eps", last_plot())

lasso_sim_tightn_plot <- subset(lasso_df_filtered, covers == TRUE) %>% 
  ggplot(aes(x = method_raw, y = tightn, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  scale_pattern_manual(values = c("best" = "none", 
                                  "se" = "circle")) + 
  coord_cartesian(ylim = c(0, 0.3)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.02)) +
  # facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "Tightness") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
# lasso_sim_tightn_plot

sink("8-sim-results/results-out/lasso-tightn.txt")
subset(lasso_df_filtered, covers == TRUE) %>% 
  aggregate(tightn ~ method_raw + rule + n_eval, ., summary)
sink()


# gt_facet_labels <- c("150" = "150 learning samples",
#                      "300" = "300 learning samples")
# subset(lasso_df_filtered, method_raw == "Wald") %>% 
#   mutate(n_learn = 3 * n_eval) %>% 
#   ggplot(aes(x = rule, y = groundtruth)) + 
#   geom_boxplot() + 
#   coord_cartesian(ylim = c(0.6585, 0.826)) + 
#   scale_y_continuous(breaks = seq(0.5, 1, 0.05), minor_breaks = seq(0.5, 1, 0.02)) + 
#   # facet_wrap(~ n_learn, scales = "free", labeller = labeller(n_learn = gt_facet_labels)) + 
#   scale_x_discrete(labels = c("best" = "Default", "se" = "Proposed")) + 
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/lasso-boxplot-truth.eps", last_plot())

lasso_sim_gt_plot <- subset(lasso_df_filtered, method_raw == "Wald") %>% 
  mutate(n_learn = 3 * n_eval) %>% 
  ggplot(aes(x = rule, y = groundtruth, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  scale_pattern_manual(values = c("best" = "none", 
                                  "se" = "circle")) + 
  coord_cartesian(ylim = c(0.6585, 0.826)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.05), minor_breaks = seq(0, 1, 0.01)) +
  # facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
  scale_x_discrete(labels = c("best" = "Default", "se" = "Proposed")) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "True performance") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
# lasso_sim_gt_plot

sink("8-sim-results/results-out/lasso-truth.txt")
subset(lasso_df_filtered, method_raw == "Wald") %>% 
  mutate(n_learn = 3 * n_eval) %>% 
  aggregate(groundtruth ~ rule + n_learn, ., summary, digits = 3)
sink()

(lasso_sim_coverage_plot | lasso_sim_gt_plot) / (lasso_sim_bounds_plot | lasso_sim_tightn_plot)
.myggsave("8-sim-results/results-out/lasso-sim-plots.png", last_plot())

rm(lasso_sim_coverage_plot, lasso_sim_bounds_plot, lasso_sim_tightn_plot, lasso_sim_gt_plot)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# random forest results ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rf_cvg_tbl <- subset(coverage_tbl, classifier == "random forest")
rf_cvg_tbl <- rf_cvg_tbl %>%
  mutate(facet = paste(rule, n_eval, sep = "-"))
# facet_order <- c("best-50", "ten-50", "best-100", "ten-100")
facet_order <- c("best-100", "ten-100")
rf_cvg_tbl$facet <- factor(rf_cvg_tbl$facet, levels = facet_order)
rf_cvg_tbl_summary <- rf_cvg_tbl %>% dplyr::count(facet)
rf_cvg_tbl_filtered <- rf_cvg_tbl %>%
  dplyr::semi_join(rf_cvg_tbl_summary %>% filter(n > 0), by = "facet")

# facet_labels <- c("best-50" = "default pipeline, 50 evaluation samples",
#                   "ten-50" = "proposed pipeline, 50 evaluation samples",
#                   "best-100" = "default pipeline, 100 evaluation samples",
#                   "ten-100" = "proposed pipeline, 100 evaluation samples")
facet_labels <- c("best-100" = "default pipeline, 100 evaluation samples",
                  "ten-100" = "proposed pipeline, 100 evaluation samples")

# ggplot(rf_cvg_tbl, aes(x = method_raw, y = coverage)) +
#   geom_bar(stat = "identity") + 
#   coord_cartesian(ylim = c(0.92, 1.0)) + 
#   scale_y_continuous(breaks = seq(0, 2, 0.05), minor_breaks = seq(0, 2, 0.01)) +
#   facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
#   geom_text(aes(label = substr(round(coverage, 3), 2, 5)), vjust = -1, size = 5) + 
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/rf-barplot-coverage.eps", last_plot())

# ggplot(rf_cvg_tbl, aes(x = method_raw, y = coverage, shape = facet)) + 
#   geom_point(size = 4) + 
#   geom_hline(yintercept = conservative_coverage, linetype = 2, size = 1) + 
#   geom_hline(yintercept = acceptable_coverage, linetype = 2, size = 1) + 
#   scale_y_continuous(breaks = seq(0, 2, 0.01), minor_breaks = seq(0, 2, 0.01)) + 
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"), 
#         legend.position = "none") + 
#   geom_text(aes(label = substr(round(coverage, 3), 2, 5)), hjust = -.2, vjust = 1, size = 5) 
# .myggsave("8-sim-results/results-out/rf-barplot-coverage.eps", last_plot())

# rf_cvg_tbl$acceptable <- acceptable_coverage
# rf_cvg_tbl$conservative <- conservative_coverage
rf_sim_coverage_plot <- ggplot(rf_cvg_tbl, aes(x = method_raw, y = coverage, shape = rule)) + 
  geom_rect(aes(ymin = acceptable_coverage, ymax = conservative_coverage, xmin = -Inf, xmax = Inf), 
            fill = "lightgrey", alpha = 0.2) + 
  geom_point(size = 4) + 
  scale_shape_manual(values = c(17, 16)) + 
  # geom_hline(yintercept = conservative_coverage, linetype = 2) + 
  # geom_hline(yintercept = acceptable_coverage, linetype = 2) + 
  scale_y_continuous(breaks = seq(0, 2, 0.01), minor_breaks = seq(0, 2, 0.01)) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "Coverage probability") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
# geom_text(aes(label = substr(round(coverage, 3), 2, 5)), hjust = -.15, vjust = 1.5, size = 5) 
# rf_sim_coverage_plot


rf_df <- subset(df, classifier == "random forest") %>% 
  mutate(facet = paste(rule, n_eval, sep = "-"))
rf_df$facet <- factor(rf_df$facet, levels = facet_order)
rf_df_summary <- rf_df %>% dplyr::count(facet)
rf_df_filtered <- rf_df %>% dplyr::semi_join(rf_df_summary %>% filter(n > 0), by = "facet")


# subset(rf_df_filtered, covers == TRUE) %>% 
#   ggplot(aes(x = method_raw, y = bound)) + 
#   geom_boxplot() + 
#   coord_cartesian(ylim = c(0.45, 0.76)) + 
#   scale_y_continuous(breaks = seq(0, 1, 0.2), minor_breaks = seq(0, 1, 0.02)) +
#   facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/rf-boxplot-bound.eps", last_plot())

rf_sim_bounds_plot <- subset(rf_df_filtered, covers == TRUE) %>% 
  ggplot(aes(x = method_raw, y = bound, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  scale_pattern_manual(values = c("best" = "none", 
                                  "ten" = "circle")) + 
  coord_cartesian(ylim = c(0.45, 0.76)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.02)) +
  # facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "Lower limit") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        axis.title.y = element_text(size = 13), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
# rf_sim_bounds_plot


sink("8-sim-results/results-out/rf-bound.txt")
subset(rf_df_filtered, covers == TRUE) %>% 
  aggregate(bound ~ method_raw + rule + n_eval, ., summary, digits = 3)
sink()


# subset(rf_df_filtered, covers == TRUE) %>% 
#   ggplot(aes(x = method_raw, y = tightn)) + 
#   geom_boxplot() + 
#   coord_cartesian(ylim = c(0, 0.25)) +
#   scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.02)) +
#   facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
#   theme_minimal() +
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/rf-boxplot-tightn.eps", last_plot())

rf_sim_tightn_plot <- subset(rf_df_filtered, covers == TRUE) %>% 
  ggplot(aes(x = method_raw, y = tightn, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  scale_pattern_manual(values = c("best" = "none", 
                                  "ten" = "circle")) + 
    coord_cartesian(ylim = c(0, 0.255)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.02)) +
  # facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "Tightness") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
# rf_sim_tightn_plot


sink("8-sim-results/results-out/rf-tightn.txt")
subset(rf_df_filtered, covers == TRUE) %>% 
  aggregate(tightn ~ method_raw + rule + n_eval, ., summary, digits = 3)
sink()


# # gt_facet_labels <- c("150" = "150 learning samples",
# #                      "300" = "300 learning samples")
# gt_facet_labels <- c("300" = "300 learning samples")
# subset(rf_df_filtered, method_raw == "Wald") %>% 
#   mutate(n_learn = 3 * n_eval) %>% 
#   ggplot(aes(x = rule, y = groundtruth)) + 
#   geom_boxplot() + 
#   coord_cartesian(ylim = c(0.653, 0.787)) + 
#   scale_y_continuous(breaks = seq(0.5, 0.8, 0.02), minor_breaks = seq(0.5, 0.8, 0.01)) + 
#   facet_wrap(~ n_learn, scales = "free", labeller = labeller(n_learn = gt_facet_labels)) + 
#   scale_x_discrete(labels = c("best" = "default", "ten" = "proposed")) + 
#   theme_minimal() + 
#   labs(x = element_blank(), y = element_blank()) + 
#   theme(strip.text = element_text(size = 13), 
#         axis.text = element_text(size = 13), 
#         panel.grid.major.x = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA), 
#         panel.spacing = unit(1, "lines"))
# .myggsave("8-sim-results/results-out/rf-boxplot-truth.eps", last_plot())

rf_sim_gt_plot <- subset(rf_df_filtered, method_raw == "Wald") %>% 
  mutate(n_learn = 3 * n_eval) %>% 
  ggplot(aes(x = rule, y = groundtruth, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  scale_pattern_manual(values = c("best" = "none", 
                                  "ten" = "circle")) + 
  coord_cartesian(ylim = c(0.653, 0.787)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.05), minor_breaks = seq(0, 1, 0.01)) +
  # facet_wrap(~ facet, scales = "free", ncol = 2, labeller = labeller(facet = facet_labels)) + 
  scale_x_discrete(labels = c("best" = "Default", "ten" = "Proposed")) + 
  theme_minimal() + 
  labs(x = element_blank(), y = "True performance") + 
  theme(strip.text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        panel.spacing = unit(1, "lines"), 
        legend.position = "none")
# rf_sim_gt_plot

sink("8-sim-results/results-out/rf-truth.txt")
subset(rf_df_filtered, method_raw == "Wald") %>% 
  mutate(n_learn = 3 * n_eval) %>% 
  aggregate(groundtruth ~ rule + n_learn, ., summary, digits = 3)
sink()

(rf_sim_coverage_plot | rf_sim_gt_plot) / (rf_sim_bounds_plot | rf_sim_tightn_plot)
.myggsave("8-sim-results/results-out/rf-sim-plots.png", last_plot())

rm(rf_sim_coverage_plot, rf_sim_bounds_plot, rf_sim_tightn_plot, rf_sim_gt_plot)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
