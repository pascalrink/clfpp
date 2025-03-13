
# This script produces the numbers in Table 4 in the main document (percentage 
# of final models from the proposed pipeline that outperform the final model 
# from the default pipeline in a per-data set comparison in the lasso 
# simulations)

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)
library(ggpattern)

# cis_path <- "1-savefiles/cis/rf/n200/"
# cis_files <- list.files(cis_path, recursive = TRUE)
# 
# df_list <- lapply(cis_files, function(.fn) {
#   .df <- paste0(cis_path, .fn) %>% 
#     readRDS
#   # .df <- .df[which(.df$covers), ]
#   .df_2 <- data.frame(proposed = .df$proposed$gt, default = .df$default$gt, proposed_id = .df$proposed$id, default_id = .df$default$id)
#   .df_2$proposed_least <- (.df_2$proposed >= .df_2$default) * 1.0
#   .df_2$proposed_strict <- (.df_2$proposed > .df_2$default) * 1.0
#   .df_2$same_id <- (.df_2$proposed_id == .df_2$default_id) * 1.0
#   return(.df_2)
# })
# 
# sink("8-sim-results/results-out/rf-n200-pairwise-gt.txt")
# table_df <- do.call(rbind, df_list)
# print("rf-n200")
# colSums(table_df)
# colMeans(table_df)
# sink()


cis_path <- "1-savefiles/cis/rf/n400/"
cis_files <- list.files(cis_path, recursive = TRUE)

df_list <- lapply(cis_files, function(.fn) {
  .df <- paste0(cis_path, .fn) %>% 
    readRDS
  # .df <- .df[which(.df$covers), ]
  .df_2 <- data.frame(proposed = .df$proposed$gt, default = .df$default$gt, proposed_id = .df$proposed$id, default_id = .df$default$id)
  .df_2$proposed_least <- (.df_2$proposed >= .df_2$default) * 1.0
  .df_2$proposed_strict <- (.df_2$proposed > .df_2$default) * 1.0
  .df_2$same_id <- (.df_2$proposed_id == .df_2$default_id) * 1.0
  return(.df_2)
})

sink("8-sim-results/results-out/rf-n400-pairwise-gt.txt")
table_df <- do.call(rbind, df_list)
print("rf-n400")
colSums(table_df)
colMeans(table_df)
sink()

