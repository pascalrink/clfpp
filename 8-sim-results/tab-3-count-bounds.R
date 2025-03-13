
# This script produces the numbers in Table 3 in the main document (percentage 
# of MABT bounds that outperform the comparators in a per-data set comparison in 
# the lasso simulations)

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)
library(ggpattern)

cis_path <- "1-savefiles/cis/class/normal/"
cis_files <- list.files(cis_path, recursive = TRUE)

GenTable <- function(tbl_measure, tbl_feats_type, tbl_n_eval, tbl_rule, tbl_cv) {
  
  paste(tbl_measure, tbl_feats_type, "n_eval =", tbl_n_eval, "rule =", tbl_rule, "cv =", tbl_cv)
  
  df_list <- lapply(cis_files, function(.fn) {
    .df <- paste0(cis_path, .fn) %>% 
      readRDS
    .df <- .df[which(.df$covers), ]
    # .df <- subset(.df, feats_type == tbl_feats_type & cv == tbl_cv & (method == "mabt" & rule == tbl_rule | rule == "best") & n_eval == tbl_n_eval)
    .df <- subset(.df, n_eval == tbl_n_eval)
    .df <- subset(.df, measure == tbl_measure)
    if (nrow(.df) == 0) {return(NULL)}
    .df <- subset(.df, select = c(method, rule, bound, n_eval, cv, measure, feats_type))
    return(.df)
  })
  
  .GenCompTable <- function(.lelem) {
    .lelem$rank <- rank(1-.lelem$bound, ties.method = "max")
    Y <- expand.grid(.lelem$rank, .lelem$rank)
    methods <- paste(.lelem$method, .lelem$rule, .lelem$n_eval, .lelem$cv, .lelem$measure, sep = "+")
    n_methods <- length(methods)
    Y$method1 <- rep(methods, each = n_methods)
    Y$method2 <- rep(methods, times = n_methods)
    Y <- Y[Y$method1 != Y$method2, ]
    Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0 
    return(Y)
  }
  
  table_list <- lapply(df_list, .GenCompTable)
  table_df <- do.call(rbind, table_list)
  table_df$comparison <- paste0(table_df$method1, " > ", table_df$method2)
  
  table_sum <- aggregate(lbtr ~ comparison, table_df, sum)
  table_mean <- aggregate(lbtr ~ comparison, table_df, mean)
  
  mts <- table_sum[substr(table_sum$comparison, 1, 4) == "mabt", ]
  mtm <- table_mean[substr(table_mean$comparison, 1, 4) == "mabt", ]
  
  # aufsteigend ordnen
  # mts[order(mts$lbtr), ]
  return(list(mtm = mtm[order(mtm$lbtr), ], 
              mts = mts[order(mts$lbtr), ]))
  
}

out100 <- GenTable("class", "normal", 100, "se", TRUE)


GenTableGEQ <- function(tbl_measure, tbl_feats_type, tbl_n_eval, tbl_rule, tbl_cv) {
  
  paste(tbl_measure, tbl_feats_type, "n_eval =", tbl_n_eval, "rule =", tbl_rule, "cv =", tbl_cv)
  
  df_list <- lapply(cis_files, function(.fn) {
    .df <- paste0(cis_path, .fn) %>% 
      readRDS
    .df <- .df[which(.df$covers), ]
    # .df <- subset(.df, feats_type == tbl_feats_type & cv == tbl_cv & (method == "mabt" & rule == tbl_rule | rule == "best") & n_eval == tbl_n_eval)
    .df <- subset(.df, n_eval == tbl_n_eval)
    .df <- subset(.df, measure == tbl_measure)
    if (nrow(.df) == 0) {return(NULL)}
    .df <- subset(.df, select = c(method, rule, bound, n_eval, cv, measure, feats_type))
    return(.df)
  })
  
  .GenCompTable <- function(.lelem) {
    .lelem$rank <- rank(1-.lelem$bound, ties.method = "max")
    Y <- expand.grid(.lelem$rank, .lelem$rank)
    methods <- paste(.lelem$method, .lelem$rule, .lelem$n_eval, .lelem$cv, .lelem$measure, sep = "+")
    n_methods <- length(methods)
    Y$method1 <- rep(methods, each = n_methods)
    Y$method2 <- rep(methods, times = n_methods)
    Y <- Y[Y$method1 != Y$method2, ]
    Y$lbtr <- (Y$Var1 >= Y$Var2) * 1.0 
    return(Y)
  }
  
  table_list <- lapply(df_list, .GenCompTable)
  table_df <- do.call(rbind, table_list)
  table_df$comparison <- paste0(table_df$method1, " >= ", table_df$method2)
  
  table_sum <- aggregate(lbtr ~ comparison, table_df, sum)
  table_mean <- aggregate(lbtr ~ comparison, table_df, mean)
  
  mts <- table_sum[substr(table_sum$comparison, 1, 4) == "mabt", ]
  mtm <- table_mean[substr(table_mean$comparison, 1, 4) == "mabt", ]
  
  # aufsteigend ordnen
  # mts[order(mts$lbtr), ]
  return(list(mtm = mtm[order(mtm$lbtr), ], 
              mts = mts[order(mts$lbtr), ]))
  
}


out100_geq <- GenTableGEQ("class", "normal", 100, "se", TRUE)





sink("8-sim-results/results-out/lasso-pairwise-bounds-n400.txt")
out100$mtm
# comparison      lbtr
# 15            mabt+se+100+TRUE+class > wald+best+100+TRUE+class 0.3401213
# 17          mabt+se+100+TRUE+class > wilson+best+100+TRUE+class 0.5472700
# 13 mabt+se+100+TRUE+class > clopper-pearson+best+100+TRUE+class 0.6702646
# 16              mabt+se+100+TRUE+class > wald+se+100+TRUE+class 0.8999157
# 18            mabt+se+100+TRUE+class > wilson+se+100+TRUE+class 0.9997900
# 14   mabt+se+100+TRUE+class > clopper-pearson+se+100+TRUE+class 1.0000000

out100_geq$mtm
# 37            mabt+se+100+TRUE+class >= wald+best+100+TRUE+class 0.3172264
# 33              mabt+se+100+TRUE+class >= bt+best+100+TRUE+class 0.5184082
# 39          mabt+se+100+TRUE+class >= wilson+best+100+TRUE+class 0.5448949
# 35 mabt+se+100+TRUE+class >= clopper-pearson+best+100+TRUE+class 0.6716008
# 38              mabt+se+100+TRUE+class >= wald+se+100+TRUE+class 0.8777918
# 34                mabt+se+100+TRUE+class >= bt+se+100+TRUE+class 0.9989509
# 40            mabt+se+100+TRUE+class >= wilson+se+100+TRUE+class 0.9991607
# 36   mabt+se+100+TRUE+class >= clopper-pearson+se+100+TRUE+class 1.0000000

out100$mts
# 37            mabt+se+100+TRUE+class > wald+best+100+TRUE+class 1464
# 33              mabt+se+100+TRUE+class > bt+best+100+TRUE+class 2195
# 39          mabt+se+100+TRUE+class > wilson+best+100+TRUE+class 2567
# 35 mabt+se+100+TRUE+class > clopper-pearson+best+100+TRUE+class 3176
# 38              mabt+se+100+TRUE+class > wald+se+100+TRUE+class 4166
# 34                mabt+se+100+TRUE+class > bt+se+100+TRUE+class 4520
# 40            mabt+se+100+TRUE+class > wilson+se+100+TRUE+class 4762
# 36   mabt+se+100+TRUE+class > clopper-pearson+se+100+TRUE+class 4766

out100_geq$mts
# 37            mabt+se+100+TRUE+class >= wald+best+100+TRUE+class 1464
# 33              mabt+se+100+TRUE+class >= bt+best+100+TRUE+class 2436
# 39          mabt+se+100+TRUE+class >= wilson+best+100+TRUE+class 2567
# 35 mabt+se+100+TRUE+class >= clopper-pearson+best+100+TRUE+class 3176
# 38              mabt+se+100+TRUE+class >= wald+se+100+TRUE+class 4166
# 34                mabt+se+100+TRUE+class >= bt+se+100+TRUE+class 4761
# 40            mabt+se+100+TRUE+class >= wilson+se+100+TRUE+class 4762
# 36   mabt+se+100+TRUE+class >= clopper-pearson+se+100+TRUE+class 4766
sink()



