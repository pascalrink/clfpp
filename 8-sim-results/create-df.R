
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)


# load lasso results
lasso_path <- '1-savefiles/cis/class/normal/'
lasso_files <- list.files(lasso_path, recursive = TRUE)
lasso_df <- lapply(lasso_files, function(.fn) {
  paste0(lasso_path, .fn) %>% readRDS}) %>% do.call(rbind, .)


# load lasso pnorm results
# pnorm_df <- readRDS("7-more-additions/mabt-with-pnorm-out-200.RDS") %>%
#   do.call(rbind, .) %>%
#   rbind(do.call(rbind, readRDS("7-more-additions/mabt-with-pnorm-out-400.RDS")))

pnorm_df <- readRDS("7-more-additions/mabt-with-pnorm-out-400.RDS") %>%
  do.call(rbind, .)


# load random forest results
# rf_n200_path <- "1-savefiles/cis/rf/n200/"
# rf_n200_files <- list.files(rf_n200_path, recursive = TRUE)
# rf_n200_list <- lapply(
#   rf_n200_files, function(.fn) {paste0(rf_n200_path, .fn) %>% readRDS}) %>% 
#   lapply(function(.lelem) {
#     .lelem$sidak$ci$method[.lelem$sidak$ci$method == "bt"] <- "bt_sidak"
#     return(.lelem)})

# rf_n400_path <- "1-savefiles/cis/rf/n400/"
# rf_n400_files <- list.files(rf_n400_path, recursive = TRUE)
# rf_n400_list <- lapply(
#   rf_n400_files, function(.fn) {paste0(rf_n400_path, .fn) %>% readRDS}) %>% 
#   lapply(function(.lelem) {
#     .lelem$sidak$ci$method[.lelem$sidak$ci$method == "bt"] <- "bt_sidak"
#     return(.lelem)})

rf_n400_path <- "1-savefiles/cis/rf/n400/"
rf_n400_files <- list.files(rf_n400_path, recursive = TRUE)
rf_n400_list <- lapply(
  rf_n400_files, function(.fn) {paste0(rf_n400_path, .fn) %>% readRDS})


# process lasso results
lasso_df <- subset(
  lasso_df, 
  select = c(est, method, bound, groundtruth, covers, n_eval, rule, seed))
lasso_df$classifier <- "lasso"
lasso_df$transf <- "N.A"
lasso_df$transf[lasso_df$method == "mabt"] <- "ecdf"


# process pnorm results
pnorm_df <- subset(
  pnorm_df, 
  select = c(est, method, bound, groundtruth, covers, n_eval, rule, seed))
pnorm_df$classifier <- "lasso"
pnorm_df$transf <- "pnorm"


# process random forest results
RfFormatResultList <- function(rl, args) {
  df <- lapply(rl, function(lelem) {
    lapply(lelem, data.frame) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)
  df$n <- args[[1]]
  return(df)
}

# rf_df <- rbind(RfFormatResultList(rf_n200_list, "200"), 
#                RfFormatResultList(rf_n400_list, "400"))
rf_df <- RfFormatResultList(rf_n400_list, "400")
rf_df <- rename(rf_df, method = ci.method, bound = ci.lwr, groundtruth = gt)
rf_df$covers <- rf_df$bound < rf_df$groundtruth
rf_df$n_eval <- as.numeric(rf_df$n) / 4
rf_df$rule <- NA
best_rule_methods <- c("bt", "clopper-pearson", "wald", "wilson")
rf_df$rule[rf_df$method %in% best_rule_methods] <- "best"
ten_rule_methods <- paste0(best_rule_methods, "_sidak") %>% c("mabt")
rf_df$rule[rf_df$method %in% ten_rule_methods] <- "ten"
rf_df$seed <- "N.A"
rf_df$classifier <- "random forest"
rf_df$transf <- "N.A"
rf_df$transf[rf_df$method == "mabt"] <- "ecdf"

rf_df <- select(rf_df, -c(id, n))


# merge all results
df <- rbind(lasso_df, pnorm_df, rf_df)
df$tightn <- df$groundtruth - df$bound
format(Sys.time(), "%y%m%d-%H%M") %>% paste0("8-sim-results/sim-results-df-", ., ".RDS") %>% saveRDS(df, .)

# ----
GenCompTable <- function(result_list) {
  
  .FormatResults <- function(.lelem) {
    .lelem$default <- data.frame(.lelem$default)
    .lelem$proposed <- data.frame(.lelem$proposed)
    .lelem$sidak <- data.frame(.lelem$sidak)
    df <- do.call(rbind, .lelem)
    df$covers <- df$ci.lwr < df$gt
    df <- df[which(df$covers), ]
    if (nrow(df) == 0) {return(NULL)}
    df <- select(df, c(ci.method, ci.lwr))
    return(df)
  }
  
  .GenCompTable <- function(.lelem) {
    .lelem$rank <- rank(1-.lelem$ci.lwr, ties.method = "max")
    Y <- expand.grid(.lelem$rank, .lelem$rank)
    methods <- .lelem$ci.method
    n_methods <- length(methods)
    Y$method1 <- rep(methods, each = n_methods)
    Y$method2 <- rep(methods, times = n_methods)
    Y <- Y[Y$method1 != Y$method2, ]
    Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0  # kein Unterschied ob > oder >=
    return(Y)
  }
  
  df_list <- lapply(result_list, function(rl) {
    df <- .FormatResults(rl)
    if (is.null(df)) return(NULL)
    df$diff <- NA
    if ("mabt" %in% df$ci.method) df$diff <- df$ci.lwr[df$ci.method == "mabt"] - df$ci.lwr
    return(df)
  })
  table_list <- lapply(df_list, .GenCompTable)
  table_df <- do.call(rbind, table_list)
  table_df$comparison <- paste0(table_df$method1, " > ", table_df$method2)
  
  return(list(table_sum  = aggregate(lbtr ~ comparison, table_df, sum), 
              table_mean = aggregate(lbtr ~ comparison, table_df, mean),
              cis = df_list))
}

# comp_tab_200 <- GenCompTable(rf_n200_list)
comp_tab_400 <- GenCompTable(rf_n400_list)

# sink("8-sim-results/results-out/rf-n200-pairwise.txt")
# comp_tab_200$table_sum[substr(comp_tab_200$table_sum$comparison, 1, 4) == "mabt", ]
# comp_tab_200$table_mean[substr(comp_tab_200$table_mean$comparison, 1, 4) == "mabt", ]
# sink()

sink("8-sim-results/results-out/rf-n400-pairwise.txt")
comp_tab_400$table_sum[substr(comp_tab_400$table_sum$comparison, 1, 4) == "mabt", ]
comp_tab_400$table_mean[substr(comp_tab_400$table_mean$comparison, 1, 4) == "mabt", ]
sink()
