
rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(ggpattern)

# source("4-plots/myggsave-function.R")

# n_threshold <- 1000
# 
# GetNPerLambda <- function(df) {
#   n_df <- aggregate(covers ~ lambda + method, df, NROW)
#   n_df <- rename(n_df, n = covers)
#   df <- merge(df, n_df) %>% subset(n >= n_threshold)
#   return(df)
# }
# 
# GetFirst1000 <- function(df) {
#   by_lambda_list <- dplyr::group_split(df, lambda)
#   by_lambda_list <- lapply(by_lambda_list, function(bll) bll[1:n_threshold, ])
#   by_lambda_df <- do.call(rbind, by_lambda_list)
#   by_lambda_df$n <- NULL
#   return(by_lambda_df)
# }
# 
# MyFunction <- function(d, m) {
#   d <- subset(d, method == m)
#   d$lambda %>% unique %>% length %>% paste0(m, ": # of models is ", .) %>% print
#   d <- GetNPerLambda(d) %>% GetFirst1000
#   d$lambda %>% unique %>% length %>% 
#     paste0(m, ": for ", ., " models there are at least ", 
#            n_threshold, " replications") %>% 
#     print
#   return(d)
# }

df <- readRDS("6-additions/conditional-coverage/conditional-regular.RDS")
per_lambda_df <- aggregate(covers ~ method + lambda, df, mean) %>% 
  rename(coverage = covers)

# analyze the results for models that have at least 1000 replications ----

aggregate(covers ~ method, df, mean)
aggregate(coverage ~ method, per_lambda_df, summary)

# ggplot ----

alpha_level <- 0.05
cvg_lvl <- 1-alpha_level
delta_cvg <- sqrt(alpha_level * (1-alpha_level) / 1000)

bxplt <- ggplot(per_lambda_df, aes(x = factor(method), y = coverage)) +
  geom_rect(aes(ymin = cvg_lvl - delta_cvg, ymax = cvg_lvl + delta_cvg, xmin = -Inf, xmax = Inf), 
            fill = "lightgrey", alpha = 0.2) + 
  coord_cartesian(ylim = c(0.82, 0.97)) + 
  # geom_boxplot(fill = "none", alpha = 0.2, color = "black", 
  #              outlier.colour = "black") + 
  geom_boxplot(fill = "white", alpha = 0.5) +
  theme_minimal() + 
  xlab(element_blank()) + 
  ylab("Coverage probability") + 
  scale_y_continuous(breaks = c(0.85, 0.9, 0.95), labels = c("85%", "90%", "95%"), 
                     minor_breaks = seq(0.82, 0.97, 0.01)) +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), 
        legend.position = "none") + 
  geom_boxplot(
    outlier.colour = "black",  # Color of outliers (black)
    outlier.shape = 16,        # Shape of outliers (solid circle)
    outlier.size = 2,          # Size of outliers
    fill = NA,                 # Remove the fill of the box
    color = NA                 # Remove the color of the box outline
  )
# .myggsave("6-additions/conditional-coverage/ccs-results-boxplot.eps", bxplt)


mabt_bxplt <- subset(per_lambda_df, method == "MABT") %>%
  ggplot(aes(x = lambda * 1000, y = coverage)) + 
  geom_rect(aes(ymin = cvg_lvl - delta_cvg, ymax = cvg_lvl + delta_cvg, xmin = -Inf, xmax = Inf), 
            fill = "lightgrey", alpha = 0.2) + 
  coord_cartesian(ylim = c(0.94, 0.972)) + 
  geom_point(size = 4) + 
  xlab(expression(lambda %.% 10^3)) +  
  ylab("Coverage probability") + 
  theme_minimal() + 
  scale_y_continuous(breaks = c(0.94, 0.95, 0.96, 0.97),
                     labels = c("94%", "95%", "96%", "97%")) +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title = element_text(size = 16)) 
        # panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "solid"),
        # panel.grid.minor = element_line(color = "lightgray", size = 0.25, linetype = "dashed"))
# .myggsave("6-additions/conditional-coverage/ccs-lambda-v-coverage-mabt.eps", mabt_bxplt)


# subset(per_lambda_df, method == "BBC-CV") %>%
#   ggplot(aes(x = lambda * 1000, y = coverage)) + 
#   geom_point(size = 4) + 
#   xlab(expression(lambda %.% 10^3)) +  
#   ylab("Coverage probability") + 
#   theme_minimal() + 
#   scale_y_continuous(breaks = c(0.87, 0.89, 0.91, 0.93, 0.95),
#                      labels = c("87%", "89%", "91%", "93%", "95%")) +
#   theme(axis.text.x = element_text(size = 16), 
#         axis.text.y = element_text(size = 16), 
#         axis.title = element_text(size = 16))
# 
# 
# subset(per_lambda_df, method == "NCV") %>%
#   ggplot(aes(x = lambda * 1000, y = coverage)) + 
#   geom_point(size = 4) + 
#   xlab(expression(lambda %.% 10^3)) +  
#   ylab("Coverage probability") + 
#   theme_minimal() + 
#   scale_y_continuous(breaks = c(0.91, 0.93, 0.95, 0.97),
#                      labels = c("91%", "93%", "95%", "97%")) +
#   theme(axis.text.x = element_text(size = 16), 
#         axis.text.y = element_text(size = 16), 
#         axis.title = element_text(size = 16))



bbccv_ncv_bxplt <- subset(per_lambda_df, method != "MABT") %>%
  ggplot(aes(x = lambda * 1000, y = coverage, shape = method)) + 
  geom_rect(aes(ymin = cvg_lvl - delta_cvg, ymax = cvg_lvl + delta_cvg, xmin = -Inf, xmax = Inf), 
            fill = "lightgrey", alpha = 0.2) + 
  geom_point(size = 4) + 
  xlab(expression(lambda %.% 10^3)) +  
  ylab("Coverage probability") + 
  theme_minimal() + 
  scale_y_continuous(breaks = c(0.85, 0.87, 0.89, 0.91, 0.93, 0.95, 0.97),
                     labels = c("85%", "87%", "89%", "91%", "93%", "95%", "97%")) +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title = element_text(size = 16), 
        legend.position = "none")
# .myggsave("6-additions/conditional-coverage/ccs-lambda-v-coverage-bbccv-ncv.eps", bbccv_ncv_bxplt)

saveRDS(bxplt, "6-additions/conditional-coverage/plots/conditional-regular-coverage.RDS")
saveRDS(mabt_bxplt, "6-additions/conditional-coverage/plots/conditional-regular-scatter-mabt.RDS")
saveRDS(bbccv_ncv_bxplt, "6-additions/conditional-coverage/plots/conditional-regular-scatter-bbcncv.RDS")



