
# Output files: ../../images/fig--compare-sidak-bonferroni.png

rm(list = ls())

library(dplyr)
library(ggplot2)

alpha <- 0.05

# m_range <- 1:50
# out_df <- data.frame(m = m_range, bonferroni = alpha / m_range, sidak = 1-(1-alpha)^(1/m_range))
# out_df$delta_rel <- out_df$sidak / out_df$bonferroni
# out_df$delta_abs <- out_df$sidak - out_df$bonferroni
# 
# ggplot(out_df, aes(x = m, y = delta_rel)) +
#   geom_line()

# m_range <- 2:10
m_range <- c(1, 2, 5, 10, 50, 100, 200, 500)
bonferroni_df<- data.frame(m = m_range, type = "bonferroni", alpha_adj = alpha / m_range)
sidak_df <- data.frame(m = m_range, type = "sidak", alpha_adj = 1-(1-alpha)^(1/m_range))
out_df <- rbind(bonferroni_df, sidak_df)

ggplot(out_df, aes(x = factor(m), y = alpha_adj, color = type)) +
  geom_line(aes(group = type)) + 
  scale_color_brewer(palette = "Dark2")


ggsave("../../images/fig--compare-sidak-bonferroni.png")
