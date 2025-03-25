
# Output files: ../../images/fig--sim-result-max-t-vs-sidak.png

rm(list = ls())

library(dplyr)
library(ggplot2)

set.seed(1)

# n <- 100
alpha <- 0.05

m_range <- c(2, 5, 10, 50, 500)
rho_range <- c(0, 0.8, 0.99)

fun_args <- expand.grid(m_range, rho_range)
names(fun_args) <- c("m", "rho")

MyFun <- function(m, rho) {
  mu_true <- rep(0, m)
  sigma_true <- matrix(rho, nrow = m, ncol = m)
  diag(sigma_true) <- 1
  
  max_quantile <- mvtnorm::qmvnorm(1-alpha, tail = "lower.tail", mean = 0, sigma = sigma_true)$quantile
  return(pnorm(max_quantile))
}

myfun_out <- rep(NA, nrow(fun_args))
for (i in 1:nrow(fun_args)) {
  myfun_out[i] <- MyFun(fun_args$m[i], fun_args$rho[i])
  print(i)
}

maxt_df <- data.frame(fun_args, type = "maxt", alpha_adj = 1-myfun_out)
sidak_df <- data.frame(m = m_range, rho = -999, type = "sidak", alpha_adj = 1-(1-alpha)^(1/m_range))
out_df <- rbind(maxt_df, sidak_df)


ggplot(out_df, aes(x = factor(m), y = alpha_adj, shape = factor(rho))) + 
  geom_point(aes(shape = type, size = 2)) + 
  geom_path(aes(group = rho)) + 
  coord_cartesian(ylim = c(0, 0.05))


ggplot(out_df, aes(x = factor(m), y = alpha_adj)) + 
  geom_point(aes(shape = type), size = 5) + 
  geom_path(aes(group = rho)) +
  scale_shape_manual(values = c(18, 0)) +
  coord_cartesian(xlim = c(1, 5), ylim = c(0, 0.05)) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        legend.position = "none") + 
  scale_y_continuous(breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05), labels = c("0", "1%", "2%", "3%", "4%", "5%")) + 
  xlab("Number of hypotheses m") + 
  ylab("Adjusted significance level") + 
  geom_text(data = data.frame(x = 1.25, y = 0.015, label = "\u03C1 = 0"),
            aes(x = x, y = y, label = label), 
            color = "black", size = 5) + 
  geom_text(data = data.frame(x = 2.55, y = 0.021, label = "\u03C1 = 0.8"), 
            aes(x = x, y = y, label = label), 
            color = "black", size = 5) + 
  geom_text(data = data.frame(x = 3.75, y = 0.038, label = "\u03C1 = 0.99"), 
            aes(x = x, y = y, label = label), 
            color = "black", size = 5) + 
  theme(text = element_text(color = "black"))

ggsave("../../images/fig--sim-result-max-t-vs-sidak.png", width = 13, height = 8, units = "cm")
