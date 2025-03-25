
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

set.seed(13)

x <- rnorm(1000, 1.005, 55)
mu <- mean(x)

MyMean <- function(d, i) mean(d[i])
boot.array <- boot::boot(x, MyMean, R = 5000) %>% boot::boot.array()

# ----

t.original <- (x * 1/length(x) * t(boot.array)) %>% colSums
# summary(t.original)
# summary(boot$t)

d <- density(t.original)
df <- data.frame(x = d$x, y = d$y / sum(d$y))

# ----

mu_0 <- -1.4  # 0, -1

h0 <- mu + mu_0
w <- boot::exp.tilt(L = x, theta = h0, t0 = mu)$p  

# ----

t.tilted <- (x * w * t(boot.array)) %>% colSums
d.tilted <- density(t.tilted)
df.tilted <- data.frame(x = d.tilted$x, y = d.tilted$y / sum(d.tilted$y))
df.tilted.filtered <- df.tilted[df.tilted$x >= mu, ]

ggplot(df, aes(y = y)) + 
  geom_line(aes(x = x), color = "black", size = 1) + 
  geom_line(data = df.tilted, aes(x = x, y = y), col = "black", size = 1, linetype = 2) + 
  geom_segment(x = mu, xend = mu, y = 0, yend = max(df$y), col = "black", size = 0.75, linetype = 3) + 
  geom_segment(x = mu_0, xend = mu_0, y = 0, yend = max(df.tilted$y), col = "black", size = 0.75, linetype = 3) + 
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid = element_blank()) + 
  geom_ribbon(data = df.tilted.filtered, aes(x = x, ymax = y), ymin = 0, fill = "grey", alpha = 0.4) + 
  annotate("text", x = 1.5, y = 0.0006, label = "p", size = 7) + 
  annotate("text", x = 0.96, y = -0.00035, label = expression(hat(theta)[n]), size = 7) + 
  annotate("text", x = -1.4, y = -0.00035, label = expression(xi), size = 7) + 
  annotate("text", x = -4.9, y = 0.0018, label = expression(hat(F)[tau]), size = 7) + 
  annotate("text", x = -2.6, y = 0.00174, label = expression(F[n]), size = 7) + 
  geom_segment(aes(x = 0.75, y = 0.0015, xend = -1.3, yend = 0.0015), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
               size = 0.8, color = "black", linetype = 1)
  

# custom ggsave function to save in 1:sqrt(2) aspect ratio
.myggsave <- function(filename, plot) {
  ggsave(filename=filename, plot = plot, height = 147, width = 208, units = "mm")
}

.myggsave("/Users/pascal/Nextcloud/Manuskripte/Dissertation/images/fig--tilt-plot.png", 
          last_plot())
