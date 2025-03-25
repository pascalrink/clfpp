
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

set.seed(1)

# custom ggsave function to save in 1:sqrt(2) aspect ratio
.myggsave <- function(filename, plot) {
  ggsave(filename=filename, plot = plot, height = 147, width = 208, units = "mm")
}

x1 <- runif(20)
x2 <- runif(20)

y <- x1^2 + 2 * x2^2
y <- (y > mean(y)) * 1.0

df <- data.frame(x1, x2, y)

decision_boundary_plot <- ggplot(df, aes(x = x1, y = x2, shape = factor(y))) +
  geom_point(size = 5, fill = "black") +
  geom_abline(slope = -0.6, intercept = 0.89, color = "black", size = 1) +
  stat_function(fun = function(x) log(-0.5 * x^4 + 1) + 0.66,
                color = "black", linetype = "dashed", size = 1) +
  theme_minimal() + 
  xlab(expression("x"[1])) + ylab(expression("x"[2])) +
  theme(
    axis.text = element_blank(), 
    legend.position = "none", 
    axis.title.x = element_text(size = 22), 
    axis.title.y = element_text(size = 22))


.myggsave("/Users/pascal/Nextcloud/Manuskripte/Dissertation/images/fig--decision-boundary-plot.png", 
          decision_boundary_plot)