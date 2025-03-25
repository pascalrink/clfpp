
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

# custom ggsave function to save in 1:sqrt(2) aspect ratio
.myggsave <- function(filename, plot) {
  ggsave(filename=filename, plot = plot, height = 147, width = 208, units = "mm")
}

x <- seq(1, 2, length.out = 1001)
y1 <- (-1) * ((1.35*x-1.85)^2 - 0.4*x^1.4 + 1.1) + 1
y2 <- (-1) * (1/(1.2*x^4)) + 1.05

df <- data.frame(x = c(x, x), y = c(y1, y2), z = c(rep(2, 1001), rep(1, 1001)))

overfit_plot <- ggplot(df, aes(x = x, y = y, linetype = factor(z))) + 
  coord_cartesian(ylim = c(0, 1)) +
  geom_line(size = 2) + 
  xlab("Model complexity") + 
  ylab("Prediction performance") + 
  scale_y_continuous(labels = c("60%", "70%", "80%", "90%", "100%")) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), 
        legend.position = "none", 
        panel.grid.minor = element_blank())


.myggsave("/Users/pascal/Nextcloud/Manuskripte/Dissertation/images/fig--overfitting-plot.png", 
          overfit_plot)