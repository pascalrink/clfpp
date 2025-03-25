rm(list = ls())
graphics.off()

# Load necessary library
library(dplyr)
library(ggplot2)

# Set the seed for reproducibility
set.seed(123)

# Generate data from a normal distribution
data <- rnorm(10000000, mean = 0, sd = 1)

# Create a data frame
df <- density(data)
df <- data.frame(x = df$x, y = df$y)
df_filtered <- df[df$x >= 1.75, ] %>% data.frame

# Create a density plot using ggplot
ggplot(df, aes(x = x, y = y)) + 
  geom_line(size = 1) + 
  geom_vline(xintercept = 1.75, color = "black", linetype = 2, size = 1) + 
  labs(x = "Value of test statistic T", 
       y = "Density") + 
  geom_text(data = data.frame(x = 2.7, y = 0.17, label = "T = 1.75"), 
            aes(x = x, y = y, label = label), 
            color = "black", size = 6) + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16)) + 
  geom_ribbon(data = df_filtered, aes(ymax = y), ymin = 0, fill = "black", alpha = 0.33) + 
  geom_text(data = data.frame(x = 2.1, y = 0.0225, label = "p"), 
            aes(x = x, y = y, label = label), 
            color = "black", size = 6) + 
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), minor_breaks = FALSE) + 
  scale_y_continuous(minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(0, 0.4))

ggsave("../../images/fig--test-stat-normal-dist.png", width = 13, height = 8, units = "cm")
