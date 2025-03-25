# Load necessary library
library(ggplot2)

# Set the seed for reproducibility
set.seed(123)

# Generate data from a normal distribution
data <- rnorm(10000000, mean = 0, sd = 1)

# Create a data frame
df <- data.frame(value = data)

# Create a density plot using ggplot
ggplot(data = df, aes(x = value)) +
  geom_density(fill = "skyblue", color = "blue", alpha = 0.5) +
  coord_cartesian(xlim = c(-4, 4)) + 
  geom_vline(xintercept = 1.75, color = "red") + 
  theme_minimal() +
  labs(x = "value of test statistic T", 
       y = "probability density function") + 
  geom_text(data = data.frame(x = 2.5, y = 0.13, label = "T = 1.75"), 
            aes(x = x, y = y, label = label), 
            color = "red", size = 4) + 
  theme(text = element_text(color = "black"))


ggsave("../../images/fig--test-stat-normal-dist.png", width = 13, height = 8, units = "cm")
