library(ggplot2)
library(dplyr)
library(readr)

data <- read_csv(file.path("..", "3_output", "RDataUse", "Panel_data_with_PCA.csv"))

plot_data <- data %>%
  filter(!is.na(year), !is.na(parent_ses_pca), !is.na(child_isei)) %>%
  mutate(year = as.character(year))

# Add pooled panel labeled as Total
plot_data_total <- plot_data %>%
  mutate(year = "Total")

plot_data2 <- bind_rows(plot_data, plot_data_total)

# Keep Total as the last facet
plot_data2$year <- factor(
  plot_data2$year,
  levels = c(sort(unique(plot_data$year)), "Total")
)

p <- ggplot(plot_data2, aes(x = parent_ses_pca, y = child_isei)) +
  geom_point(color = "#4A90E2", alpha = 0.08, size = 0.9) +
  geom_smooth(method = "lm", color = "#08519C", linewidth = 1) +
  facet_wrap(~ year, nrow = 2) +
  labs(
    title = "Child ISEI vs Parent SES (PCA) by Year",
    x = "Parent SES (PCA score)",
    y = "Child ISEI"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8)
  )

print(p)

ggsave(
  file.path("0_unused_quantile_regression", "pca_faceted_scatter_fit.png"),
  plot = p,
  dpi = 300
)