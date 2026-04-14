#==============================
# 1. Load packages and data
#==============================
library(tidyverse)
font_add("Arial", regular = "C:/Windows/Fonts/arial.ttf")
showtext_auto()
# Read CSV exported from Stata
qr_data <- read_csv(file.path("..", "3_output", "RDataUse", "qr_results.csv"))

#==============================
# 2. Data preparation
#==============================
qr_data <- qr_data %>%
  filter(!is.na(b)) %>%  # Drop missing coefficients
  mutate(
    x = case_when(
      q == 10 ~ 1,
      q == 25 ~ 2,
      q == 50 ~ 3,
      q == 75 ~ 4,
      q == 90 ~ 5
    ),
    year_label = ifelse(year == 9999, "Total", as.character(year))
  )

# Extract pooled OLS data
ols_line <- qr_data %>% filter(year == 9999)

#==============================
# 3. Plot
#==============================
library(ggplot2)

ggplot(qr_data %>% filter(year != 9999), aes(x = x, y = b)) +
  # Confidence band
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "skyblue", alpha = 0.2) +
  # Coefficient line and points
  geom_line(color = "steelblue3", size = 1) +
  geom_point(color = "steelblue3", size = 1.2) +
  # Pooled OLS line
  geom_line(data = ols_line, aes(x = x, y = b), color = "firebrick3", size = 1, linetype = "dashed") +
  geom_ribbon(data = ols_line, aes(x = x, ymin = lo, ymax = hi), fill = "firebrick3", alpha = 0.1) +
  # Facet by year
  facet_wrap(~year_label, ncol = 4) +
  # X-axis labels
  scale_x_continuous(
    breaks = 1:5,
    labels = c("10", "25", "50", "75", "90"),
    limits = c(0.5, 5.5)
  ) +
  # Dashed zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # Labels
  labs(
    x = "Quantile",
    y = "Coefficient",
    title = "Quantile Regression Effects by Year",
    caption = "Shaded area = 95% CI; Dark dashed = pooled OLS"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 20, face = "bold"),
    plot.caption = element_text(size = 16)
  )

#==============================
# 4. Save figure
#==============================
ggsave(file.path("0_unused_quantile_regression", "qr_faceted_blue.png"),  dpi = 300)