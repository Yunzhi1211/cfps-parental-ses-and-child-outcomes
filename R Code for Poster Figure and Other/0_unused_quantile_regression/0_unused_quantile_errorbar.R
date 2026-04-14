library(tidyverse)
library(ggplot2)
library(showtext)

#==============================
# 1. Load Arial font
#==============================
font_add("Arial", regular = "C:/Windows/Fonts/arial.ttf")
showtext_auto()

#==============================
# 2. Data preparation
#==============================
qr_data <- read_csv(file.path("..", "3_output", "RDataUse", "qr_results.csv")) %>%
  filter(!is.na(b)) %>%
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

total_line <- qr_data %>% filter(year == 9999)

#==============================
# 3. Plot (single blue style + error bars)
#==============================
ggplot(qr_data %>% filter(year != 9999), aes(x = x, y = b)) +
  geom_line(size = 1, color = "steelblue3") +
  geom_point(size = 1.2, color = "steelblue3") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, alpha = 0.6, color = "deepskyblue3") +
  # Total / pooled OLS dashed line with error bars
  geom_line(data = total_line, aes(x = x, y = b), color = "firebrick3", size = 1.2, linetype = "dashed") +
  geom_errorbar(data = total_line, aes(x = x, ymin = lo, ymax = hi), width = 0.2, alpha = 0.4, color = "firebrick3") +
  facet_wrap(~year_label, ncol = 4) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("10", "25", "50", "75", "90"),
    limits = c(0.5, 5.5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "Quantile",
    y = "Coefficient",
    title = "Quantile Regression Effects by Year",
    caption = "Error bars = 95% CI; Red dashed = Total sample"
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
ggsave(file.path("0_unused_quantile_regression", "qr_faceted_total_steelblue.png"), dpi = 300)