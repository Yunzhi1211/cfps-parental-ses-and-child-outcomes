library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# Read data
df <- read.csv(
  file.path("1_research_question_funnel_chart", "sampleflow_total.csv"),
  stringsAsFactors = FALSE
)

# Custom blue palette
blue_colors <- c(
  "#08306B",
  "#0B3C7A",
  "#175290",
  "#2C6AA6",
  "#4F89C6",
  "#7FB3DD"
)

# Prepare data: remove "Yearly file saved"
plot_df <- df %>%
  filter(step != "Yearly file saved") %>%
  arrange(desc(n_rows)) %>%
  mutate(
    step = factor(step, levels = rev(step)),
    step_wrap = str_wrap(step, width = 22),   # Automatically wrap long labels
    pct_of_first = n_rows / max(n_rows),
    xmin = -n_rows / 2,
    xmax =  n_rows / 2,
    ymin = seq_along(n_rows) - 0.5,
    ymax = seq_along(n_rows) + 0.5,
    label = paste0(comma(n_rows), " (", percent(pct_of_first, accuracy = 0.1), ")"),
    fill_rank = factor(row_number())
  )

# Manually set the x-axis range to prevent right-side labels from being clipped
x_left  <- -max(plot_df$n_rows) / 2 * 1.1
x_right <-  max(plot_df$n_rows) / 2 * 1.35

p <- ggplot(plot_df) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_rank),
    color = "white",
    linewidth = 0.8
  ) +
  geom_text(
    aes(x = 0, y = seq_along(n_rows), label = step_wrap),
    color = "white",
    size = 5,
    lineheight = 0.95
  ) +
  geom_text(
    aes(x = xmax + max(n_rows) * 0.10, y = seq_along(n_rows), label = label),
    hjust = 0,
    size = 4.8
  ) +
  scale_fill_manual(values = blue_colors) +
  scale_x_continuous(labels = comma) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(x_left, x_right), clip = "off") +
  labs(
    title = "Sample Selection Funnel",
    x = "Number of observations",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(20, 80, 20, 20)
  )

print(p)

# Save the figure
ggsave(
  filename = file.path("1_research_question_funnel_chart", "sample_funnel_total.png"),
  plot = p,
  width = 12,
  height = 7,
  dpi = 300
)
