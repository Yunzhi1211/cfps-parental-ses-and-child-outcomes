library(tidyverse)

# -------------------------
# 1. Data from the table
# -------------------------
df <- tribble(
  ~term,              ~outcome, ~estimate, ~se,
  "Parent SES (PCA)", "ISEI",    2.782,     0.113,
  "Male (=1)",        "ISEI",   -5.283,     0.335,
  "Child age",        "ISEI",   -0.025,     0.024,
  "Urban",            "ISEI",    3.870,     0.330,
  
  "Parent SES (PCA)", "SIOPS",   1.506,     0.145,
  "Male (=1)",        "SIOPS",  -3.075,     0.377,
  "Child age",        "SIOPS",   0.074,     0.031,
  "Urban",            "SIOPS",   0.693,     0.457,
  
  "Parent SES (PCA)", "EGP",    -0.588,     0.034,
  "Male (=1)",        "EGP",     1.634,     0.090,
  "Child age",        "EGP",     0.011,     0.008,
  "Urban",            "EGP",    -0.972,     0.113
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    term = factor(term, levels = c("Parent SES (PCA)", "Urban", "Male (=1)", "Child age")),
    outcome = factor(outcome, levels = c("ISEI", "SIOPS", "EGP")),
    label = sprintf("%.2f", estimate)
  )

# -------------------------
# 2. Plot (poster-finished style)
# -------------------------
# Visual order for y axis: strongest substantive term at top
plot_df <- df %>%
  mutate(term = factor(term, levels = rev(levels(term))))

# Dynamic label placement: outside bar on the estimate direction
plot_df <- plot_df %>%
  mutate(
    label_x = NA_real_,
    label_hjust = 1,
    band_rank = factor(
      as.integer(factor(term, levels = rev(levels(term)))),
      levels = 1:4
    )
  )

x_lim <- range(c(plot_df$lower, plot_df$upper), na.rm = TRUE)
x_pad <- diff(x_lim) * 0.10
x_limits <- c(x_lim[1] - x_pad, x_lim[2] + x_pad)
label_column_x <- x_limits[2] - diff(x_limits) * 0.03

plot_df <- plot_df %>%
  mutate(label_x = label_column_x)

# Stable background bands for each term row (avoid geom_tile clipping warnings)
bg_df <- plot_df %>%
  distinct(term, band_rank) %>%
  mutate(
    y_id = as.numeric(term),
    ymin = y_id - 0.41,
    ymax = y_id + 0.41,
    xmin = x_limits[1],
    xmax = x_limits[2]
  )

term_palette <- c(
  "Parent SES (PCA)" = "#1F4E8C",
  "Urban" = "#4B7EB8",
  "Male (=1)" = "#78A7D5",
  "Child age" = "#A8C7E6"
)

p <- ggplot(plot_df, aes(x = estimate, y = term)) +
  # Background bands (top darker, bottom lighter)
  geom_rect(
    data = bg_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = band_rank),
    inherit.aes = FALSE,
    color = "white",
    linewidth = 0.8,
    alpha = 0.58
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "22",
    color = "#8E9AA8",
    linewidth = 1.0
  ) +
  geom_segment(
    aes(x = 0, xend = estimate, yend = term, color = term),
    linewidth = 3.8,
    alpha = 1,
    lineend = "round"
  ) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0,
    linewidth = 1.6,
    color = "#2F3B4D",
    orientation = "y"
  ) +
  geom_point(
    aes(color = term),
    shape = 16,
    size = 8.2,
    alpha = 1
  ) +
  geom_label(
    aes(x = label_x, label = label, hjust = label_hjust),
    fill = "white",
    color = "#203247",
    fontface = "bold",
    size = 8.6,
    label.size = 0,
    label.padding = unit(0.14, "lines")
  ) +
  facet_wrap(~ outcome, nrow = 1, scales = "fixed") +
  scale_x_continuous(
    limits = x_limits,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#BFD6EE",
      "2" = "#D2E2F3",
      "3" = "#E2ECF8",
      "4" = "#EEF4FC"
    ),
    guide = "none"
  ) +
  scale_color_manual(values = term_palette, guide = "none") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Robustness: Alternative Outcome Variables",
    subtitle = "Effect estimates with 95% confidence intervals across ISEI, SIOPS, and EGP",
    x = "Coefficient estimate",
    y = NULL,
    caption = "Standard errors clustered at the province level"
  ) +
  theme_minimal(base_size = 30) +
  theme(
    plot.title = element_text(face = "bold", size = 48, hjust = 0.5, color = "#0F2E57"),
    plot.subtitle = element_text(size = 28, hjust = 0.5, color = "#445A73", margin = margin(b = 12)),
    axis.text.y = element_text(size = 42, face = "bold", color = "#1D2F45", margin = margin(r = 16)),
    axis.text.x = element_text(size = 22, color = "#334155"),
    axis.title.x = element_text(size = 24, face = "bold", margin = margin(t = 12)),
    strip.text = element_text(size = 32, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#0F3D73", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#D7E0EB", linewidth = 0.85),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(size = 20, color = "#55657A"),
    plot.margin = margin(20, 25, 16, 20)
  )

# -------------------------
# 3. Export (poster-panel locked size)
# -------------------------
# Lock physical size so saved PNG matches poster usage.
save_w_cm <- 30
save_h_cm <- 18
save_dpi  <- 320

if (requireNamespace("ragg", quietly = TRUE)) {
  ggsave(
    "robustness_alternative_outcomes.png",
    p,
    width = save_w_cm,
    height = save_h_cm,
    units = "cm",
    dpi = save_dpi,
    limitsize = FALSE,
    bg = "white",
    device = ragg::agg_png
  )
} else {
  ggsave(
    "robustness_alternative_outcomes.png",
    p,
    width = save_w_cm,
    height = save_h_cm,
    units = "cm",
    dpi = save_dpi,
    limitsize = FALSE,
    bg = "white"
  )
}
print(p)
