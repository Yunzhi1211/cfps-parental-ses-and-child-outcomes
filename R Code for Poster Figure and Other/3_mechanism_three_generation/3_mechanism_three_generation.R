#========================
# 1. Load packages
#========================
library(ggplot2)
library(dplyr)
library(scales)
library(grid)   # for unit()

#========================
# 2. Input data
#========================
df <- data.frame(
  x           = c(1, 2, 3, 4),
  grandparent = c("PGF", "PGM", "MGF", "MGM"),
  label_full  = c("Paternal\nGrandfather",
                  "Paternal\nGrandmother",
                  "Maternal\nGrandfather",
                  "Maternal\nGrandmother"),
  estimate    = c(4.346, 5.850, 6.606, 5.584),
  se          = c(0.696, 0.576, 2.399, 1.301),
  stars       = c("***", "***", "***", "***"),
  n           = c(417, 747, 47, 117)
)

#========================
# 3. Compute 95% confidence intervals
#========================
df <- df %>%
  mutate(
    lower     = estimate - 1.96 * se,
    upper     = estimate + 1.96 * se,
    label_num = sprintf("%.2f%s", estimate, stars),
    label_n   = paste0("N = ", comma(n))
  )

ymax_plot <- max(df$upper) + 2.4

#========================
# 4. Create plot
#========================
p <- ggplot(df, aes(x = x, y = estimate)) +
  
  # Background shading: paternal vs maternal line
  annotate("rect",
           xmin = 0.5, xmax = 2.5,
           ymin = 0, ymax = ymax_plot,
           fill = "#E8F2FB", alpha = 0.95) +
  
  annotate("rect",
           xmin = 2.5, xmax = 4.5,
           ymin = 0, ymax = ymax_plot,
           fill = "#F3F8FC", alpha = 0.98) +
  
  # Vertical separator between paternal and maternal sides
  annotate("segment",
           x = 2.5, xend = 2.5,
           y = 0, yend = ymax_plot,
           color = "#D6E6F5",
           linewidth = 1.2) +
  
  # Group labels
  annotate("label",
           x = 1.5, y = ymax_plot - 0.45,
           label = "Paternal Line",
           fill = "#D7EAF9", color = "#123B73",
           fontface = "bold",
           size = 5.3,
           label.size = 0.5,
           label.r = unit(0.15, "lines")) +
  
  annotate("label",
           x = 3.5, y = ymax_plot - 0.45,
           label = "Maternal Line",
           fill = "#EAF3FA", color = "#123B73",
           fontface = "bold",
           size = 5.3,
           label.size = 0.5,
           label.r = unit(0.15, "lines")) +
  
  # Decorative glow circles behind points
  geom_point(
    data = df,
    aes(x = x, y = estimate),
    inherit.aes = FALSE,
    size = 18,
    shape = 21,
    fill = "#A9D2F3",
    color = NA,
    alpha = 0.16
  ) +
  geom_point(
    data = df,
    aes(x = x, y = estimate),
    inherit.aes = FALSE,
    size = 12,
    shape = 21,
    fill = "#7DB7E8",
    color = NA,
    alpha = 0.16
  ) +
  
  # Zero reference line
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 1.4,
    color = "#7D7D7D"
  ) +
  
  # Confidence interval line
  geom_linerange(
    aes(ymin = lower, ymax = upper),
    linewidth = 4.0,
    color = "#6FAFE3",
    alpha = 0.98
  ) +
  
  # Error bar caps
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.18,
    linewidth = 1.7,
    color = "#245C90"
  ) +
  
  # Main points
  geom_point(
    size = 9.5,
    shape = 21,
    fill = "#0D3B66",
    color = "white",
    stroke = 2.3
  ) +
  
  # Small highlight in the center of each point
  geom_point(
    size = 3.2,
    shape = 21,
    fill = "#DCECF9",
    color = "white",
    stroke = 0.8
  ) +
  
  # Coefficient labels
  geom_label(
    aes(y = upper + 0.78, label = label_num),
    fill = "#EAF3FB",
    color = "#0D3B66",
    size = 6.2,
    fontface = "bold",
    label.size = 0,
    label.padding = unit(0.24, "lines")
  ) +
  
  # Sample size labels
  geom_text(
    aes(y = 0.48, label = label_n),
    color = "#5B6C7A",
    size = 5.0,
    fontface = "bold"
  ) +
  
  scale_x_continuous(
    limits = c(0.5, 4.5),
    breaks = df$x,
    labels = df$label_full,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  labs(
    title = "Grandparent-specific Effects on Child ISEI",
    subtitle = "Coefficient estimates with 95% confidence intervals",
    x = NULL,
    y = "Coefficient estimate",
    caption = paste0(
      "PGF = paternal grandfather; PGM = paternal grandmother; ",
      "MGF = maternal grandfather; MGM = maternal grandmother\n",
      "Controls include parent SES, sex, age, urban residence, and year fixed effects."
    )
  ) +
  
  coord_cartesian(ylim = c(0, ymax_plot), clip = "off") +
  
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(
      size = 34,
      face = "bold",
      color = "#123B73",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 20,
      face = "bold",
      color = "#4F6D8A",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.title.y = element_text(
      size = 22,
      face = "bold",
      color = "#1F2D3D",
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(
      size = 18,
      face = "bold",
      color = "#102A43",
      lineheight = 0.95,
      margin = margin(t = 12)
    ),
    axis.text.y = element_text(
      size = 17,
      face = "bold",
      color = "#334E68"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "#D9EAF7",
      linewidth = 1.2
    ),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(28, 30, 24, 30),
    plot.caption = element_text(
      size = 13,
      color = "#5B6C7A",
      hjust = 0
    )
  )

print(p)

#========================
# 5. Export figure
#========================
ggsave(
  "grandparent_effects_poster_paternal_maternal_bg.png",
  plot = p,
  width = 13,
  height = 8.5,
  dpi = 400,
  bg = "white"
)
