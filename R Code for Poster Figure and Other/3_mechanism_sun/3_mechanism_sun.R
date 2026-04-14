library(ggplot2)
library(dplyr)
library(readr)

# Read the data
df <- read_csv(file.path("3_mechanism_sun", "7_1_main_mechanism_results.csv"))

# Keep valid observations only
plot_df <- df %>%
  filter(
    !is.na(group),
    !is.na(mlabel),
    !is.na(pm_share),
    !is.na(N),
    N >= 50
  ) %>%
  mutate(pm_share = as.numeric(pm_share)) %>%
  filter(pm_share > 0)

# Rank groups by total contribution, from largest to smallest
group_order <- plot_df %>%
  group_by(group) %>%
  summarise(
    group_share = sum(pm_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(group_share)) %>%
  mutate(group_rank = row_number())

# Sort mediators within each group by contribution
plot_df <- plot_df %>%
  left_join(group_order, by = "group") %>%
  arrange(group_rank, desc(pm_share)) %>%
  mutate(id = row_number())

# Compute group positions
group_pos <- plot_df %>%
  group_by(group, group_share, group_rank) %>%
  summarise(
    start = min(id),
    end = max(id),
    .groups = "drop"
  ) %>%
  mutate(mid = (start + end) / 2) %>%
  arrange(group_rank)

# Manually define a dark-to-light blue palette
manual_blues <- c(
  "#4E8EC3",
  "#73A6CF",
  "#98BDDD",
  "#BDD6EA",
  "#E1EDF8"
)

# Interpolate colors if needed
if (nrow(group_pos) != length(manual_blues)) {
  manual_blues <- colorRampPalette(c(
    "#4E8EC3", "#73A6CF", "#98BDDD", "#BDD6EA", "#E1EDF8"
  ))(nrow(group_pos))
}

# Assign colors and cleaner group labels
# Assign colors and shorter horizontal group labels
group_pos <- group_pos %>%
  arrange(group_rank) %>%
  mutate(
    fill_col = manual_blues,
    group_short = case_when(
      group == "Human capital" ~ "Human cap.",
      group == "Labor market" ~ "Labor",
      group == "Information/life" ~ "Info",
      group == "Behavior/social" ~ "Behavior",
      group == "Subjective" ~ "Subjective",
      TRUE ~ group
    )
  )


# Merge colors back
plot_df <- plot_df %>%
  left_join(group_pos %>% select(group, fill_col), by = "group")

# Set geometry parameters
inner_radius <- 5.0
outer_start  <- 5.35
outer_scale  <- 3.2
min_outer_height <- 0.35

# Build inner ring
inner_df <- group_pos %>%
  mutate(
    xmin = start - 0.5,
    xmax = end + 0.5,
    ymin = 0,
    ymax = inner_radius
  )



# Build outer ring using square-root scaling to reveal more variation
outer_df <- plot_df %>%
  mutate(
    outer_height_raw = sqrt(pm_share)
  )

# Rescale outer heights to a visually balanced range
h_min <- 0.22
h_max <- 1.55

outer_df <- outer_df %>%
  mutate(
    outer_height = h_min +
      (outer_height_raw - min(outer_height_raw)) /
      (max(outer_height_raw) - min(outer_height_raw)) * (h_max - h_min),
    ymin = outer_start,
    ymax = outer_start + outer_height
  )



# Count the total number of outer segments
n_bar <- nrow(plot_df)

# Compute outer label positions
label_df <- outer_df %>%
  mutate(
    angle_mid = 90 - 360 * (id - 0.5) / n_bar,
    hjust = ifelse(angle_mid < -90 | angle_mid > 90, 1, 0),
    label_y = ymax + 0.35,
    share_pct = pm_share * 100,
    share_lab = case_when(
      share_pct < 0.01 ~ "<0.01%",
      share_pct < 0.1  ~ paste0(sub("\\.?0+$", "", sprintf("%.2f", share_pct)), "%"),
      share_pct < 1    ~ paste0(sub("\\.?0+$", "", sprintf("%.1f", share_pct)), "%"),
      TRUE             ~ paste0(sub("\\.?0+$", "", sprintf("%.1f", share_pct)), "%")
    ),
    outer_lab = paste0(mlabel, "\n", share_lab)
  )




# Compute horizontal label positions for the inner disk
group_label_df <- group_pos %>%
  mutate(
    label_y = case_when(
      group == "Labor market" ~ inner_radius * 0.55,
      group == "Information/life" ~ inner_radius * 0.42,
      group == "Human capital" ~ inner_radius * 0.50,
      group == "Behavior/social" ~ inner_radius * 0.45,
      group == "Subjective" ~ inner_radius * 0.47,
      TRUE ~ inner_radius * 0.45
    )
  )



# Draw the plot
p <- ggplot() +
  geom_rect(
    data = inner_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = inner_df$fill_col,
    color = "white",
    linewidth = 0.7
  ) +
  geom_rect(
    data = outer_df,
    aes(xmin = id - 0.46, xmax = id + 0.46, ymin = ymin, ymax = ymax),
    fill = outer_df$fill_col,
    color = "white",
    linewidth = 0.45
  ) +
  geom_text(
    data = label_df,
    aes(x = id, y = label_y, label = outer_lab, hjust = hjust),
    angle = 0,
    size = 3.0,
    lineheight = 0.9
  ) +
  geom_text(
    data = group_label_df,
    aes(x = mid, y = label_y, label = group_short),
    angle = 0,
    size = 5.0,
    fontface = "bold"
  ) +
  coord_polar(theta = "x", start = 0, clip = "off") +
  xlim(0.5, n_bar + 0.5) +
  ylim(0, max(label_df$label_y) + 0.25) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16,
      lineheight = 1.05,
      margin = margin(b = -3)
    ),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(-8, 60, 20, 60)
  ) +
  labs(
    title = "Mechanism contributions",
    subtitle = NULL
  )


print(p)

# Save the figure to the current working directory
ggsave(
  file.path("3_mechanism_sun", "mechanism_sunburst_manual_blue_final.png"),
  plot = p,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)
