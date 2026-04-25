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

# =========================
# Quantile mediation figure
# =========================
q_all <- read_csv(file.path("3_mechanism_sun", "7_1b_quantile_class_mediation_all.csv"), show_col_types = FALSE)

# Build a clean quantile dataset
q_clean <- q_all %>%
  filter(!is.na(q), !is.na(mlabel), !is.na(abs_indirect), !is.na(N), N >= 50, q %in% c(25, 50, 75)) %>%
  mutate(
    class_label = case_when(
      q == 25 ~ "Q25",
      q == 50 ~ "Q50",
      q == 75 ~ "Q75",
      TRUE ~ as.character(q)
    ),
    class_label = factor(class_label, levels = c("Q25", "Q50", "Q75"))
  )

# Identify top mediators with the largest cross-quantile differences
top_n <- 5
top_diff <- q_clean %>%
  group_by(mediator, mlabel) %>%
  summarise(
    max_abs = max(abs_indirect, na.rm = TRUE),
    min_abs = min(abs_indirect, na.rm = TRUE),
    diff_abs = max_abs - min_abs,
    .groups = "drop"
  ) %>%
  arrange(desc(diff_abs), desc(max_abs)) %>%
  slice_head(n = top_n)

plot_topn <- q_clean %>%
  semi_join(top_diff, by = c("mediator", "mlabel")) %>%
  left_join(top_diff %>% select(mediator, diff_abs), by = "mediator") %>%
  left_join(
    top_diff %>%
      arrange(desc(diff_abs), desc(max_abs)) %>%
      mutate(color_rank = row_number()),
    by = c("mediator", "mlabel", "diff_abs")
  ) %>%
  group_by(mediator, mlabel, color_rank) %>%
  mutate(
    label_q75 = ifelse(class_label == "Q75", mlabel, NA_character_)
  ) %>%
  ungroup()

# Build upper-left label block
q75_labels <- plot_topn %>%
  filter(class_label == "Q75") %>%
  arrange(desc(abs_indirect)) %>%
  mutate(
    label_rank = row_number(),
    label_y = seq(from = 2.25, to = 1.30, length.out = n()),
    label_txt = paste0(label_rank, ". ", gsub("^Child\\s+", "", mlabel))
  )

plot_main <- plot_topn
q75_labels_main <- q75_labels

# Blue-first palette with one red accent for the most different line
line_colors <- c(
  "#C62828", "#0B3C6F", "#15558A", "#1F6EA3", "#2E7EB0",
  "#4A95C2", "#63A6D3", "#7CB6DC", "#9AC9E9", "#B9DAF1"
)
line_colors <- line_colors[1:length(unique(plot_topn$color_rank))]
names(line_colors) <- as.character(sort(unique(plot_topn$color_rank)))

p_q <- ggplot(plot_main, aes(x = as.numeric(class_label), y = abs_indirect, color = as.factor(color_rank), group = mlabel)) +
  geom_line(linewidth = 2.1, alpha = 0.97, lineend = "round") +
  geom_point(size = 5.6, alpha = 1, stroke = 1.0, shape = 21, fill = "white") +
  geom_label(
    data = q75_labels_main,
    aes(x = 0.88, y = label_y, label = label_txt, color = as.factor(color_rank)),
    fill = "white",
    label.size = 0.30,
    hjust = 0,
    label.r = unit(0.12, "lines"),
    size = 15.5 / .pt,
    fontface = "bold"
  ) +
  scale_color_manual(values = line_colors) +
  labs(
    title = paste0("Top ", top_n, " mediators by quantile difference"),
    subtitle = "Difference metric: max(|a*b|) - min(|a*b|) across Q25, Q50, Q75",
    x = NULL,
    y = "|Indirect effect|"
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Q25", "Q50", "Q75"),
    limits = c(0.78, 3.10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 32) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 22, color = "#123A63", margin = margin(t = 10)),
    axis.text.y = element_text(face = "bold", size = 20, color = "#1E3A5F"),
    axis.title.y = element_text(face = "bold", size = 20, color = "#1E3A5F"),
    plot.title = element_text(face = "bold", size = 40, hjust = 0.5, color = "#143A5A"),
    plot.subtitle = element_text(size = 26, hjust = 0.5, margin = margin(b = 20), color = "#2F5D7C"),
    panel.grid.major.x = element_line(color = "#E5EFF8", linewidth = 0.8),
    panel.grid.major.y = element_line(color = "#D8E4F0", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(26, 70, 30, 120)
  )

print(p_q)

ggsave(
  file.path("3_mechanism_sun", "quantile_top_diff_mediators_fancy.png"),
  plot = p_q,
  width = 24,
  height = 18,
  units = "cm",
  dpi = 400,
  bg = "white"
)
