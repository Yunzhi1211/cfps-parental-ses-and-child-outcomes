# =========================================================
# 0. Packages
# =========================================================
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(scales)
library(patchwork)

# =========================================================
# 1. Read panel data (same style as mapcoef.R)
# =========================================================
df <- read_csv(
  file.path("..", "3_output", "panel", "Panel_data.csv"),
  show_col_types = FALSE
)

# =========================================================
# 2. Helper
# =========================================================
to_num <- function(x) suppressWarnings(as.numeric(x))

# =========================================================
# 3. Build parent SES using PCA (same logic as mapcoef.R)
# =========================================================
df2 <- df %>%
  mutate(
    child_isei = to_num(child_isei),
    year = to_num(year),
    f_edu = to_num(f_edu),
    m_edu = to_num(m_edu),
    f_isei = to_num(f_isei),
    m_isei = to_num(m_isei),
    f_party = to_num(f_party),
    m_party = to_num(m_party),
    parent_edu = rowMeans(cbind(f_edu, m_edu), na.rm = TRUE),
    parent_isei_raw = rowMeans(cbind(f_isei, m_isei), na.rm = TRUE),
    parent_party_raw = rowMeans(cbind(f_party, m_party), na.rm = TRUE)
  ) %>%
  mutate(
    parent_edu = ifelse(is.nan(parent_edu), NA, parent_edu),
    parent_isei_raw = ifelse(is.nan(parent_isei_raw), NA, parent_isei_raw),
    parent_party_raw = ifelse(is.nan(parent_party_raw), NA, parent_party_raw)
  )

pca_data <- df2 %>%
  select(parent_edu, parent_isei_raw, parent_party_raw) %>%
  filter(complete.cases(.))

if (nrow(pca_data) == 0) stop("PCA inputs are all missing.")

pca_fit <- prcomp(pca_data, center = TRUE, scale. = TRUE)

df2 <- df2 %>%
  mutate(parent_ses = NA_real_)

idx_complete <- complete.cases(df2[, c("parent_edu", "parent_isei_raw", "parent_party_raw")])

df2$parent_ses[idx_complete] <- predict(
  pca_fit,
  newdata = df2[idx_complete, c("parent_edu", "parent_isei_raw", "parent_party_raw")]
)[, 1]

if (cor(df2$parent_ses, df2$parent_edu, use = "complete.obs") < 0) {
  df2$parent_ses <- -df2$parent_ses
}

df2 <- df2 %>%
  mutate(parent_ses = as.numeric(scale(parent_ses)))

cat("PCA loadings:\n")
print(round(pca_fit$rotation, 3))
cat("\nVariance explained by PC1:\n")
print(round(summary(pca_fit)$importance[2, 1], 3))

# =========================================================
# 4. Year-specific regressions
# =========================================================
df_use <- df2 %>%
  filter(!is.na(year), !is.na(child_isei), !is.na(parent_ses))

year_stats <- df_use %>%
  count(year, name = "n")

min_n <- 80
min_n_latest <- 20
latest_year <- max(year_stats$year, na.rm = TRUE)

valid_years <- year_stats %>%
  filter(n >= min_n | (year == latest_year & n >= min_n_latest)) %>%
  pull(year)

if (length(valid_years) == 0) {
  stop("No year has enough observations for regression.")
}

year_coef <- df_use %>%
  filter(year %in% valid_years) %>%
  group_by(year) %>%
  group_modify(~{
    fit <- lm(child_isei ~ parent_ses, data = .x)
    tidy(fit, conf.int = TRUE) %>%
      filter(term == "parent_ses")
  }) %>%
  ungroup() %>%
  left_join(year_stats, by = "year") %>%
  mutate(
    year = as.integer(year),
    sig = p.value < 0.05
  ) %>%
  arrange(year)

print(year_coef)

if (!latest_year %in% year_coef$year) {
  warning("Latest year is not included in year_coef after filtering.")
}

# =========================================================
# 5. Fancy figure
# =========================================================
y_min <- min(year_coef$conf.low, na.rm = TRUE) - 0.05
y_max <- max(year_coef$conf.high, na.rm = TRUE) + 0.05

# Build a smooth trend with confidence band for a richer look
lo_fit <- loess(estimate ~ year, data = year_coef, span = 0.75)
lo_grid <- data.frame(year = seq(min(year_coef$year), max(year_coef$year), length.out = 220))
lo_pred <- predict(lo_fit, newdata = lo_grid, se = TRUE)
lo_grid <- lo_grid %>%
  mutate(
    fit = lo_pred$fit,
    se = lo_pred$se.fit,
    low = fit - 1.96 * se,
    high = fit + 1.96 * se
  )

# Highlight the local max and min years
peak_row <- year_coef %>% slice_max(estimate, n = 1, with_ties = FALSE)
low_row  <- year_coef %>% slice_min(estimate, n = 1, with_ties = FALSE)
last_row <- year_coef %>% slice_max(year, n = 1, with_ties = FALSE)

# Soft horizontal bands to create a poster-like depth
band_breaks <- pretty(c(y_min, y_max), n = 5)
band_dat <- data.frame(
  ymin = head(band_breaks, -1),
  ymax = tail(band_breaks, -1),
  id = seq_len(length(band_breaks) - 1)
)

p_year <- ggplot(year_coef, aes(x = year, y = estimate, color = estimate)) +
  geom_rect(
    data = band_dat,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = id),
    inherit.aes = FALSE,
    alpha = 0.2,
    color = NA
  ) +
  geom_hline(
    yintercept = 0,
    color = "#7F8EA3",
    linewidth = 0.9,
    linetype = "22"
  ) +
  geom_ribbon(
    data = lo_grid,
    aes(x = year, ymin = low, ymax = high),
    inherit.aes = FALSE,
    fill = alpha("#7EA7D8", 0.24),
    color = NA
  ) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    linewidth = 2.2,
    alpha = 0.92
  ) +
  geom_segment(
    aes(x = year, xend = year, y = 0, yend = estimate),
    color = alpha("#94A3B8", 0.28),
    linewidth = 0.9
  ) +
  geom_point(
    aes(size = n, shape = sig),
    stroke = 1.2,
    fill = "white"
  ) +
  geom_line(
    aes(group = 1),
    color = alpha("#3D4A63", 0.35),
    linewidth = 0.95
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    span = 0.75,
    color = "#153E75",
    linewidth = 1.5
  ) +
  geom_text(
    data = year_coef %>% filter(year == min(year) | year == max(year)),
    aes(label = sprintf("%.2f", estimate)),
    nudge_y = 0.045,
    color = "#1F2937",
    size = 4.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_label(
    data = peak_row,
    aes(x = year, y = estimate + 0.06, label = paste0("Peak ", year, ": ", sprintf("%.2f", estimate))),
    inherit.aes = FALSE,
    fill = alpha("#E7F0FB", 0.98),
    color = "#17406F",
    fontface = "bold",
    size = 3.7,
    label.size = 0,
    label.padding = unit(0.14, "lines")
  ) +
  geom_label(
    data = low_row,
    aes(x = year, y = estimate - 0.06, label = paste0("Low ", year, ": ", sprintf("%.2f", estimate))),
    inherit.aes = FALSE,
    fill = alpha("#F3F6FA", 0.98),
    color = "#3F4D63",
    fontface = "bold",
    size = 3.5,
    label.size = 0,
    label.padding = unit(0.14, "lines")
  ) +
  annotate(
    "label",
    x = max(year_coef$year) - 0.1,
    y = y_min + 0.08,
    label = paste0(
      "Latest year: ", last_row$year,
      "\nCoef: ", sprintf("%.2f", last_row$estimate),
      "  |  N: ", comma(last_row$n)
    ),
    hjust = 1,
    vjust = 0,
    fill = alpha("white", 0.95),
    color = "#1F2937",
    size = 3.5,
    label.size = 0
  ) +
  scale_fill_gradient(
    low = "#F8FBFF",
    high = "#DCEAF8",
    guide = "none"
  ) +
  scale_color_gradient2(
    low = "#A9C8E8",
    mid = "#4F80B8",
    high = "#113F79",
    midpoint = median(year_coef$estimate, na.rm = TRUE),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 1),
    labels = c(`TRUE` = "p < 0.05", `FALSE` = "Not significant"),
    name = NULL
  ) +
  scale_size_continuous(
    range = c(3.2, 8.4),
    breaks = pretty_breaks(3),
    name = "Sample size"
  ) +
  scale_x_continuous(
    breaks = year_coef$year,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  labs(
    title = "Intergenerational Transmission Dynamics by Year",
    subtitle = "Parental SES effect on child ISEI with yearly estimates, 95% CI, and smoothed trajectory",
    x = NULL,
    y = "Coefficient estimate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11.3, color = "grey30", hjust = 0.5, margin = margin(b = 14)),
    axis.text.x = element_text(size = 10, color = "#4B5563"),
    axis.text.y = element_text(size = 11, color = "#374151"),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E3E9F2", linewidth = 0.65),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 4),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(16, 16, 14, 14)
  )

print(p_year)

ggsave(
  file.path("2_keyfindings_yearcoef", "yearcoef_fancy.png"),
  p_year,
  width = 11,
  height = 6,
  dpi = 600,
  bg = "white"
)

# =========================================================
# 6. Super poster style (light theme, two small panels)
# =========================================================

# Left small panel: timeline trend with smooth line
p_year_left <- p_year +
  labs(
    title = "Yearly Dynamics",
    subtitle = "Smoothed trajectory with yearly CI"
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    legend.position = "none",
    plot.margin = margin(8, 8, 8, 8)
  )

# Right small panel: ranking-card style by year
year_rank_dat <- year_coef %>%
  arrange(desc(estimate)) %>%
  mutate(
    rank = row_number(),
    year_f = factor(year, levels = rev(year))
  )

xmin_rank <- min(year_rank_dat$conf.low, na.rm = TRUE) - 0.14
xmax_rank <- max(year_rank_dat$conf.high, na.rm = TRUE) + 0.14
label_x_rank <- xmax_rank - 0.02
card_fill_seq <- colorRampPalette(c("#DDEBFA", "#F7FAFE"))(nrow(year_rank_dat))

card_dat_rank <- year_rank_dat %>%
  mutate(
    card_fill = card_fill_seq[rank],
    card_x = (xmin_rank + xmax_rank) / 2,
    card_width = xmax_rank - xmin_rank,
    card_height = 0.84
  )

p_year_right <- ggplot(year_rank_dat, aes(x = estimate, y = year_f)) +
  geom_tile(
    data = card_dat_rank,
    aes(x = card_x, y = year_f, width = card_width, height = card_height),
    inherit.aes = FALSE,
    fill = card_dat_rank$card_fill,
    color = "white",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = seq(0, floor(xmax_rank * 10) / 10, by = 0.1),
    color = "#E4EAF2",
    linewidth = 0.45
  ) +
  geom_vline(
    xintercept = 0,
    color = "#AAB3BE",
    linewidth = 0.9,
    linetype = "22"
  ) +
  geom_segment(
    aes(x = 0, xend = estimate, y = year_f, yend = year_f),
    color = "#C9D9EC",
    linewidth = 1.8,
    lineend = "round"
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    color = "#4F80B8",
    height = 0,
    linewidth = 1.8
  ) +
  geom_point(
    aes(size = n, fill = estimate),
    shape = 21,
    color = "white",
    stroke = 1.0
  ) +
  geom_label(
    aes(x = label_x_rank, label = sprintf("%.2f", estimate)),
    size = 3.35,
    fontface = "bold",
    fill = alpha("white", 0.96),
    color = "#2F3A4C",
    label.size = 0,
    hjust = 1
  ) +
  scale_fill_gradient2(
    low = "#AFCDEA",
    mid = "#5A86BE",
    high = "#224F92",
    midpoint = median(year_rank_dat$estimate, na.rm = TRUE),
    guide = "none"
  ) +
  scale_size_continuous(range = c(3.0, 6.2), guide = "none") +
  scale_x_continuous(
    limits = c(xmin_rank, xmax_rank),
    breaks = pretty_breaks(4),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Year Ranking Card",
    subtitle = "Coefficient size by year",
    x = "Coefficient estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "grey35"),
    axis.text.y = element_text(size = 8.5, color = "#2F3540"),
    axis.text.x = element_text(size = 9.5, color = "#4B5563"),
    axis.title.x = element_text(size = 10.5),
    panel.grid = element_blank(),
    plot.margin = margin(8, 10, 8, 4)
  )

year_two_panel <- p_year_left + p_year_right +
  plot_layout(widths = c(1.52, 1.0))

print(year_two_panel)

ggsave(
  file.path("2_keyfindings_yearcoef", "yearcoef_superposter_twopanel_light.png"),
  year_two_panel,
  width = 12.4,
  height = 6.2,
  dpi = 650,
  bg = "white"
)
