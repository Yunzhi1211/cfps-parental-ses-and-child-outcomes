# =========================================================
# 0. Packages
# =========================================================
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(scales)
library(patchwork)
library(quantreg)

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
# 4. Year-specific quantile regressions (RQ2 core)
# =========================================================
df_use <- df2 %>%
  filter(!is.na(year), !is.na(child_isei), !is.na(parent_ses))

safe_qcoef <- function(dat, tau) {
  tryCatch({
    fit <- rq(child_isei ~ parent_ses, tau = tau, data = dat)
    unname(coef(fit)[["parent_ses"]])
  }, error = function(e) NA_real_)
}

boot_qdiff <- function(dat, reps = 250) {
  est <- safe_qcoef(dat, 0.75) - safe_qcoef(dat, 0.25)
  n <- nrow(dat)
  if (!is.finite(est) || n < 120) {
    return(tibble(qdiff = est, qdiff_lo = NA_real_, qdiff_hi = NA_real_))
  }
  diffs <- rep(NA_real_, reps)
  for (b in seq_len(reps)) {
    ds <- dat[sample.int(n, n, replace = TRUE), , drop = FALSE]
    c25 <- safe_qcoef(ds, 0.25)
    c75 <- safe_qcoef(ds, 0.75)
    diffs[b] <- c75 - c25
  }
  diffs <- diffs[is.finite(diffs)]
  if (length(diffs) < 30) {
    return(tibble(qdiff = est, qdiff_lo = NA_real_, qdiff_hi = NA_real_))
  }
  tibble(
    qdiff = est,
    qdiff_lo = as.numeric(quantile(diffs, 0.025, na.rm = TRUE)),
    qdiff_hi = as.numeric(quantile(diffs, 0.975, na.rm = TRUE))
  )
}

year_stats <- df_use %>%
  count(year, name = "n")

valid_years <- year_stats %>%
  filter(n >= 100) %>%
  pull(year)

year_q <- df_use %>%
  filter(year %in% valid_years) %>%
  group_by(year) %>%
  group_modify(~{
    qd <- boot_qdiff(.x, reps = 250)
    tibble(
      q25 = safe_qcoef(.x, 0.25),
      q50 = safe_qcoef(.x, 0.50),
      q75 = safe_qcoef(.x, 0.75),
      qdiff = qd$qdiff,
      qdiff_lo = qd$qdiff_lo,
      qdiff_hi = qd$qdiff_hi
    )
  }) %>%
  ungroup() %>%
  left_join(year_stats, by = "year") %>%
  mutate(
    sig_diff = ifelse(is.na(qdiff_lo) | is.na(qdiff_hi), FALSE, qdiff_lo > 0 | qdiff_hi < 0)
  ) %>%
  arrange(year)

print(year_q)

line_dat <- bind_rows(
  year_q %>% transmute(year, quantile = "Q25", beta = q25),
  year_q %>% transmute(year, quantile = "Q50", beta = q50),
  year_q %>% transmute(year, quantile = "Q75", beta = q75)
) %>%
  mutate(quantile = factor(quantile, levels = c("Q25", "Q50", "Q75")))

# =========================================================
# 5. Two-panel RQ2 figure
# =========================================================
yr_min <- min(year_q$year, na.rm = TRUE)
yr_max <- max(year_q$year, na.rm = TRUE)
y_left_min <- min(line_dat$beta, na.rm = TRUE) - 0.25
y_left_max <- max(line_dat$beta, na.rm = TRUE) + 0.35
y_right_min <- min(year_q$qdiff_lo, na.rm = TRUE) - 0.2
y_right_max <- max(year_q$qdiff_hi, na.rm = TRUE) + 0.2

peak_gap <- year_q %>% slice_max(qdiff, n = 1, with_ties = FALSE)
last_gap <- year_q %>% slice_max(year, n = 1, with_ties = FALSE)

p_left <- ggplot(line_dat, aes(x = year, y = beta, color = quantile)) +
  geom_rect(
    data = data.frame(
      xmin = yr_min - 0.6, xmax = yr_max + 0.6,
      ymin = -Inf, ymax = Inf
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "#F5F9FF",
    color = NA
  ) +
  geom_line(linewidth = 1.9, alpha = 0.98, show.legend = FALSE) +
  geom_point(size = 3.9, stroke = 1.0, fill = "white", shape = 21, show.legend = FALSE) +
  scale_color_manual(values = c("Q25" = "#9CC1E4", "Q50" = "#4B83BA", "Q75" = "#133F72")) +
  labs(
    title = "Time Heterogeneity Across Quantiles",
    subtitle = "Separated panels make quantile trajectories directly comparable",
    x = NULL,
    y = "Coefficient"
  ) +
  scale_x_continuous(
    breaks = sort(unique(year_q$year)),
    limits = c(yr_min - 0.55, yr_max + 0.65)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.12))) +
  facet_wrap(~quantile, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 21, hjust = 0.5, color = "#163E68"),
    plot.subtitle = element_text(size = 11.3, hjust = 0.5, color = "#35597A", margin = margin(b = 8)),
    axis.text.x = element_text(size = 10.3, face = "bold", color = "#41566E"),
    axis.text.y = element_text(size = 10.3, face = "bold", color = "#41566E"),
    axis.title.y = element_text(size = 14, face = "bold", color = "#1F3F5F"),
    panel.grid.major.x = element_line(color = "#E3EAF3", linewidth = 0.7),
    panel.grid.major.y = element_line(color = "#E3EAF3", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 11.5, face = "bold", color = "#173E67"),
    strip.background = element_rect(fill = "#E8F1FC", color = NA),
    panel.background = element_rect(fill = "#F8FBFF", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.margin = margin(12, 14, 12, 14)
  )

p_right <- ggplot(year_q, aes(x = year, y = qdiff)) +
  geom_rect(
    data = data.frame(xmin = yr_min - 0.5, xmax = yr_max + 0.5, ymin = y_right_min, ymax = y_right_max),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "#F9FBFF",
    color = NA
  ) +
  geom_hline(yintercept = 0, color = "#8A98AB", linetype = "22", linewidth = 0.9) +
  geom_ribbon(aes(ymin = qdiff_lo, ymax = qdiff_hi), fill = "#CCE0F7", alpha = 0.82) +
  geom_line(color = "#BC2D2A", linewidth = 1.9) +
  geom_point(aes(shape = sig_diff), size = 4.1, color = "#BC2D2A", fill = "white", stroke = 1.2) +
  geom_label(
    data = peak_gap,
    aes(label = paste0("Peak ", year, ": ", sprintf("%.2f", qdiff))),
    nudge_y = 0.45,
    size = 3.5,
    fontface = "bold",
    fill = alpha("white", 0.95),
    color = "#9E1F1C",
    label.size = 0
  ) +
  geom_label(
    data = last_gap,
    aes(label = paste0("Latest ", year, ": ", sprintf("%.2f", qdiff))),
    nudge_y = -0.42,
    size = 3.3,
    fontface = "bold",
    fill = alpha("white", 0.95),
    color = "#2B415A",
    label.size = 0
  ) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c(`TRUE` = "CI excludes 0", `FALSE` = "CI includes 0")) +
  labs(
    title = "Is Q75 > Q25 Over Time?",
    subtitle = "Gap = beta(Q75) - beta(Q25), with 95% bootstrap CI (solid point: CI excludes 0)",
    x = NULL,
    y = "Q75 - Q25"
  ) +
  scale_x_continuous(breaks = sort(unique(year_q$year))) +
  coord_cartesian(ylim = c(y_right_min, y_right_max)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 21, hjust = 0.5, color = "#163E68"),
    plot.subtitle = element_text(size = 11.3, hjust = 0.5, color = "#35597A", margin = margin(b = 8)),
    axis.text.x = element_text(size = 10.5, face = "bold", color = "#41566E"),
    axis.text.y = element_text(size = 10.5, face = "bold", color = "#41566E"),
    axis.title.y = element_text(size = 12.5, face = "bold", color = "#1F3F5F"),
    panel.grid.major.x = element_line(color = "#E3EAF3", linewidth = 0.7),
    panel.grid.major.y = element_line(color = "#E3EAF3", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F8FBFF", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.margin = margin(12, 14, 12, 14)
  )

year_two_panel <- p_left + p_right + plot_layout(widths = c(1, 1))

print(year_two_panel)

ggsave(
  file.path("2_keyfindings_yearcoef", "yearcoef_fancy.png"),
  year_two_panel,
  width = 11,
  height = 6,
  dpi = 600,
  bg = "white"
)
