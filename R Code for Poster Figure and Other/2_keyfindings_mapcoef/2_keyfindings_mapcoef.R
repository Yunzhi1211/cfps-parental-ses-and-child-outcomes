# =========================================================
# 0. Packages
# =========================================================
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(broom)
library(patchwork)
library(forcats)
library(scales)
library(grid)
library(quantreg)

use_shadowtext <- requireNamespace("shadowtext", quietly = TRUE)

# =========================================================
# 1. Read panel data
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
# 3. Build parent SES using PCA
# =========================================================
df2 <- df %>%
  mutate(
    child_isei = to_num(child_isei),
    child_province = to_num(child_province),
    year = to_num(year),
    
    f_edu   = to_num(f_edu),
    m_edu   = to_num(m_edu),
    f_isei  = to_num(f_isei),
    m_isei  = to_num(m_isei),
    f_party = to_num(f_party),
    m_party = to_num(m_party),
    
    parent_edu       = rowMeans(cbind(f_edu,   m_edu),   na.rm = TRUE),
    parent_isei_raw  = rowMeans(cbind(f_isei,  m_isei),  na.rm = TRUE),
    parent_party_raw = rowMeans(cbind(f_party, m_party), na.rm = TRUE)
  ) %>%
  mutate(
    parent_edu       = ifelse(is.nan(parent_edu), NA, parent_edu),
    parent_isei_raw  = ifelse(is.nan(parent_isei_raw), NA, parent_isei_raw),
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
# 4. Province + Region mapping
# =========================================================
prov_map <- tibble(
  child_province = c(11,12,13,14,15,21,22,23,31,32,33,34,35,36,37,
                     41,42,43,44,45,46,50,51,52,53,54,61,62,63,64,65),
  prov_name = c("Beijing","Tianjin","Hebei","Shanxi","Inner Mongolia",
                "Liaoning","Jilin","Heilongjiang","Shanghai","Jiangsu",
                "Zhejiang","Anhui","Fujian","Jiangxi","Shandong",
                "Henan","Hubei","Hunan","Guangdong","Guangxi","Hainan",
                "Chongqing","Sichuan","Guizhou","Yunnan","Tibet",
                "Shaanxi","Gansu","Qinghai","Ningxia","Xinjiang")
)

region_map <- tibble(
  prov_name = c(
    "Beijing","Tianjin","Hebei","Shanghai","Jiangsu","Zhejiang","Fujian",
    "Shandong","Guangdong","Hainan",
    "Shanxi","Anhui","Jiangxi","Henan","Hubei","Hunan",
    "Inner Mongolia","Guangxi","Chongqing","Sichuan","Guizhou","Yunnan",
    "Tibet","Shaanxi","Gansu","Qinghai","Ningxia","Xinjiang",
    "Liaoning","Jilin","Heilongjiang"
  ),
  region_label = c(
    rep("East", 10),
    rep("Central", 6),
    rep("West", 12),
    rep("Northeast", 3)
  )
)

# Excluded from estimation but shown as grey outlines
drop_prov <- c("Xinjiang", "Tibet")

# =========================================================
# 5. Prepare analysis data
# =========================================================
df_use <- df2 %>%
  left_join(prov_map, by = "child_province") %>%
  left_join(region_map, by = "prov_name") %>%
  filter(
    !is.na(child_isei),
    !is.na(parent_ses),
    !is.na(region_label),
    !is.na(prov_name),
    !prov_name %in% drop_prov
  )

cat("\nObservations by region:\n")
print(table(df_use$region_label))

cat("\nObservations by province (analysis sample):\n")
print(sort(table(df_use$prov_name), decreasing = TRUE))

# =========================================================
# 6. Region-specific regressions
# =========================================================
safe_qcoef <- function(dat, tau) {
  tryCatch({
    fit <- rq(child_isei ~ parent_ses, tau = tau, data = dat)
    unname(coef(fit)[["parent_ses"]])
  }, error = function(e) NA_real_)
}

boot_qdiff <- function(dat, reps = 300) {
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
  if (length(diffs) < 40) {
    return(tibble(qdiff = est, qdiff_lo = NA_real_, qdiff_hi = NA_real_))
  }
  tibble(
    qdiff = est,
    qdiff_lo = as.numeric(quantile(diffs, 0.025, na.rm = TRUE)),
    qdiff_hi = as.numeric(quantile(diffs, 0.975, na.rm = TRUE))
  )
}

region_coef <- df_use %>%
  group_by(region_label) %>%
  group_modify(~{
    qd <- boot_qdiff(.x, reps = 300)
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
  mutate(diff_sig = ifelse(is.na(qdiff_lo) | is.na(qdiff_hi), NA, qdiff_lo > 0 | qdiff_hi < 0))

print(region_coef)

# =========================================================
# 7. Read shapefile
# =========================================================
china_map <- st_read(
  file.path("..", "0_raw", "gadm41_CHN_1.shp"),
  quiet = TRUE
)

china_map <- china_map %>%
  rename(prov_name_raw = NAME_1) %>%
  mutate(
    prov_name = case_when(
      prov_name_raw == "Beijing" ~ "Beijing",
      prov_name_raw == "Tianjin" ~ "Tianjin",
      prov_name_raw == "Hebei" ~ "Hebei",
      prov_name_raw == "Shanxi" ~ "Shanxi",
      prov_name_raw %in% c("Nei Mongol", "Inner Mongolia") ~ "Inner Mongolia",
      prov_name_raw == "Liaoning" ~ "Liaoning",
      prov_name_raw == "Jilin" ~ "Jilin",
      prov_name_raw == "Heilongjiang" ~ "Heilongjiang",
      prov_name_raw == "Shanghai" ~ "Shanghai",
      prov_name_raw == "Jiangsu" ~ "Jiangsu",
      prov_name_raw == "Zhejiang" ~ "Zhejiang",
      prov_name_raw == "Anhui" ~ "Anhui",
      prov_name_raw == "Fujian" ~ "Fujian",
      prov_name_raw == "Jiangxi" ~ "Jiangxi",
      prov_name_raw == "Shandong" ~ "Shandong",
      prov_name_raw == "Henan" ~ "Henan",
      prov_name_raw == "Hubei" ~ "Hubei",
      prov_name_raw == "Hunan" ~ "Hunan",
      prov_name_raw == "Guangdong" ~ "Guangdong",
      prov_name_raw %in% c("Guangxi", "Guangxi Zhuang") ~ "Guangxi",
      prov_name_raw == "Hainan" ~ "Hainan",
      prov_name_raw == "Chongqing" ~ "Chongqing",
      prov_name_raw == "Sichuan" ~ "Sichuan",
      prov_name_raw == "Guizhou" ~ "Guizhou",
      prov_name_raw == "Yunnan" ~ "Yunnan",
      prov_name_raw %in% c("Xizang", "Tibet") ~ "Tibet",
      prov_name_raw == "Shaanxi" ~ "Shaanxi",
      prov_name_raw == "Gansu" ~ "Gansu",
      prov_name_raw == "Qinghai" ~ "Qinghai",
      prov_name_raw %in% c("Ningxia", "Ningxia Hui") ~ "Ningxia",
      prov_name_raw %in% c("Xinjiang", "Xinjiang Uygur") ~ "Xinjiang",
      TRUE ~ prov_name_raw
    )
  )

china_map2 <- china_map %>%
  left_join(region_map, by = "prov_name") %>%
  filter(prov_name %in% prov_map$prov_name)

# =========================================================
# 8. Map data:
#    - included provinces get coefficients
#    - Xinjiang/Tibet kept as excluded provinces
# =========================================================
region_coef2 <- region_coef %>%
  mutate(region_label = as.character(region_label))

china_plot_dat <- china_map2 %>%
  left_join(region_coef2, by = "region_label") %>%
  mutate(
    map_status = ifelse(prov_name %in% drop_prov, "excluded", "included"),
    estimate_map = ifelse(prov_name %in% drop_prov, NA, qdiff)
  )

map_included <- china_plot_dat %>%
  filter(map_status == "included")

map_excluded <- china_plot_dat %>%
  filter(map_status == "excluded")

region_outline <- map_included %>%
  group_by(region_label) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Manually adjusted label anchor points
region_labs <- data.frame(
  region_label = c("East", "Central", "West", "Northeast"),
  lon = c(118.3, 112.3, 101.2, 126.0),
  lat = c(29.0, 33.7, 35.8, 45.5)
)

# =========================================================
# 9. Aesthetic settings
# =========================================================
map_palette <- c("#DCEBFA", "#AFCDEA", "#6D97C7", "#224F92")

coef_palette <- c(
  "East" = "#224F92",
  "Central" = "#5A86BE",
  "West" = "#AFCDEA",
  "Northeast" = "#7897B5"
)

# =========================================================
# 10. Left panel: map with Xinjiang/Tibet greyed out
# =========================================================
fill_breaks <- seq(
  floor(min(map_included$estimate_map, na.rm = TRUE) * 2) / 2,
  ceiling(max(map_included$estimate_map, na.rm = TRUE) * 2) / 2,
  length.out = 5
)

p_map_base <- ggplot() +
  # excluded provinces: grey
  geom_sf(
    data = map_excluded,
    fill = "#E6E8EC",
    color = "white",
    linewidth = 0.9,
    linejoin = "round"
  ) +
  # included provinces: colored by coefficient
  geom_sf(
    data = map_included,
    aes(fill = estimate_map),
    color = "white",
    linewidth = 0.9,
    linejoin = "round"
  ) +
  # region boundaries only for included provinces
  geom_sf(
    data = region_outline,
    fill = NA,
    color = "white",
    linewidth = 1.8,
    linejoin = "round"
  ) +
  # emphasise excluded province outline
  geom_sf(
    data = map_excluded,
    fill = NA,
    color = "#C9CDD4",
    linewidth = 1.1,
    linejoin = "round"
  ) +
  scale_fill_stepsn(
    colors = map_palette,
    breaks = fill_breaks,
    limits = range(fill_breaks),
    guide = guide_colorsteps(
      title = "Coefficient",
      title.position = "top",
      barwidth = unit(7.5, "cm"),
      barheight = unit(0.45, "cm"),
      even.steps = TRUE
    )
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Regional Heterogeneity in Quantile Gaps",
    subtitle = "Map shows Q75 - Q25 effect of parental SES"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11.8, color = "grey35", hjust = 0.5, margin = margin(b = 12)),
    legend.position = c(0.46, 0.09),
    legend.direction = "horizontal",
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text = element_text(size = 9.5),
    plot.margin = margin(10, 5, 10, 10)
  )

if (use_shadowtext) {
  p_map_final <- p_map_base +
    shadowtext::geom_shadowtext(
      data = region_labs,
      aes(x = lon, y = lat, label = region_label),
      color = "white",
      bg.color = alpha("#1E3E66", 0.85),
      fontface = "bold",
      size = 5.4
    ) +
    annotate(
      "label",
      x = 88.5, y = 39.5,
      label = "Excluded:\nXinjiang, Tibet",
      fill = alpha("#F3F4F6", 0.95),
      color = "#5B6470",
      size = 3.6,
      fontface = "plain",
      label.size = 0
    )
} else {
  p_map_final <- p_map_base +
    geom_label(
      data = region_labs,
      aes(x = lon, y = lat, label = region_label),
      fill = alpha("#1E3E66", 0.85),
      color = "white",
      fontface = "bold",
      size = 5,
      label.size = 0,
      label.padding = unit(0.18, "lines")
    ) +
    annotate(
      "label",
      x = 88.5, y = 39.5,
      label = "Excluded:\nXinjiang, Tibet",
      fill = alpha("#F3F4F6", 0.95),
      color = "#5B6470",
      size = 3.6,
      fontface = "plain",
      label.size = 0
    )
}

# =========================================================
# 11. Right panel: dumbbell (Q25 vs Q75)
# =========================================================
coef_plot_dat <- region_coef2 %>%
  arrange(desc(qdiff)) %>%
  mutate(region_label = factor(region_label, levels = rev(region_label)))

xmax_plot <- max(coef_plot_dat$q75, na.rm = TRUE) + 0.5

p_coef_final <- ggplot(coef_plot_dat, aes(y = region_label)) +
  geom_segment(
    aes(x = q25, xend = q75, yend = region_label),
    color = "#97B5D6",
    linewidth = 3.0,
    lineend = "round"
  ) +
  geom_point(
    aes(x = q25),
    size = 5.2,
    color = "#6FA0CF",
    fill = "white",
    shape = 21,
    stroke = 1.2
  ) +
  geom_point(
    aes(x = q75),
    size = 5.8,
    color = "#1E4F8C",
    fill = "#1E4F8C",
    shape = 21,
    stroke = 1.0
  ) +
  geom_text(
    aes(x = q25, label = "Q25"),
    nudge_y = -0.23,
    color = "#6FA0CF",
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    aes(x = q75, label = "Q75"),
    nudge_y = -0.23,
    color = "#1E4F8C",
    size = 3.5,
    fontface = "bold"
  ) +
  geom_label(
    aes(x = q75 - 0.10, label = paste0("Gap=", sprintf("%.2f", qdiff))),
    hjust = 1,
    size = 4.2,
    fontface = "bold",
    fill = alpha("white", 0.95),
    color = "#374151",
    label.size = 0
  ) +
  scale_x_continuous(
    limits = c(min(coef_plot_dat$q25, na.rm = TRUE) - 0.2, xmax_plot),
    breaks = pretty_breaks(5)
  ) +
  labs(
    title = "Q25 vs Q75 by Region",
    subtitle = "Dumbbell length = quantile gap (Q75 - Q25)",
    x = "Parental SES coefficient",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11.8, color = "grey35", hjust = 0.5, margin = margin(b = 14)),
    axis.text.y = element_text(size = 20, color = "#2F3540"),
    axis.text.x = element_text(size = 12, color = "#4B5563"),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E2E8F0", linewidth = 0.7),
    plot.margin = margin(30, 25, 10, 0)
  )

# =========================================================
# 12. Combine and save
# =========================================================
final_region_poster <- p_map_final + p_coef_final +
  plot_layout(widths = c(1.7, 1.0))

print(final_region_poster)

ggsave(
  file.path("2_keyfindings_mapcoef", "regional_heterogeneity_poster_rankcard_greyXT.png"),
  final_region_poster,
  width = 12.8,
  height = 6.5,
  dpi = 600,
  bg = "white"
)
