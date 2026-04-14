# =========================
# poster_figures.R
# =========================

library(haven)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(quantreg)
library(tidyr)
library(purrr)
library(sf)
library(haven)
library(patchwork)
library(grid)
library(scales)

# -------------------------
# 1. Read data
# -------------------------
df <- read_dta(file.path("..", "3_output", "panel", "Panel_data.dta"))

# -------------------------
# 2. Construct parent_party_any
# -------------------------
df <- df %>%
  mutate(
    f_party = as.numeric(f_party),
    m_party = as.numeric(m_party)
  ) %>%
  mutate(
    parent_party_any = case_when(
      !is.na(f_party) | !is.na(m_party) ~ as.numeric((f_party > 0) | (m_party > 0)),
      TRUE ~ NA_real_
    )
  )

# -------------------------
# 3. PCA for parental SES
# -------------------------
pca_vars <- c("f_isei", "m_isei", "parent_party_any", "f_edu", "m_edu")

pca_index <- complete.cases(df[, pca_vars])

pca_fit <- prcomp(df[pca_index, pca_vars], center = TRUE, scale. = TRUE)

df$parent_ses_pca <- NA_real_
df$parent_ses_pca[pca_index] <- predict(pca_fit)[, 1]

# -------------------------
# 4. Figure 1: PCA loadings
# -------------------------
loadings_df <- data.frame(
  variable = rownames(pca_fit$rotation),
  loading = pca_fit$rotation[, 1]
)

p_pca <- ggplot(loadings_df, aes(x = reorder(variable, loading), y = loading, fill = loading > 0)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("#9ECAE1", "#08519C")) +
  labs(
    title = "Components of the parental SES index (PC1 loadings)",
    x = NULL,
    y = "PCA loading"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18)
  )

ggsave("pca_loadings_parent_ses.png", p_pca, width = 9, height = 6, dpi = 300)

# -------------------------
# 5. Figure 2: Distribution plot
# -------------------------
df_dist <- df %>%
  filter(!is.na(parent_ses_pca), !is.na(child_isei)) %>%
  mutate(
    parent_ses_group = ntile(parent_ses_pca, 3),
    parent_ses_group = factor(
      parent_ses_group,
      levels = 1:3,
      labels = c("Low parental SES", "Middle parental SES", "High parental SES")
    )
  )

p_dist <- ggplot(df_dist, aes(x = parent_ses_group, y = child_isei, fill = parent_ses_group)) +
  geom_violin(trim = FALSE, alpha = 0.75, color = NA) +
  geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", color = "#2B2B2B") +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  scale_fill_manual(values = c("#C6DBEF", "#6BAED6", "#2171B5")) +
  labs(
    title = "Distribution of child occupational attainment by parental SES",
    x = "Parental SES group",
    y = "Child occupational attainment (ISEI)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold")
  )

ggsave("distribution_child_isei_by_parent_ses.png", p_dist, width = 10, height = 7, dpi = 300)

# -------------------------
# 6. Figure 3: Sankey plot
# -------------------------
df_flow <- df %>%
  filter(!is.na(parent_ses_pca), !is.na(child_isei)) %>%
  mutate(
    parent_ses_group = ntile(parent_ses_pca, 3),
    child_occ_group  = ntile(child_isei, 3)
  ) %>%
  mutate(
    parent_ses_group = factor(
      parent_ses_group,
      levels = 1:3,
      labels = c("Low parental SES", "Middle parental SES", "High parental SES")
    ),
    child_occ_group = factor(
      child_occ_group,
      levels = 1:3,
      labels = c("Low child occupation", "Middle child occupation", "High child occupation")
    )
  )

flow_tab <- df_flow %>%
  count(parent_ses_group, child_occ_group)

p_sankey <- ggplot(
  flow_tab,
  aes(axis1 = parent_ses_group, axis2 = child_occ_group, y = n)
) +
  geom_alluvium(aes(fill = parent_ses_group), width = 0.22, alpha = 0.85) +
  geom_stratum(width = 0.28, fill = "#EEF3FA", color = "#4A6484", linewidth = 0.6) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4.5, fontface = "bold") +
  scale_x_discrete(
    limits = c("Parental SES", "Child occupation"),
    expand = c(.08, .08)
  ) +
  scale_fill_manual(values = c("#9ECAE1", "#3182BD", "#08519C")) +
  labs(
    title = "Intergenerational mobility from parental SES to child occupation",
    y = "Number of respondents",
    x = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 18)
  )

ggsave("sankey_intergenerational.png", p_sankey, width = 12, height = 7, dpi = 300)

# -------------------------
# 7. Figure 4: Year × Quantile heatmap
# -------------------------
taus <- seq(0.1, 0.9, by = 0.1)

df_qr <- df %>%
  filter(
    !is.na(year),
    !is.na(child_isei),
    !is.na(parent_ses_pca),
    !is.na(child_gender),
    !is.na(child_age),
    !is.na(child_urban)
  ) %>%
  group_by(year) %>%
  filter(n() >= 50) %>%
  ungroup()

qr_results <- df_qr %>%
  group_by(year) %>%
  group_modify(~{
    dat <- .x
    
    map_dfr(taus, function(tt) {
      out <- tryCatch({
        fit <- rq(
          child_isei ~ parent_ses_pca + child_gender + child_age + child_urban,
          tau = tt,
          data = dat
        )
        
        co <- summary(fit, se = "nid")$coefficients
        
        tibble(
          tau = tt,
          coef = co["parent_ses_pca", 1],
          se   = co["parent_ses_pca", 2]
        )
      }, error = function(e) {
        tibble(
          tau = tt,
          coef = NA_real_,
          se   = NA_real_
        )
      })
      
      out
    })
  }) %>%
  ungroup()

p_heat <- ggplot(qr_results, aes(x = factor(year), y = tau, fill = coef)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(
    low = "#EFF3FF",
    high = "#08519C",
    na.value = "grey90"
  ) +
  labs(
    title = "Year-by-quantile heterogeneity in parental SES effects",
    x = "Survey year",
    y = "Quantile",
    fill = "SES effect"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("heatmap_year_quantile_ses_effect.png", p_heat, width = 10, height = 7, dpi = 300)

# -------------------------
# 8. Figure 5: Province coefficient map only
# -------------------------

china_map <- st_read(file.path("..", "0_raw", "gadm41_CHN_1.shp"), quiet = TRUE)

exclude_provinces <- c(
  "Tibet", "Xizang", "Xizang Zizhiqu",
  "Xinjiang", "Xinjiang Uygur", "Xinjiang Uygur Zizhiqu"
)

df <- df %>%
  mutate(
    province_raw = as.character(as_factor(child_province)),
    province_map = case_when(
      province_raw %in% c("Beijing", "北京市", "北京") ~ "Beijing",
      province_raw %in% c("Tianjin", "天津市", "天津") ~ "Tianjin",
      province_raw %in% c("Hebei", "河北省", "河北") ~ "Hebei",
      province_raw %in% c("Shanxi", "山西省", "山西") ~ "Shanxi",
      province_raw %in% c("Inner Mongolia", "Nei Mongol", "内蒙古自治区", "内蒙古") ~ "Inner Mongolia",
      province_raw %in% c("Liaoning", "辽宁省", "辽宁") ~ "Liaoning",
      province_raw %in% c("Jilin", "吉林省", "吉林") ~ "Jilin",
      province_raw %in% c("Heilongjiang", "黑龙江省", "黑龙江") ~ "Heilongjiang",
      province_raw %in% c("Shanghai", "上海市", "上海") ~ "Shanghai",
      province_raw %in% c("Jiangsu", "江苏省", "江苏") ~ "Jiangsu",
      province_raw %in% c("Zhejiang", "浙江省", "浙江") ~ "Zhejiang",
      province_raw %in% c("Anhui", "安徽省", "安徽") ~ "Anhui",
      province_raw %in% c("Fujian", "福建省", "福建") ~ "Fujian",
      province_raw %in% c("Jiangxi", "江西省", "江西") ~ "Jiangxi",
      province_raw %in% c("Shandong", "山东省", "山东") ~ "Shandong",
      province_raw %in% c("Henan", "河南省", "河南") ~ "Henan",
      province_raw %in% c("Hubei", "湖北省", "湖北") ~ "Hubei",
      province_raw %in% c("Hunan", "湖南省", "湖南") ~ "Hunan",
      province_raw %in% c("Guangdong", "广东省", "广东") ~ "Guangdong",
      province_raw %in% c("Guangxi", "广西壮族自治区", "广西") ~ "Guangxi",
      province_raw %in% c("Hainan", "海南省", "海南") ~ "Hainan",
      province_raw %in% c("Chongqing", "重庆市", "重庆") ~ "Chongqing",
      province_raw %in% c("Sichuan", "四川省", "四川") ~ "Sichuan",
      province_raw %in% c("Guizhou", "贵州省", "贵州") ~ "Guizhou",
      province_raw %in% c("Yunnan", "云南省", "云南") ~ "Yunnan",
      province_raw %in% c("Tibet", "Xizang", "西藏自治区", "西藏") ~ "Tibet",
      province_raw %in% c("Shaanxi", "陕西省", "陕西") ~ "Shaanxi",
      province_raw %in% c("Gansu", "甘肃省", "甘肃") ~ "Gansu",
      province_raw %in% c("Qinghai", "青海省", "青海") ~ "Qinghai",
      province_raw %in% c("Ningxia", "宁夏回族自治区", "宁夏") ~ "Ningxia Hui",
      province_raw %in% c("Xinjiang", "新疆维吾尔自治区", "新疆") ~ "Xinjiang Uygur",
      province_raw %in% c("Taiwan", "台湾省", "台湾") ~ "Taiwan",
      province_raw %in% c("Hong Kong", "香港特别行政区", "香港") ~ "Hong Kong",
      province_raw %in% c("Macau", "澳门特别行政区", "澳门") ~ "Macao",
      TRUE ~ province_raw
    )
  )

# Reverse SES direction
df <- df %>% mutate(parent_ses_pca = -parent_ses_pca)

prov_coef <- df %>%
  filter(
    !is.na(province_map),
    !is.na(child_isei),
    !is.na(parent_ses_pca),
    !is.na(child_gender),
    !is.na(child_age),
    !is.na(child_urban)
  ) %>%
  group_by(province_map) %>%
  filter(n() >= 50) %>%
  group_modify(~{
    tryCatch({
      fit <- lm(
        child_isei ~ parent_ses_pca + child_gender + child_age + child_urban,
        data = .x
      )
      tibble(
        coef_parent_ses = coef(fit)[["parent_ses_pca"]],
        n = nrow(.x)
      )
    }, error = function(e) {
      tibble(
        coef_parent_ses = NA_real_,
        n = nrow(.x)
      )
    })
  }) %>%
  ungroup()

map_coef <- china_map %>%
  left_join(prov_coef, by = c("NAME_1" = "province_map")) %>%
  mutate(
    coef_parent_ses = ifelse(NAME_1 %in% exclude_provinces, NA, coef_parent_ses)
  )

p_coef_only <- ggplot(map_coef) +
  geom_sf(aes(fill = coef_parent_ses), color = "white", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "#DEEBF7",
    mid = "white",
    high = "#08519C",
    midpoint = 0,
    na.value = "grey90"
  ) +
  labs(
    title = "Provincial effect of parental SES on child occupational attainment",
    fill = "Coefficient"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 17),
    legend.title = element_text(face = "bold")
  )

ggsave("province_coefficient_map.png", p_coef_only, width = 11, height = 6.5, dpi = 300)

# -------------------------
# 9. Parent SES -> Arrow -> Child ISEI
# -------------------------

prov_parent <- df %>%
  filter(
    !is.na(province_map),
    !is.na(parent_ses_pca)
  ) %>%
  group_by(province_map) %>%
  summarise(
    mean_parent_ses = mean(parent_ses_pca, na.rm = TRUE),
    .groups = "drop"
  )

prov_child <- df %>%
  filter(
    !is.na(province_map),
    !is.na(child_isei)
  ) %>%
  group_by(province_map) %>%
  summarise(
    mean_child_isei = mean(child_isei, na.rm = TRUE),
    .groups = "drop"
  )

map_parent <- china_map %>%
  left_join(prov_parent, by = c("NAME_1" = "province_map")) %>%
  mutate(
    mean_parent_ses = ifelse(NAME_1 %in% exclude_provinces, NA, mean_parent_ses)
  )

map_child <- china_map %>%
  left_join(prov_child, by = c("NAME_1" = "province_map")) %>%
  mutate(
    mean_child_isei = ifelse(NAME_1 %in% exclude_provinces, NA, mean_child_isei)
  )

p_parent_left <- ggplot(map_parent) +
  geom_sf(aes(fill = mean_parent_ses), color = "white", linewidth = 0.25) +
  scale_fill_gradient(
    low = "#EFF3FF",
    high = "#08519C",
    na.value = "grey90"
  ) +
  labs(
    title = "Parental SES",
    fill = "Mean SES"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  )

p_child_right <- ggplot(map_child) +
  geom_sf(aes(fill = mean_child_isei), color = "white", linewidth = 0.25) +
  scale_fill_gradient(
    low = "#EFF3FF",
    high = "#08519C",
    na.value = "grey90"
  ) +
  labs(
    title = "Child ISEI",
    fill = "Mean ISEI"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  )

arrow_df <- tibble(
  x = seq(0.05, 0.82, length.out = 100),
  xend = seq(0.06, 0.83, length.out = 100),
  y = 0.5,
  yend = 0.5,
  col = seq(0, 1, length.out = 100)
)

p_arrow_mid <- ggplot() +
  geom_segment(
    data = arrow_df,
    aes(x = x, y = y, xend = xend, yend = yend, color = col),
    linewidth = 5.5,
    lineend = "round"
  ) +
  scale_color_gradient(low = "#C6DBEF", high = "#08519C", guide = "none") +
  annotate(
    "segment",
    x = 0.82, y = 0.5, xend = 0.98, yend = 0.5,
    linewidth = 5.5, color = "#08519C",
    arrow = arrow(length = unit(0.26, "inches"), type = "closed")
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.72,
    label = "Intergenerational transmission",
    size = 5.2, fontface = "bold", color = "#084594"
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.28,
    label = "from family background to child attainment",
    size = 4.1, color = "grey30"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void()

p_transition_only <- p_parent_left + p_arrow_mid + p_child_right +
  plot_layout(widths = c(1.1, 0.85, 1.1))

ggsave("parent_child_transition_map.png", p_transition_only, width = 16, height = 4.8, dpi = 300)
