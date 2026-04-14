library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Read data
df <- read_dta(file.path("..", "3_output", "RDataUse", "mapdata_for_r.dta"))

# Province code mapping table
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

# Merge province names
df <- df %>%
  left_join(prov_map, by = "child_province")

# Target years
years_keep <- c(2010, 2012, 2014, 2016, 2018, 2020, 2022)

# Prepare data
plot_df <- df %>%
  filter(year %in% years_keep) %>%
  select(child_province, prov_name, year, mean_isei) %>%
  complete(child_province, year = years_keep) %>%
  group_by(child_province) %>%
  fill(prov_name, .direction = "downup") %>%
  ungroup() %>%
  group_by(child_province, prov_name) %>%
  mutate(overall = mean(mean_isei, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.nan(overall)) %>%
  mutate(
    prov_name = reorder(prov_name, overall),
    year = factor(year, levels = years_keep, labels = c("10","12","14","16","18","20","22"))
  )

# Plot
p <- ggplot(plot_df, aes(x = year, y = prov_name, fill = mean_isei)) +
  geom_tile(width = 0.7, height = 0.9) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    na.value = "white",
    name = "Mean ISEI"
  ) +
  labs(
    title = "Provincial Mean Child ISEI, 2010–2022",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  )

print(p)

# Export figure
ggsave(
  file.path("0_unused_heatmaps", "heatmap_mean_childisei_r.png"),
  plot = p,
  width = 5.2,
  height = 8.8,
  dpi = 400
)
