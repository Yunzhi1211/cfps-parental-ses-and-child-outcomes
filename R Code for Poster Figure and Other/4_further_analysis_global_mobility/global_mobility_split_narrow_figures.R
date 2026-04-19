############################################################
# Narrow poster figures:
# 1. Regular world map
# 2. Tall cohort trend chart
############################################################

rm(list = ls())

packages <- c(
  "tidyverse", "sf", "rnaturalearth", "rnaturalearthdata",
  "scales", "grid"
)
installed <- rownames(installed.packages())
for (p in packages) {
  if (!p %in% installed) install.packages(p, repos = "https://cran.rstudio.com/")
}

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(grid)

base_dir <- "4_further_analysis_global_mobility"

find_gdim_file <- function() {
  candidates <- c(
    file.path(base_dir, "GDIM_2023_03.csv"),
    "GDIM_2023_03.csv",
    file.path("..", "GDIM_2023_03.csv")
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop(
      paste(
        "GDIM_2023_03.csv was not found.",
        "Please place it in",
        shQuote(base_dir),
        "or update `find_gdim_file()` in this script."
      )
    )
  }
  hit
}

gdim <- readr::read_csv(find_gdim_file(), show_col_types = FALSE)

map_df <- gdim %>%
  filter(cohort == 1980, parent == "avg", child == "all") %>%
  transmute(
    country,
    code,
    beta = as.numeric(BETA)
  ) %>%
  filter(!is.na(beta))

trend_df <- gdim %>%
  filter(parent == "avg", child == "all") %>%
  transmute(
    code,
    region,
    cohort = as.numeric(cohort),
    beta = as.numeric(BETA)
  ) %>%
  filter(!is.na(beta), !is.na(cohort))

china_beta <- map_df %>%
  filter(code == "CHN") %>%
  pull(beta)

china_trend <- trend_df %>%
  filter(code == "CHN") %>%
  transmute(cohort, beta, group = "China")

global_trend <- trend_df %>%
  group_by(cohort) %>%
  summarise(beta = mean(beta, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = "Global")

eap_trend <- trend_df %>%
  filter(region == "East Asia & Pacific") %>%
  group_by(cohort) %>%
  summarise(beta = mean(beta, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = "East Asia")

trend_small <- bind_rows(china_trend, global_trend, eap_trend) %>%
  filter(cohort %in% c(1940, 1950, 1960, 1970, 1980)) %>%
  mutate(group = factor(group, levels = c("China", "East Asia", "Global")))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, geometry)

world_plot <- world %>%
  left_join(map_df, by = c("iso_a3" = "code"))

china_sf <- world %>%
  filter(iso_a3 == "CHN")

china_label_df <- tibble(
  x = 140,
  y = -12,
  label = paste0("China\nbeta=", sprintf("%.2f", china_beta))
)

############################################################
# Figure 1: regular world map
############################################################

p_map <- ggplot(world_plot) +
  geom_sf(aes(fill = beta), color = "white", linewidth = 0.12) +
  geom_sf(
    data = china_sf,
    fill = NA,
    color = "#D94841",
    linewidth = 0.85
  ) +
  annotate(
    "curve",
    x = 116, xend = 138,
    y = 24, yend = -8,
    curvature = -0.18,
    color = "#D94841",
    linewidth = 0.55,
    arrow = arrow(length = unit(0.12, "cm"))
  ) +
  geom_label(
    data = china_label_df,
    aes(x = x, y = y, label = label),
    fill = alpha("white", 0.96),
    color = "#D94841",
    fontface = "bold",
    size = 2.9,
    hjust = 0,
    linewidth = 0.15,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colours = c("#EEF4FB", "#BDD7EE", "#6FA8DC", "#2E75B6", "#173F6B"),
    na.value = "#F3F5F7",
    name = expression(beta),
    limits = c(0.10, 1.10),
    oob = squish,
    breaks = c(0.2, 0.4, 0.6, 0.8, 1.0),
    labels = label_number(accuracy = 0.1)
  ) +
  coord_sf(
    xlim = c(-170, 180),
    ylim = c(-58, 85),
    expand = FALSE
  ) +
  labs(
    title = "Global Educational Mobility Benchmark",
    subtitle = "World Bank GDIM, 1980 cohort",
    caption = "Higher beta = stronger intergenerational persistence"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, color = "#163A63", hjust = 0),
    plot.subtitle = element_text(size = 8.5, color = "grey35", hjust = 0),
    plot.caption = element_text(size = 7, color = "grey45", hjust = 0),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 8, face = "bold", color = "#163A63"),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.9, "cm"),
    plot.margin = margin(4, 4, 2, 4)
  )

############################################################
# Figure 2: tall and narrow cohort chart
############################################################

p_cohort <- ggplot(trend_small, aes(x = cohort, y = beta, color = group, group = group)) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 2.1) +
  scale_color_manual(
    values = c(
      "China" = "#D94841",
      "East Asia" = "#4F87C2",
      "Global" = "#163A63"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = c(1940, 1950, 1960, 1970, 1980),
    labels = c("1940s\nbirths", "1950s\nbirths", "1960s\nbirths", "1970s\nbirths", "1980s\nbirths"),
    expand = c(0.04, 0.04)
  ) +
  scale_y_continuous(
    limits = c(0.42, 0.70),
    breaks = c(0.45, 0.55, 0.65),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Cohort Trend",
    x = NULL,
    y = expression(beta)
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#163A63", hjust = 0),
    axis.title.y = element_text(size = 11, color = "#163A63", angle = 0, vjust = 0.5),
    axis.text = element_text(size = 9.5, color = "#394150"),
    legend.position = "top",
    legend.text = element_text(size = 9.5),
    legend.key.width = unit(0.95, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(4, 4, 4, 4)
  )

############################################################
# Save
############################################################

map_file <- file.path(base_dir, "Figure_Global_Mobility_Map_Regular.png")
cohort_file <- file.path(base_dir, "Figure_Global_Mobility_Cohort_Tall.png")

ggsave(map_file, p_map, width = 6.2, height = 4.8, dpi = 420, bg = "white")
ggsave(cohort_file, p_cohort, width = 3.4, height = 6.6, dpi = 420, bg = "white")

cat("\n=== Done ===\n")
cat("Map:", map_file, "\n")
cat("Cohort:", cohort_file, "\n")
