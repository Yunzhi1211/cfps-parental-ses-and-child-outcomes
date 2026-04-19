############################################################
# Figure: Observed-to-Expected Mobility Flows
# Purpose:
# This figure visualizes intergenerational mobility flows from
# parental SES (origin) to children's occupational destination
# (EGP class).
#
# IMPORTANT INTERPRETATION:
# The figure does NOT simply show which flows are numerically large.
# Instead, each link is evaluated against an independence model.
#
# - Red links: observed flows are higher than expected
# - Blue links: observed flows are lower than expected
# - Light grey-blue links: observed flows are close to expectation
#
# In other words, the figure highlights whether specific
# origin-destination combinations are over-represented or
# under-represented after accounting for marginal distributions.
############################################################

############################################################
# 0. Clear environment and load packages
############################################################

rm(list = ls())

packages <- c("tidyverse", "circlize", "readr", "scales")
installed <- rownames(installed.packages())

for (p in packages) {
  if (!p %in% installed) install.packages(p)
}

library(tidyverse)
library(circlize)
library(readr)
library(scales)

############################################################
# 1. Read data
############################################################

file_path <- file.path("..", "3_output", "panel", "Panel_data.csv")
out_dir   <- "2_keyfindings_chord"

df_raw <- read_csv(file_path, show_col_types = FALSE)

############################################################
# 2. Basic cleaning
############################################################

df <- df_raw %>%
  mutate(
    f_edu     = as.numeric(f_edu),
    m_edu     = as.numeric(m_edu),
    f_egp     = as.numeric(f_egp),
    m_egp     = as.numeric(m_egp),
    f_party   = as.numeric(f_party),
    m_party   = as.numeric(m_party),
    child_egp = as.numeric(child_egp),
    child_edu = as.numeric(child_edu),
    f_party01 = if_else(f_party == 1, 1, 0, missing = NA_real_),
    m_party01 = if_else(m_party == 1, 1, 0, missing = NA_real_)
  )

############################################################
# 3. Map EGP classes to continuous scores
# These scores are used to build the parental SES index
############################################################

egp_score_map <- c(
  "1" = 12.8255,
  "2" = 11.5193,
  "3" = 9.7457,
  "4" = 8.5565,
  "5" = 7.0721,
  "6" = 6.9194,
  "7" = 5.3315,
  "8" = 3.9834,
  "9" = 1.3983
)

egp_labels <- c(
  "1" = "I Higher service",
  "2" = "II Lower service",
  "3" = "III Routine clerical/sales",
  "4" = "IV Small employer",
  "5" = "V Independent",
  "6" = "VI Manual foreman",
  "7" = "VII Skilled manual",
  "8" = "VIII Semi-unskilled manual",
  "9" = "IX Farmers/Farm managers"
)

df <- df %>%
  mutate(
    f_egp_score = as.numeric(egp_score_map[as.character(f_egp)]),
    m_egp_score = as.numeric(egp_score_map[as.character(m_egp)])
  )

############################################################
# 4. Construct parental SES components
# For each component, take the maximum available value across parents
############################################################

df <- df %>%
  rowwise() %>%
  mutate(
    parent_edu = ifelse(
      all(is.na(c(f_edu, m_edu))),
      NA_real_,
      max(c(f_edu, m_edu), na.rm = TRUE)
    ),
    parent_egp_score = ifelse(
      all(is.na(c(f_egp_score, m_egp_score))),
      NA_real_,
      max(c(f_egp_score, m_egp_score), na.rm = TRUE)
    ),
    parent_party = ifelse(
      all(is.na(c(f_party01, m_party01))),
      NA_real_,
      max(c(f_party01, m_party01), na.rm = TRUE)
    )
  ) %>%
  ungroup()

############################################################
# 5. Build parental SES index using PCA
# Then divide the index into terciles:
# Low / Middle / High parental SES
############################################################

pca_df <- df %>%
  filter(!is.na(parent_edu), !is.na(parent_egp_score), !is.na(parent_party)) %>%
  select(pid, parent_edu, parent_egp_score, parent_party)

pca_res <- prcomp(
  pca_df %>% select(parent_edu, parent_egp_score, parent_party),
  center = TRUE,
  scale. = TRUE
)

pca_df$parent_ses_pca <- pca_res$x[, 1]

# Ensure that higher PCA scores indicate higher SES
if (pca_res$rotation["parent_edu", 1] < 0 | pca_res$rotation["parent_egp_score", 1] < 0) {
  pca_df$parent_ses_pca <- -pca_df$parent_ses_pca
}

df <- df %>%
  left_join(pca_df %>% select(pid, parent_ses_pca), by = "pid") %>%
  mutate(
    parent_ses_group = ntile(parent_ses_pca, 3),
    parent_ses_group = factor(
      parent_ses_group,
      levels = c(1, 2, 3),
      labels = c("Low parental SES", "Middle parental SES", "High parental SES")
    ),
    child_egp_lab = recode(as.character(child_egp), !!!egp_labels),
    child_egp_lab = factor(child_egp_lab, levels = egp_labels)
  )

############################################################
# 6. Labels and color settings
############################################################

ses_short <- c(
  "Low parental SES"    = "Low SES",
  "Middle parental SES" = "Mid SES",
  "High parental SES"   = "High SES"
)

egp_short <- c(
  "I Higher service"           = "I",
  "II Lower service"           = "II",
  "III Routine clerical/sales" = "III",
  "IV Small employer"          = "IV",
  "V Independent"              = "V",
  "VI Manual foreman"          = "VI",
  "VII Skilled manual"         = "VII",
  "VIII Semi-unskilled manual" = "VIII",
  "IX Farmers/Farm managers"   = "IX"
)

egp_full <- c(
  "I Higher service"           = "Higher service",
  "II Lower service"           = "Lower service",
  "III Routine clerical/sales" = "Routine clerical/sales",
  "IV Small employer"          = "Small employer",
  "V Independent"              = "Independent",
  "VI Manual foreman"          = "Manual foreman",
  "VII Skilled manual"         = "Skilled manual",
  "VIII Semi-unskilled manual" = "Semi-unskilled manual",
  "IX Farmers/Farm managers"   = "Farmers/Farm managers"
)

sector_order <- c(
  "Low parental SES",
  "Middle parental SES",
  "High parental SES",
  "I Higher service",
  "II Lower service",
  "III Routine clerical/sales",
  "IV Small employer",
  "V Independent",
  "VI Manual foreman",
  "VII Skilled manual",
  "VIII Semi-unskilled manual",
  "IX Farmers/Farm managers"
)

grid_col <- c(
  "Low parental SES"           = "#D6EAF8",
  "Middle parental SES"        = "#7FB3D5",
  "High parental SES"          = "#1F618D",
  "I Higher service"           = "#EFF6FC",
  "II Lower service"           = "#DCECF8",
  "III Routine clerical/sales" = "#C6DDF0",
  "IV Small employer"          = "#A9CCE3",
  "V Independent"              = "#8DB9DA",
  "VI Manual foreman"          = "#73A8D0",
  "VII Skilled manual"         = "#5B96C4",
  "VIII Semi-unskilled manual" = "#3E7FB1",
  "IX Farmers/Farm managers"   = "#245A88"
)

origin_band_col <- "#1F4E79"
dest_band_col   <- "#5B8DB8"

############################################################
# 7. Create origin-destination flow table
############################################################

flow_df <- df %>%
  filter(!is.na(parent_ses_group), !is.na(child_egp_lab)) %>%
  count(parent_ses_group, child_egp_lab, name = "observed") %>%
  rename(from = parent_ses_group, to = child_egp_lab) %>%
  mutate(
    from = as.character(from),
    to   = as.character(to)
  )

used_sectors <- sector_order[sector_order %in% union(flow_df$from, flow_df$to)]

############################################################
# 8. Compute marginal totals and expected flows
#
# Expected count under independence:
# expected_ij = row_total_i * col_total_j / total_n
#
# Interpretation:
# - If observed / expected > 1, the flow is more common than expected
# - If observed / expected < 1, the flow is less common than expected
############################################################

row_totals <- flow_df %>%
  group_by(from) %>%
  summarise(row_total = sum(observed), .groups = "drop")

col_totals <- flow_df %>%
  group_by(to) %>%
  summarise(col_total = sum(observed), .groups = "drop")

total_n <- sum(flow_df$observed)

flow_df <- flow_df %>%
  left_join(row_totals, by = "from") %>%
  left_join(col_totals, by = "to") %>%
  mutate(
    expected = row_total * col_total / total_n,
    oe_ratio = observed / expected
  )

############################################################
# 9. Classify links by observed-to-expected ratio
#
# Thresholds:
# - O/E >= 1.25 : substantially over-represented
# - O/E <= 0.75 : substantially under-represented
# - otherwise   : close to expectation
#
# These thresholds are for visualization rather than formal testing.
############################################################

flow_df <- flow_df %>%
  mutate(
    residual_type = case_when(
      oe_ratio >= 1.25 ~ "Over-represented",
      oe_ratio <= 0.75 ~ "Under-represented",
      TRUE ~ "Near expected"
    ),
    oe_capped = pmin(pmax(oe_ratio, 0.50), 1.80),
    link_col = case_when(
      residual_type == "Over-represented" ~ alpha(
        "#C0392B",
        rescale(oe_capped, to = c(0.55, 0.90), from = c(1.25, 1.80))
      ),
      residual_type == "Under-represented" ~ alpha(
        "#2166AC",
        rescale(1 / oe_capped, to = c(0.50, 0.85), from = c(1 / 0.75, 1 / 0.50))
      ),
      TRUE ~ alpha("#B8CFE3", 0.28)
    ),
    link_border = case_when(
      residual_type == "Near expected" ~ NA_character_,
      TRUE ~ alpha("white", 0.60)
    ),
    link_lwd = case_when(
      residual_type == "Near expected" ~ 0.20,
      TRUE ~ 1.10
    )
  )

flow_df$link_col[is.na(flow_df$link_col)] <- alpha("#B8CFE3", 0.28)

############################################################
# 10. Additional summaries for labels and legend
############################################################

sector_totals <- c(
  setNames(row_totals$row_total, row_totals$from),
  setNames(col_totals$col_total, col_totals$to)
)

destination_pct <- col_totals %>%
  mutate(p = col_total / sum(col_total))

ses_existing <- intersect(
  c("Low parental SES", "Middle parental SES", "High parental SES"),
  used_sectors
)

egp_existing <- intersect(
  c(
    "I Higher service",
    "II Lower service",
    "III Routine clerical/sales",
    "IV Small employer",
    "V Independent",
    "VI Manual foreman",
    "VII Skilled manual",
    "VIII Semi-unskilled manual",
    "IX Farmers/Farm managers"
  ),
  used_sectors
)

############################################################
# 11. Define sector gaps
# Small gaps within groups, large gaps between origin and destination
############################################################

gap_map <- c(
  "Low parental SES"           = 1,
  "Middle parental SES"        = 1,
  "High parental SES"          = 20,
  "I Higher service"           = 1,
  "II Lower service"           = 1,
  "III Routine clerical/sales" = 1,
  "IV Small employer"          = 1,
  "V Independent"              = 1,
  "VI Manual foreman"          = 1,
  "VII Skilled manual"         = 1,
  "VIII Semi-unskilled manual" = 1,
  "IX Farmers/Farm managers"   = 20
)

gap_after <- unname(gap_map[used_sectors])

############################################################
# 12. Output figure
############################################################

out_file <- file.path(out_dir, "Figure_ObservedExpected_Mobility_Chord.png")

png(
  filename = out_file,
  width = 9200,
  height = 5200,
  res = 200
)

layout(
  matrix(c(1, 2), nrow = 1),
  widths = c(3.0, 1.4)
)

############################################################
# LEFT PANEL: Chord diagram
############################################################

par(
  mar = c(1, 1, 4, 0),
  xpd = NA
)

circos.clear()
circos.par(
  start.degree = 260,
  gap.after = gap_after,
  track.margin = c(0.004, 0.004),
  cell.padding = c(0, 0, 0, 0),
  canvas.xlim = c(-1.05, 1.05),
  canvas.ylim = c(-1.00, 1.00)
)

chordDiagram(
  x = flow_df[, c("from", "to", "observed")],
  order = used_sectors,
  grid.col = grid_col[used_sectors],
  col = flow_df$link_col,
  transparency = 0,
  link.border = flow_df$link_border,
  link.lwd = flow_df$link_lwd,
  annotationTrack = "grid",
  preAllocateTracks = list(
    list(track.height = 0.11),
    list(track.height = 0.16)
  ),
  directional = 0,
  link.sort = TRUE,
  link.decreasing = FALSE,
  link.largest.ontop = TRUE
)

############################################################
# Track 1: Outer band for origin / destination grouping
############################################################

circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    sector_name <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    
    ring_col <- if (sector_name %in% names(ses_short)) {
      origin_band_col
    } else {
      dest_band_col
    }
    
    circos.rect(
      xleft = xlim[1],
      ybottom = ylim[1],
      xright = xlim[2],
      ytop = ylim[2],
      col = ring_col,
      border = NA
    )
  }
)

if (length(ses_existing) > 0) {
  highlight.sector(
    sector.index = ses_existing,
    track.index = 1,
    col = adjustcolor(origin_band_col, alpha.f = 0.98),
    border = NA,
    text = "Origin",
    cex = 5.50,
    text.col = "white",
    niceFacing = FALSE,
    facing = "bending.inside",
    padding = c(0.005, 0.005, 0.005, 0.005)
  )
}

if (length(egp_existing) > 0) {
  highlight.sector(
    sector.index = egp_existing,
    track.index = 1,
    col = adjustcolor(dest_band_col, alpha.f = 0.98),
    border = NA,
    text = "Destination",
    cex = 5.00,
    text.col = "white",
    niceFacing = FALSE,
    facing = "bending.inside",
    padding = c(0.005, 0.005, 0.005, 0.005)
  )
}

############################################################
# Track 2: Inner labels
# - SES sectors show short label + sample size
# - EGP sectors show Roman numerals
############################################################

circos.trackPlotRegion(
  track.index = 2,
  bg.border = NA,
  panel.fun = function(x, y) {
    sector_name <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    
    ring_col <- if (sector_name %in% names(ses_short)) {
      adjustcolor(origin_band_col, alpha.f = 0.92)
    } else {
      adjustcolor(dest_band_col, alpha.f = 0.92)
    }
    
    circos.rect(
      xleft = xlim[1],
      ybottom = ylim[1],
      xright = xlim[2],
      ytop = ylim[2],
      col = ring_col,
      border = NA
    )
    
    if (sector_name %in% names(ses_short)) {
      circos.text(
        x = mean(xlim),
        y = mean(ylim),
        labels = paste0(
          ses_short[sector_name],
          "\n(n=", comma(sector_totals[sector_name]), ")"
        ),
        facing = "bending.inside",
        niceFacing = FALSE,
        adj = c(0.5, 0.5),
        cex = 3.60,
        col = "white",
        font = 2
      )
    } else {
      circos.text(
        x = mean(xlim),
        y = mean(ylim),
        labels = egp_short[sector_name],
        facing = "clockwise",
        niceFacing = TRUE,
        adj = c(0.5, 0.5),
        cex = 4.00,
        col = "white",
        font = 2
      )
    }
  }
)

title(
  "Observed-to-Expected Mobility Flows by Parental SES",
  cex.main = 4.50,
  font.main = 2,
  line = 2.0
)

circos.clear()

############################################################
# RIGHT PANEL: Legend and interpretation guide
############################################################

par(
  mar = c(2, 0.5, 4, 2),
  xpd = NA
)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

rect(
  xleft = 0.05,
  ybottom = 0.02,
  xright = 0.97,
  ytop = 0.92,
  border = "#1F4E79",
  lwd = 2.4
)

text(
  x = 0.51,
  y = 0.88,
  labels = "EGP destination classes",
  adj = c(0.5, 0.5),
  cex = 4.80,
  font = 2,
  col = "#1F4E79"
)

legend_order <- c(
  "I Higher service",
  "II Lower service",
  "III Routine clerical/sales",
  "IV Small employer",
  "V Independent",
  "VI Manual foreman",
  "VII Skilled manual",
  "VIII Semi-unskilled manual",
  "IX Farmers/Farm managers"
)

legend_pct_map <- setNames(destination_pct$p, destination_pct$to)
legend_y <- seq(0.79, 0.24, length.out = length(legend_order))

for (i in seq_along(legend_order)) {
  k <- legend_order[i]
  pct_val <- if (k %in% names(legend_pct_map)) {
    percent(legend_pct_map[k], accuracy = 0.1)
  } else {
    "0.0%"
  }
  
  text(
    x = 0.12,
    y = legend_y[i],
    labels = egp_short[k],
    adj = c(0, 0.5),
    cex = 4.20,
    col = "#222222"
  )
  
  text(
    x = 0.22,
    y = legend_y[i],
    labels = egp_full[k],
    adj = c(0, 0.5),
    cex = 4.00,
    col = "#222222"
  )
  
  text(
    x = 0.94,
    y = legend_y[i],
    labels = pct_val,
    adj = c(1, 0.5),
    cex = 4.00,
    col = "#222222"
  )
}

# Link color legend
segments(0.25, 0.185, 0.36, 0.185, col = "#C0392B", lwd = 10)
text(0.62, 0.185, "Observed > expected", adj = c(0.5, 0.5), cex = 3.50, col = "#444444")

segments(0.25, 0.130, 0.36, 0.130, col = "#2166AC", lwd = 10)
text(0.62, 0.130, "Observed < expected", adj = c(0.5, 0.5), cex = 3.50, col = "#444444")

segments(0.25, 0.075, 0.36, 0.075, col = "#B8CFE3", lwd = 10)
text(0.62, 0.075, "Close to expected", adj = c(0.5, 0.5), cex = 3.50, col = "#444444")


dev.off()
