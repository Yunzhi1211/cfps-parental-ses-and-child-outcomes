# =========================
# 0. Data Source and Download Information
# =========================
# Database: Web of Science Core Collection
# Search date: 2026-03-24
# Search query: TS = ("family background" OR "social origin" OR "parental socioeconomic status" OR "parental SES")
# AND ("occupational mobility" OR "occupational attainment" OR "occupational status" OR "status attainment" OR "upward mobility" OR "intergenerational mobility")
# Time span: 2000-2025
# Document type: Article
# Language: English
# Export format: Plain Text
# Exported content: Full Record and Cited References
# File name: savedrecs.txt

library(bibliometrix)

out_dir <- "4_conclusion_bibliometric_analysis"
input_file <- file.path("..", "0_raw", "bibliometric", "savedrecs.txt")
if (!file.exists(input_file)) {
  input_file <- "savedrecs.txt"
}

D <- convert2df(
  file = input_file,
  dbsource = "wos",
  format = "plaintext"
)

M <- metaTagExtraction(D, Field = "AU_CO", sep = ";")
kw_field <- if ("DE" %in% names(M) && sum(!is.na(M$DE) & M$DE != "") >= 80) "DE" else "ID"

# =========================
# 1. Keyword Co-occurrence Network
# Save as PNG
# =========================
png(file.path(out_dir, "01_keyword_cooccurrence_network.png"),
    width = 2200, height = 1600, res = 200)

NetMatrix_kw <- biblioNetwork(
  M,
  analysis = "co-occurrences",
  network  = "keywords",
  sep      = ";"
)

networkPlot(
  NetMatrix_kw,
  normalize = "association",
  weighted = TRUE,
  n = 22,                      # 30 was too crowded; reduced to 22
  type = "fruchterman",
  size = TRUE,
  remove.multiple = FALSE,
  labelsize = 0.9,             # Slightly smaller labels to reduce overlap
  edgesize = 1,
  Title = "Keyword Co-occurrence Network"
)

dev.off()


# =========================
# 2. Country Collaboration Network
# Save as PNG
# =========================
png(file.path(out_dir, "02_country_collaboration_network.png"),
    width = 2200, height = 1600, res = 200)

NetMatrix_country <- biblioNetwork(
  M,
  analysis = "collaboration",
  network  = "countries",
  sep      = ";"
)

networkPlot(
  NetMatrix_country,
  n = 20,                     # Avoid displaying too many countries
  type = "circle",            # More balanced than fruchterman
  size = TRUE,
  remove.isolates = TRUE,
  labelsize = 1.2,            # Increase font size for better readability
  edgesize = 1,
  Title = "Country Collaboration Network"
)

dev.off()


# =========================
# 3. Thematic Map
# Save as PNG
# =========================
png(file.path(out_dir, "03_thematic_map.png"),
    width = 1800, height = 1300, res = 150)

thematicMap(
  M,
  field = kw_field,
  n = 50,
  minfreq = 2,
  stemming = FALSE,
  size = 0.7,
  repel = TRUE
)

dev.off()

# =========================
# 4. Top 10 Keywords
# Save as PNG
# Remove NA / na / empty values
# =========================
png(file.path(out_dir, "04_top_10_keywords.png"),
    width = 1600, height = 1200, res = 150)

kw_use <- if ("DE" %in% names(M) && sum(!is.na(M$DE) & M$DE != "") > 50) "DE" else "ID"

kw_list <- unlist(strsplit(paste(M[[kw_use]], collapse = ";"), ";"))
kw_list <- trimws(tolower(kw_list))

# Remove empty values, actual NA, and the string "na"
kw_list <- kw_list[!is.na(kw_list)]
kw_list <- kw_list[kw_list != ""]
kw_list <- kw_list[kw_list != "na"]
kw_list <- kw_list[kw_list != "n/a"]
kw_list <- kw_list[kw_list != "null"]

kw_tab <- sort(table(kw_list), decreasing = TRUE)
top_kw <- head(kw_tab, 10)

par(mar = c(5, 14, 4, 2))
barplot(
  rev(as.numeric(top_kw)),
  horiz = TRUE,
  names.arg = rev(names(top_kw)),
  col = "steelblue",
  las = 1,
  xlab = "Frequency",
  main = "Top 10 Keywords"
)

dev.off()

cat("Done! PNG files saved in:", out_dir, "\n")
