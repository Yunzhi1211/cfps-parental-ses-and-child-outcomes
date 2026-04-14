# Poster Figure Scripts (Reorganized)

This folder is reorganized by poster section order for easier team reproduction.

## Run Assumption

- Open and run from this R project: `R Code for Poster Figure and Other.Rproj`
- Working directory should be `R Code for Poster Figure and Other`
- External data paths are set relative to this folder using `..` (for example `../3_output/panel/Panel_data.csv`)
- Output files are saved into each script's own subfolder

## Folder Order

### `0_unused_*` (not currently used on poster)
- `0_unused_distribution_pca_quantile/0_unused_distribution_pca_quantile.R`
- `0_unused_heatmaps/0_unused_heatmaps.R`
- `0_unused_quantile_regression/0_unused_quantile_errorbar.R`
- `0_unused_quantile_regression/0_unused_quantile_errorband.R`
- `0_unused_quantile_regression/0_unused_quantile_scatter_by_year.R`

### `1_research_question_*`
- `1_research_question_funnel_chart/1_research_question_funnel_chart.R`

### `2_keyfindings_*`
- `2_keyfindings_chord/2_keyfindings_chord.R`
- `2_keyfindings_mapcoef/2_keyfindings_mapcoef.R`
- `2_keyfindings_yearcoef/2_keyfindings_yearcoef.R`

### `3_mechanism_*`
- `3_mechanism_robustness/3_mechanism_robustness.R`
- `3_mechanism_sun/3_mechanism_sun.R`
- `3_mechanism_three_generation/3_mechanism_three_generation.R`

### `4_conclusion_*`
- `4_conclusion_bibliometric_analysis/4_conclusion_bibliometric_analysis.R`

## Main Input Files Used by Active Poster Scripts

- `3_output/panel/Panel_data.csv`
- `0_raw/gadm41_CHN_1.shp` (and associated shapefile sidecar files)
- `1_research_question_funnel_chart/sampleflow_total.csv`
- `3_mechanism_sun/7_1_main_mechanism_results.csv`
- `0_raw/bibliometric/savedrecs.txt` (bibliometric script; fallback is local `savedrecs.txt`)

## Suggested Reproduction Order (active poster)

1. `1_research_question_funnel_chart/1_research_question_funnel_chart.R`
2. `2_keyfindings_chord/2_keyfindings_chord.R`
3. `2_keyfindings_mapcoef/2_keyfindings_mapcoef.R`
4. `2_keyfindings_yearcoef/2_keyfindings_yearcoef.R`
5. `3_mechanism_robustness/3_mechanism_robustness.R`
6. `3_mechanism_sun/3_mechanism_sun.R`
7. `3_mechanism_three_generation/3_mechanism_three_generation.R`
8. `4_conclusion_bibliometric_analysis/4_conclusion_bibliometric_analysis.R`
