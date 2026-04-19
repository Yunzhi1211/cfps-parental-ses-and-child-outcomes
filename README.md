[English](README.md) | [简体中文](README_zh.md)

# Intergenerational Mobility in China: A CFPS Panel Study of Parental SES and Child Occupational Outcomes

---

## Project Overview

This repository contains comprehensive analytical code and documentation for a longitudinal study examining **intergenerational socioeconomic mobility** in China. Using the China Family Panel Studies (CFPS) dataset spanning 2010–2022, we investigate how parental socioeconomic resources (SES) shape children's occupational achievement, with particular attention to temporal trends, geographic variation, and heterogeneous effects across population subgroups.

---

## Research Focus
- Quantifying the elasticity of intergenerational SES transmission across seven waves
- Identifying mechanisms through which parental SES operates (occupation, education, political capital)
- Assessing geographic inequality in mobility processes
- Testing robustness across occupational classification systems and modeling specifications

---

## Data Processing Highlights & Technical Innovations

### 1. **Intelligent Multi-Wave Panel Construction with Rigorous Validation**

- Systematically integrated CFPS data from **7 survey waves** (2010, 2012, 2014, 2016, 2018, 2020, 2022)
- **Automatic negative-code conversion**: Implemented sweeping logic converting all negative values to missing (excluding ID/time variables)
- **Wave-year verification**: Auto-generates wave and year identifiers with validation checks
- **Uniqueness enforcement**: Applied `isid pid year` validation to enforce panel uniqueness; script aborts with diagnostic output if violations detected
- **Intelligent missing value diagnosis**: Commented code documents past discoveries (e.g., father/mother income had >90% missing from 2016 onward; substituted family_income with ~10% missing rate)
- **Output compression**: Automatically applies Stata `compress` to reduce file size while maintaining precision

---

### 2. **Multi-Component SES Index via Year-Specific PCA**

- Constructs composite parental SES combining **3 conceptually distinct domains**:
  - **Occupational prestige**: Father & mother ISEI (International Socioeconomic Index)
  - **Human capital**: Father & mother years of education
  - **Political capital**: CPC (Communist Party) membership indicator for either parent
  
- **Year-specific rather than pooled PCA**: Each survey year receives its own PCA model
  - *Rationale*: Captures temporal shifts in how these dimensions weight together
  - *Implementation*: Loop over `levelsof year` with sample-size threshold (N≥20) per year
  - *Scoring*: Year-specific scores saved to panel-level variable for regression analysis
  - 
---

### 3. **Geographic Stratification with Sampling Bias Correction**

- **Province-level aggregation**: Computed means and counts by child's province of residence
- **Sample size filtering**: Excluded provinces with N<50 to ensure statistical stability
- **Representativeness-based exclusion**: Explicitly removed Tibet and Xinjiang due to:
  - Severe urban sampling bias in CFPS (insufficient herder/farmer population coverage)
  - Small sample sizes creating misleading geographic patterns
  - *Documented rationale*: Without exclusion, Tibet appeared as "darkest blue" (highest ISEI) due to sampling artifact

- **Geodetic matching**: Created HASC_1 codes (Hierarchical Administrative Subdivision Codes) linking 31 provinces to shapefile geometry for choropleth mapping
- **Consistent color scaling**: All 7 yearly maps use global 6-quantile breaks computed from pooled provincial means


---

### 4. **Comprehensive Multi-Dimensional Robustness Framework**

**Specification Testing:**

| Dimension | Variants | Purpose |
|-----------|----------|---------|
| **Fixed Effects** | Province only; Province + Year | Temporal confounding control |
| **Outlier Treatment** | Original; Winsorized (1%-99%) | Extreme value sensitivity |
| **Subsamples** | Urban/Rural; Male/Female | Heterogeneous effects |
| **Outcome Measures** | ISEI; SIOPS; EGP category | Measurement robustness |
| **Controls** | Base; +Log family income | Channel specification |

- **Unified variable naming**: Models swap `y_core` and `x_core` definitions, allowing single summary table to compare all specifications
- **Coefficient extraction pipeline**: `postfile` records year-specific estimates with standard errors for trend visualization
- **Clustered standard errors**: Province-level clustering throughout to account for geographic dependence

**Outputs:**
- Individual robustness tables (6_1 through 6_7)
- Single polished summary table (6_8) with explicit parameter grid
- Coefficient plots showing effect size consistency

---

### 5. **Observed-to-Expected Mobility Analysis via Chord Diagrams (R)**

**Visualization:**

The R chord diagram implementation computes **observed-to-expected (O/E) ratios** under an independence model:

```
Expected(ij) = (Row Total_i × Col Total_j) / Total_n
O/E Ratio = Observed(ij) / Expected(ij)
```

**Interpretation Rules:**
- **Red links** (O/E ≥ 1.25): Over-represented origin-destination flows (mobility premium)
- **Blue links** (O/E ≤ 0.75): Under-represented flows (mobility barrier)
- **Light grey links** (0.75 < O/E < 1.25): Close to independence expectation

**Technical Features:**
- **Tercile-based parental SES grouping**: Divides PCA score into Low/Middle/High (ntile=3)
- **EGP mapping to continuous scores**: Maps 9 EGP classes to SIOPS-style scores for PCA indexing
- **Rowwise parent aggregation**: Takes maximum education/occupational prestige/party status across parents
- **Multi-track circular layout**: Outer ring labels sectors; inner ring shows counts and Roman numerals
- **Directional rendering**: Gap specifications create visual separation between origin and destination domains

---

### 6. **Year-by-Year Regression with Coefficient Tracking**

**Generates Trend Visualizations:**
- **Scatter + CI + trend line**: Year-specific point estimates with 95% confidence intervals
- **Overlay fitted lines**: Seven regression lines with year-specific colors showing SES elasticity evolution
- **Faceted panel plots**: 2×4 grid showing all year-specific scatter plots with fitted lines

---

### 7. **Sample Flow Documentation with R Funnel Chart**

The R script visualizes sample attrition across selection steps with:
- **Horizontal bar chart**: Each step shows count and % of initial sample
- **Auto-wrapped labels**: Long text wraps at 22 characters (responsive)
- **Blue gradient palette**: Visual hierarchy from light to dark
- **Right-side labels**: Counts + percentages displayed outside bars

---

## Repository Structure

### `1_do/` – Stata Do-Files (Master Scripts)

| Script | Purpose | Key Operations |
|--------|---------|-----------------|
| **master_1_clean_data.do** | Wave-by-wave cleaning | Read 7 waves, recode negatives, enforce validity |
| **master_2_panel_build.do** | Panel integration | Append years, verify uniqueness, compress |
| **master_3_descriptive_and_maps.do** | Exploratory analysis | Province maps, descriptive tables, boxplots |
| **master_4_base_regression.do** | Main analysis | Year-by-year PCA models, trend plots |
| **master_5_quantile_regression.do** | Distributional effects | Conditional quantile regressions |
| **master_6_robustness_check.do** | Sensitivity tests | 10 robustness specifications + summary table |
| **master_7_mechanism_check.do** | Mediator analysis | Direct/indirect effects decomposition |
| **master_8_three_generation.do** | Intergenerational dynamics | Grandparent-parent-child pathways |

**Wave-Specific Files:** `10.do`, `12.do`, `14.do`, `16.do`, `18.do`, `20.do`, `22.do` (cycle-specific cleaning logic for CFPS waves 2010–2022)

### `R Code for Poster Figure and Other/` – R Visualization Scripts

| Directory | Key Script | Output Type |
|-----------|-----------|-------------|
| **1_research_question_funnel_chart/** | `1_research_question_funnel_chart.R` | Sample flow funnel |
| **2_keyfindings_chord/** | `2_keyfindings_chord.R` | Observed-to-expected chord |
| **2_keyfindings_mapcoef/** | `2_keyfindings_mapcoef.R` | Geographic coefficient variation |
| **2_keyfindings_yearcoef/** | `2_keyfindings_yearcoef.R` | Temporal trend coefficients |
| **3_mechanism_sun/** | `3_mechanism_sun.R` | Mechanism decomposition |
| **4_further_analysis_three_generation/** | `4_further_analysis_three_generation.R` | Grandparent effects |
| **4_further_analysis_global_mobility/** | `5_global_mobility_map.R` / `10_global_mobility_split_narrow_figures.R` | Global mobility benchmark figures |
| **5_conclusion_bibliometric_analysis/** | `5_conclusion_bibliometric_analysis.R` | Bibliometric analysis |

---

## Key Methodological Features

### Temporal Design
- **Panel structure:** Unbalanced panel with pid-year observations (2010–2022)
- **Year-specific PCA:** Each wave's parental SES index captures period-specific measurement structure
- **Trend extraction:** Year-by-year coefficients enable visual inspection of mobility elasticity evolution
- **Overlaid regressions:** Seven fitted lines show consistency or secular change in SES transmission

### Occupational Classifications
Multiple outcome measures ensure findings are not artifact of single classification:
- **ISEI** (International Socioeconomic Index): Continuous scale, 16–90 range
- **SIOPS** (Standard International Occupational Prestige Scale): Alternative prestige measure
- **EGP** (Erikson-Goldthorpe-Portocarero): Nine-class categorical system for class mobility

### Sample Strategy & Representativeness
- **Province-level variance:** Includes 31 provinces (excluding Tibet, Xinjiang for documented reasons)
- **Urban-rural stratification:** Separate models test whether processes differ by residency
- **Gender heterogeneity:** Male/female subsamples reveal gendered mobility patterns
- **Missing data transparency:** Commented code documents historical discoveries re: income missingness

### Geographic Analysis
- **Choropleth mapping:** Annual provincial-level maps showing occupational outcomes
- **Consistent color scaling:** Global 6-quantile breaks ensure comparability across years
- **Faceted layout:** All 7 years in single 2×4 grid with shared legend
- **Boundary artifacts:** Shapefiles use GADM 4.1 with province-level boundaries

---

## Data Requirements & Reproducibility

### Raw Data (Not Included)

Raw CFPS datasets **not included** due to:
- **File size**: Each cycle ~100–500 MB; 7 waves exceeds GitHub limits
- **Access restrictions**: CFPS data subject to institutional access agreements; users must register with CFPS Dataset

### To Reproduce

1. **Obtain CFPS data**: Register at CFPS(https://www.isss.pku.edu.cn/cfps/index.htm)
2. **Organize folder structure:**
   ```
   CFPS Project/
   ├── 0_raw/              (raw data + shapefile)
   ├── 1_do/               (Stata scripts)
   ├── 2_temp/             (temporary intermediate files)
   ├── 3_output/           (final tables & figures)
   ├── 4_log/              (Stata logs)
   └── 5_doc/              (documentation)
   ```
3. **Update paths**: Edit line 7 of `master_1_clean_data.do`:
   ```stata
   global PROJ "C:\Your\Local\Path\To\CFPS Project"
   ```
4. **Execute pipeline**: Run `master_1_clean_data.do` → `master_2_panel_build.do` → `master_3_descriptive_and_maps.do` → `master_4_base_regression.do` → robustness checks → `R Code for Poster Figure and Other/*.R`

### Software Requirements

**Stata:**
- Version: 16 or higher (for `pca`, `winsor2`, `estout`, `coefplot` packages)
- Key packages: `estout`, `coefplot`, `spmap`, `shp2dta`, `winsor2`
- Install via: `ssc install <package_name>`

**R:**
- Version: 4.0 or higher
- Key packages: `tidyverse`, `ggplot2`, `circlize`, `scales`, `readr`
- Install via: `install.packages(c("tidyverse", "circlize", "scales"))`

---

## Data Processing Highlights – Executive Summary

| Innovation | Advantage | Evidence in Code |
|-----------|-----------|------------------|
| **Year-specific PCA** | Avoids temporal pooling bias | `pca` loop in master_4, master_3, R scripts |
| **Automatic data quality checks** | Catches panel imbalances early | `isid pid year` validation in master_2 |
| **Geographic bias correction** | Transparent exclusion logic | Tibet/Xinjiang explicit removal documented in master_3 |
| **Multi-specification robustness** | Single summary table prevents errors | Unified `y_core`, `x_core` naming in master_6 |
| **O/E mobility analysis** | Reveals structural vs. compositional patterns | Chord diagram R/O calculation in 2_keyfindings_chord.R |
| **Comprehensive postfile extraction** | Enables trend visualization without re-fitting | Coefficient postfile in master_4 & master_6 |

---

## Current Status

**Project State:** Active, under revision

- ✅ Data cleaning pipeline: Complete
- ✅ Panel construction: Validated
- ✅ Descriptive statistics: Tabulated
- ✅ Base regressions: Year-by-year fitted
- ✅ Robustness checks: 10 specifications tested
- 🔄 Mechanism analysis: Under development
- 🔄 Paper draft: Under revision
- 🔄 Poster presentation: Under revision

**Note:** Some scripts, file names, and folder structures may continue to evolve as the project develops. Log files document all changes and issues encountered.

---

## Team & Contact

**Project Team:**
- **Haiyun Shi** — u3661501@connect.hku.hk
- **Yaru Yang** — u3661510@connect.hku.hk
- **Yunzhi Jiang** — jiangyunzhi@connect.hku.hk
- **Chao Huang** — u3665987@connect.hku.hk

**For Questions or Comments:**
- **Code & reproducibility**: Yunzhi Jiang — Yunzhi.j@foxmail.com
- **Data access**: Contact CFPS Dataset or isss.cfps@pku.edu.cn

---


**Last Updated:** April 2026  
**Repository:** https://github.com/Yunzhi1211/cfps-parental-ses-and-child-outcomes
