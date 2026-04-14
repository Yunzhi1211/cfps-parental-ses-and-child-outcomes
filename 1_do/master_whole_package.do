clear all
set more off

****************************************************************************************************************************
* Project root directory: (Need to change this line to reproduce the analysis, check for missing errors, or conduct further analysis.)
****************************************************************************************************************************
global PROJ "D:\一个文件夹\学习\学习\hku\8001\CFPS Project"

global RAW   "${PROJ}\0_raw"
global DO    "${PROJ}\1_do"
global TEMP  "${PROJ}\2_temp"
global OUT   "${PROJ}\3_output"
global YOUT  "${OUT}\yearly"
global POUT  "${OUT}\panel"
global LOG   "${PROJ}\4_log"
global TAB   "${OUT}\panel"

cap mkdir "${OUT}"
cap mkdir "${YOUT}"
cap mkdir "${POUT}"
cap mkdir "${LOG}"
cap mkdir "${TAB}"

cap log close
local t = subinstr("`c(current_time)'", ":", "_", .)
log using "${LOG}\master_`c(current_date)'_`t'.log", replace text

*----------------------------------*
* Load sample-flow helper programs first
*----------------------------------*
do "${DO}\master_0_sampleflow_append.do"

*----------------------------------*
* Build yearly files
*----------------------------------*
do "${DO}\10.do"
do "${DO}\12.do"
do "${DO}\14.do"
do "${DO}\16.do"
do "${DO}\18.do"
do "${DO}\20.do"
do "${DO}\22.do"

flowexport

*----------------------------------*
* Export sample-flow summary / graph
*----------------------------------*
do "${DO}\master_0_graph_produce.do"
do "${DO}\master_2_panel_build.do"
do "${DO}\master_3_descriptive_and_maps.do"
do "${DO}\master_4_base_regression.do"
do "${DO}\master_5_quantile_regression.do"
do "${DO}\master_6_robustness_check.do"
do "${DO}\master_7_mechanism_check.do"
do "${DO}\master_8_three_generation.do"

log close
