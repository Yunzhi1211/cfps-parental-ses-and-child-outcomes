clear all
set more off

****************************************************************************************************************************
* Project root directory: (only this line needs to be changed to reproduce/check missing errors/run follow-up analyses)
****************************************************************************************************************************
global PROJ "D:\一个文件夹\学习\学习\hku\8001\CFPS Project"

global RAW   "${PROJ}\0_raw"
global DO    "${PROJ}\1_do"
global TEMP  "${PROJ}\2_temp"
global OUT   "${PROJ}\3_output"
global YOUT  "${OUT}\yearly"
global POUT  "${OUT}\panel"
global LOG   "${PROJ}\4_log"
global DOC   "${PROJ}\5_doc"

cap mkdir "${OUT}"
cap mkdir "${YOUT}"
cap mkdir "${POUT}"
cap mkdir "${LOG}"

adopath ++ "${RAW}"

cap log close
local t = subinstr("`c(current_time)'", ":", "_", .)
log using "${LOG}\master_`c(current_date)'_`t'.log", replace text

do "${DO}\10.do"
do "${DO}\12.do"
do "${DO}\14.do"
do "${DO}\16.do"
do "${DO}\18.do"
do "${DO}\20.do"
do "${DO}\22.do"

log close
