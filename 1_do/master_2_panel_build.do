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

local waves "10 12 14 16 18 20 22"

foreach w of local waves {
    local f "${YOUT}`c(dirsep)'`w'_data.dta"
    di "Loading: `f'"
    confirm file "`f'"

    if "`w'"=="10" use "`f'", clear
    else          append using "`f'"

    * Mark survey wave
    capture confirm variable wave
    if _rc gen int wave = .
    replace wave = real("`w'") if missing(wave)

    * Fill in year
    capture confirm variable year
    if _rc gen int year = .
    replace year = 2000 + wave if missing(year)
}

* Convert negative codes to missing (excluding ID/time/wave variables)
ds, has(type numeric)
foreach v of varlist `r(varlist)' {
    if inlist("`v'","pid","year","wave") continue
    quietly replace `v' = . if `v' < 0
}

* Check uniqueness
capture isid pid year
if _rc {
    duplicates report pid year
    di as error "pid-year is not unique: please check whether some yearly files were not deduplicated or were appended repeatedly"
    exit 459
}

save "${POUT}\Panel_data.dta", replace

* Used to check missing values:
* In a previous run, I found that when father/mother income was added separately,
* the number of non-missing observations from 2016 onward was less than one thousand.
* Therefore, I additionally matched family_income, whose missing rate is much lower,
* at around 10%.
bysort year: count
bysort year: count if !missing(family_income)      
bysort year: summarize family_income, detail

compress
save "${POUT}`c(dirsep)'Panel_data.dta", replace

log close
