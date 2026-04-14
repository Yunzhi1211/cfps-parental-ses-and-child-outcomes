* Intro & Other Graph 

clear all
set more off

*-------------------------------
* 0. Path and environment setup
*-------------------------------
global PROJ "D:\一个文件夹\学习\学习\hku\8001\CFPS Project"
global POUT  "${PROJ}\3_output\panel"
global OUT   "${PROJ}\3_output"
global LOG   "${PROJ}\4_log"

foreach dir in "${OUT}" "${OUT}\tab" "${OUT}\fig" "${LOG}" {
    cap mkdir "`dir'"
}

cap log close
local t = subinstr("`c(current_time)'", ":", "_", .)
log using "${LOG}\analysis_regression_`c(current_date)'_`t'.log", replace text

set scheme s2color
graph set window fontface "Arial"

* Check required packages
foreach pkg in estout coefplot {
    capture which `pkg'
    if _rc {
        di as error "`pkg' not found. Please install it first: ssc install `pkg'"
        exit 499
    }
}

preserve

use "${POUT}\Panel_data.dta", clear

keep if !missing(child_isei)
keep if !missing(f_isei) | !missing(m_isei)

drop if child_isei < 0
drop if f_isei < 0 & !missing(f_isei)
drop if m_isei < 0 & !missing(m_isei)

*============================*
* 2. Construct parental occupational status variable
*============================*
gen parent_isei = .
replace parent_isei = (f_isei + m_isei)/2 if !missing(f_isei) & !missing(m_isei)
replace parent_isei = f_isei if missing(parent_isei) & !missing(f_isei)
replace parent_isei = m_isei if missing(parent_isei) & !missing(m_isei)

drop if missing(parent_isei)

*============================*
* 3. Collapse five groups into three groups
*============================*
xtile p5 = parent_isei, nq(5)

gen group3 = .
replace group3 = 1 if p5 == 1
replace group3 = 2 if inlist(p5, 2, 3, 4)
replace group3 = 3 if p5 == 5

label define g3 1 "Bottom 20%" 2 "Middle 60%" 3 "Top 20%", replace
label values group3 g3

*============================*
* 4. Check means
*============================*
tabstat child_isei, by(group3) stat(mean sd n)

*============================*
* 5. Draw bar chart
*============================*
graph bar (mean) child_isei, ///
    over(group3, label(labsize(large))) ///
    asyvars ///
    bar(1, color("198 219 239")) ///
    bar(2, color("107 174 214")) ///
    bar(3, color("8 81 156")) ///
    blabel(bar, format(%4.1f) color(black) size(large)) ///
    title("Mean Child Occupational Status by Parental Status Group", size(medsmall)) ///
    subtitle("CFPS 2010-2022", size(medium)) ///
    ytitle("Mean Child Occupational Status (ISEI)", size(medium)) ///
    ylabel(0(10)60, angle(0) labsize(medium)) ///
    legend(off) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(bottom_middle_top_bar, replace)

*============================*
* 6. Export figure
*============================*
graph export "${OUT}\fig\0_1_low_middle_high.png", replace width(2400)

restore

log close
