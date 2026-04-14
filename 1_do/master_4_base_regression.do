********************************************************************************
* Regression Analysis (PCA, Separate Regs, Overlays, Trends)
********************************************************************************

clear all
set more off

*-------------------------------
* 1. Paths and environment setup
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

*-------------------------------
* 2. Load data and process variables
*-------------------------------
confirm file "${POUT}\Panel_data.dta"
use "${POUT}\Panel_data.dta", clear

* Check core variables
local must child_isei f_isei m_isei f_edu m_edu f_party m_party
foreach v of local must {
    confirm variable `v'
}

* Generate/process variables
gen double log_family_income = ln(family_income) if family_income > 0
label var log_family_income "Log(family income)"

* Set to 1 if either parent is a CPC member; set to missing (.) only if both are missing
gen byte parent_party_any = (f_party > 0 | m_party > 0) if !missing(f_party) | !missing(m_party)
label var parent_party_any "Any parent CPC (1/0; .=both missing)"

label var child_isei "Child ISEI"

*-------------------------------
* 3. Model specification
*-------------------------------
local must_controls  child_gender child_age child_urban
local fe_full        "i.child_province"
local pca_vars       f_isei m_isei parent_party_any f_edu m_edu
local sep_regs_one   f_isei m_isei f_edu m_edu parent_party_any

local minN 20
levelsof year, local(yrs)

*-------------------------------
* 4. Year-by-year regressions: integrated loop for PCA and separate models
*-------------------------------
estimates clear
local kept_pca ""
local kept_sep ""
local mtitles_ok ""

* Generate the full-sample PCA variable before the loop
capture drop parent_ses_pca
gen double parent_ses_pca = .
label var parent_ses_pca "Parent SES PCA score (PC1)"

* Prepare a file to record coefficients
tempfile coef_data
tempname post_coef
postfile `post_coef' int year ///
    double b_pca se_pca ///
    double b_fisei se_fisei double b_misei se_misei ///
    double b_fedu se_fedu double b_medu se_medu ///
    double b_party se_party ///
    using `coef_data', replace

local fignum = 1

foreach yy of local yrs {
    di "================ Year `yy' ================"
    
    *--- (A) Mark sample for PCA model ---
    tempvar touse_pca touse_sep
    mark `touse_pca' if year == `yy'
    markout `touse_pca' child_isei `pca_vars' child_province
    
    *--- (B) Mark sample for separate model ---
    mark `touse_sep' if year == `yy'
    markout `touse_sep' child_isei `sep_regs_one' `must_controls' child_province

    * Initialize posted coefficients as missing
    local p_b = . 
    local p_se = .
    local f_b = . 
    local f_se = . 
    local m_b = . 
    local m_se = .
    local fe_b = . 
    local fe_se = . 
    local me_b = . 
    local me_se = .
    local pa_b = . 
    local pa_se = .
    local success 0

    *=== Run PCA model ===
    count if `touse_pca'
    if r(N) >= `minN' {
        capture noisily pca `pca_vars' if `touse_pca', components(1)
        if !_rc {
            * Fill the year-specific PCA score into the full-sample variable
            tempvar temp_pca
            predict double `temp_pca' if `touse_pca', score
            replace parent_ses_pca = `temp_pca' if `touse_pca'
            
            capture noisily regress child_isei parent_ses_pca `must_controls' `fe_full' if `touse_pca'
            if !_rc {
                estimates store pca`yy'
                local kept_pca "`kept_pca' pca`yy'"
                local p_b = _b[parent_ses_pca]
                local p_se = _se[parent_ses_pca]
                
                * Figures 4-1 to 4-7: year-specific scatter plots
                twoway (scatter child_isei parent_ses_pca if `touse_pca', msize(vsmall) msymbol(O)) ///
                       (lfit child_isei parent_ses_pca if `touse_pca', lwidth(0.8)), ///
                       title("Child ISEI vs Parent SES (PCA) (`yy')") xtitle("Parent SES (PCA)") ytitle("Child ISEI") ///
                       legend(off) graphregion(fcolor(white))
                       
                graph export "${OUT}/fig/4_`fignum'_scatter_pca_`yy'.png", replace width(2000)
                
                local ++fignum
            }
        }
    }

    *=== Run separate model ===
    count if `touse_sep'
    if r(N) >= `minN' {
        capture noisily regress child_isei `sep_regs_one' `must_controls' `fe_full' if `touse_sep'
        if !_rc {
            estimates store sep`yy'
            local kept_sep "`kept_sep' sep`yy'"
            local success 1
            
            local f_b = _b[f_isei] 
            local f_se = _se[f_isei]
            local m_b = _b[m_isei] 
            local m_se = _se[m_isei]
            local fe_b = _b[f_edu] 
            local fe_se = _se[f_edu]
            local me_b = _b[m_edu] 
            local me_se = _se[m_edu]
            local pa_b = _b[parent_party_any] 
            local pa_se = _se[parent_party_any]
        }
    }

    * Record year-specific information
    if `success' | "`p_b'" != "." {
        local mtitles_ok "`mtitles_ok' `yy'"
        post `post_coef' (`yy') (`p_b') (`p_se') (`f_b') (`f_se') (`m_b') (`m_se') (`fe_b') (`fe_se') (`me_b') (`me_se') (`pa_b') (`pa_se')
    }
}
postclose `post_coef'

*-------------------------------
* 5. Figure 4-8: Overlay fitted lines only (PCA)
*-------------------------------
quietly summarize parent_ses_pca, detail
local p_min = r(p1)
local p_max = r(p99)

local overlay_cmd ""
local leg_order ""
local i 1
local colors `""24 71 133" "0 114 189" "0 162 162" "119 172 48" "237 177 32" "217 83 25" "162 20 47""'

foreach yy of local yrs {
    local c : word `i' of `colors'
    local overlay_cmd `"`overlay_cmd' (lfit child_isei parent_ses_pca if year==`yy', lcolor("`c'") lwidth(medthick) range(`p_min' `p_max'))"'
    local leg_order `"`leg_order' `i' "`yy'""'
    local ++i
}

twoway `overlay_cmd', ///
    title("Yearly Trend of SES Transmission", color(black) size(medlarge) margin(b=2)) ///
    subtitle("Fitted lines of Child ISEI vs Parent SES", color(gs5) size(medium) margin(b=3)) ///
    xtitle("Parent SES (PCA score)", size(small) margin(t=3)) ///
    ytitle("Child ISEI", size(small) margin(r=3)) ///
    xlabel(-2(1)5, labsize(small)) /// 
    ylabel(20(10)70, angle(0) labsize(small)) /// 
    legend( order(`leg_order') ring(1) pos(6) row(1) ///
        size(vsmall) ///
        region(lcolor(none) fcolor(none)) ///
        symxsize(6) margin(t=2 b=2) ///
        span justification(center) ) ///
    graphregion(fcolor(white) margin(medlarge)) /// 
    plotregion(margin(small))

graph export "${OUT}\fig\4_8_pca_overlay_fitted_lines.png", replace width(3000) height(2000)

*-------------------------------
* 6. Figure 4-9: Faceted PCA panel plots
*-------------------------------
twoway (scatter child_isei parent_ses_pca, msize(vsmall) msymbol(O) mcolor(gs10%30)) ///
       (lfit child_isei parent_ses_pca, lcolor(navy) lwidth(medthick)), ///
       by(year, rows(2) total title("Child ISEI vs Parent SES (PCA) by Year", color(black)) ///
                note("") legend(off) graphregion(fcolor(white))) ///
       xtitle("Parent SES (PCA score)") ytitle("Child ISEI") 

graph export "${OUT}\fig\4_9_pca_faceted_scatter_fit.png", replace width(3000)

*-------------------------------
* 7. Export combined regression tables 4-1 and 4-2
*-------------------------------
local tb_opts "mtitles(`mtitles_ok') b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2, fmt(0 3))"

if "`kept_pca'" != "" {
    esttab `kept_pca' using "${OUT}\tab\4_1_reg_pca_byyear.rtf", replace `tb_opts' keep(parent_ses_pca) ///
        title("Child ISEI on Parent SES (PCA)") 
}
if "`kept_sep'" != "" {
    esttab `kept_sep' using "${OUT}\tab\4_2_reg_separate_byyear.rtf", replace `tb_opts' keep(`sep_regs_one') ///
        title("Child ISEI on Father/Mother SES components")
}

*-------------------------------
* 8. Plot coefficient trends (Figures 4-10, 4-11, 4-12, 4-13)
*-------------------------------
preserve

use `coef_data', clear

foreach x in pca fisei misei fedu medu party {
    gen double lo_`x' = b_`x' - 1.96*se_`x'
    gen double hi_`x' = b_`x' + 1.96*se_`x'
}

local str_yrs : subinstr local yrs " " " ", all   
local g_opts `"yline(0, lpattern(dash) lcolor(gs8)) xlabel(`str_yrs', angle(0)) xtitle("Year") ytitle("Coefficient") graphregion(fcolor(white)) plotregion(margin(small))"'

* Figure 4-10: PCA
twoway (rcap hi_pca lo_pca year, lcolor(navy%60) lwidth(medthick)) ///
       (scatter b_pca year, mcolor(navy) msymbol(O) msize(medlarge)) ///
       (line b_pca year, lcolor(navy) lwidth(medthick)), ///
       title("Yearly Effect: Parent SES (PCA)") legend(off) `g_opts'
graph export "${OUT}\fig\4_10_trend_pca.png", replace width(2400)

* Figure 4-11: Father vs Mother ISEI
twoway (rcap hi_fisei lo_fisei year, lcolor(maroon%70) lw(medthick)) (scatter b_fisei year, mc(maroon) ms(O)) (line b_fisei year, lc(maroon) lw(medthick)) ///
       (rcap hi_misei lo_misei year, lcolor(navy%70) lw(medthick))   (scatter b_misei year, mc(navy) ms(O))   (line b_misei year, lc(navy) lw(medthick)), ///
       title("Father vs Mother ISEI") legend(order(2 "Father ISEI" 5 "Mother ISEI") ring(0) pos(11) col(1)) `g_opts'
graph export "${OUT}\fig\4_11_trend_parent_ISEI.png", replace width(2400)

* Figure 4-12: Father vs Mother education
twoway (rcap hi_fedu lo_fedu year, lc(maroon%70) lw(medthick)) (scatter b_fedu year, mc(maroon) ms(O)) (line b_fedu year, lc(maroon) lw(medthick)) ///
       (rcap hi_medu lo_medu year, lc(navy%70) lw(medthick))     (scatter b_medu year, mc(navy) ms(O))     (line b_medu year, lc(navy) lw(medthick)), ///
       title("Father vs Mother Education") legend(order(2 "Father edu" 5 "Mother edu") ring(0) pos(11) col(1)) `g_opts'
graph export "${OUT}\fig\4_12_trend_parent_edu.png", replace width(2400)

* Figure 4-13: Any-parent CPC
twoway (rcap hi_party lo_party year, lcolor(black%60) lw(medthick)) ///
       (scatter b_party year, mcolor(black) msymbol(D) msize(medlarge)) ///
       (line b_party year, lcolor(black) lw(medthick)), ///
       title("Any-parent CPC") legend(off) `g_opts'
graph export "${OUT}\fig\4_13_trend_parent_party.png", replace width(2400)

restore

log close
