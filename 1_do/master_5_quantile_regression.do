********************************************************************************
* Quantile Regression Analysis (Faceted Panel by Year)
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

* Create required folders
foreach dir in "${OUT}" "${OUT}\tab" "${OUT}\fig" "${LOG}" {
    cap mkdir "`dir'"
}

cap log close
local t = subinstr("`c(current_time)'", ":", "_", .)
log using "${LOG}\analysis_QR_`c(current_date)'_`t'.log", replace text

set scheme s2color
graph set window fontface "Arial"


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
* 3. Model setup and preparation of the PCA variable
*-------------------------------
local must_controls  child_gender child_age child_urban
local fe_full        "i.child_province"
local pca_vars       f_isei m_isei parent_party_any f_edu m_edu

local minN 20
levelsof year, local(yrs)

capture drop parent_ses_pca
gen double parent_ses_pca = .
label var parent_ses_pca "Parent SES PCA score (PC1)"

foreach yy of local yrs {
    tempvar touse_pca
    mark `touse_pca' if year == `yy'
    markout `touse_pca' child_isei `pca_vars' child_province
    
    qui count if `touse_pca'
    if r(N) >= `minN' {
        capture quietly pca `pca_vars' if `touse_pca', components(1)
        if !_rc {
            tempvar temp_pca
            qui predict double `temp_pca' if `touse_pca', score
            qui replace parent_ses_pca = `temp_pca' if `touse_pca'
        }
    }
}
di "PCA variable preparation completed!"

*-------------------------------------------------------------------------------
* 4. Quantile Regression
*-------------------------------------------------------------------------------

local yrs7 2010 2012 2014 2016 2018 2020 2022
local q_list 10 25 50 75 90

* ==========================================
* Step 1: Extract quantile results for all years
* ==========================================

tempfile qr_all_data
tempname post_qr

postfile `post_qr' int year byte q double b se lo hi N using `qr_all_data', replace

foreach yy of local yrs7 {
    di "  Processing Year `yy'..."
    tempvar touse_y
    mark `touse_y' if year==`yy'
    markout `touse_y' child_isei parent_ses_pca `must_controls' child_province
    
    qui count if `touse_y'
    local Ny = r(N)
    
    foreach qq of local q_list {
        local b = .
        local se = .
        local lo = .
        local hi = .
        capture quietly qreg child_isei parent_ses_pca `must_controls' `fe_full' if `touse_y', q(`=`qq'/100')
        if !_rc {
            local b  = _b[parent_ses_pca]
            local se = _se[parent_ses_pca]
            local lo = `b' - 1.96*`se'
            local hi = `b' + 1.96*`se'
        }
        post `post_qr' (`yy') (`qq') (`b') (`se') (`lo') (`hi') (`Ny')
    }
}

* ==========================================
* Step 2: Compute pooled-sample results (set as year=9999)
* ==========================================
di "  Processing Pooled Sample..."
preserve
drop if missing(child_isei, parent_ses_pca, child_province)
foreach v of local must_controls {
    drop if missing(`v')
}
qui count
local N_pooled = r(N)

* Compute pooled OLS as the reference line
quietly regress child_isei parent_ses_pca `must_controls' i.year `fe_full'
local ols_b = _b[parent_ses_pca]
local ols_se = _se[parent_ses_pca]
local ols_ul = `ols_b' + 1.96*`ols_se'
local ols_ll = `ols_b' - 1.96*`ols_se'

* Compute pooled quantiles
foreach qq of local q_list {
    local b = .
    local se = .
    local lo = .
    local hi = .
    capture quietly qreg child_isei parent_ses_pca `must_controls' i.year `fe_full', q(`=`qq'/100')
    if !_rc {
        local b  = _b[parent_ses_pca]
        local se = _se[parent_ses_pca]
        local lo = `b' - 1.96*`se'
        local hi = `b' + 1.96*`se'
    }
    post `post_qr' (9999) (`qq') (`b') (`se') (`lo') (`hi') (`N_pooled')
}
postclose `post_qr'
restore

* ==========================================
* Step 3: Visualize the saved coefficient results
* ==========================================
* ---------------------------------------------------------
* 3.1 Prepare plotting data
* ---------------------------------------------------------
preserve
use `qr_all_data', clear
drop if missing(b)

* Map x-axis positions
gen x = .
replace x = 1 if q==10
replace x = 2 if q==25
replace x = 3 if q==50
replace x = 4 if q==75
replace x = 5 if q==90

* Generate variables from the pooled OLS parameters computed above for use in panel plots
gen ols_line = `ols_b'  if year == 9999
gen ols_hi   = `ols_ul' if year == 9999
gen ols_lo   = `ols_ll' if year == 9999

* ---------------------------------------------------------
* Figure A: Faceted panel trend by year
* Can show changes over time
* Figures A, B, and C can be placed together in the middle of the paper
* ---------------------------------------------------------
levelsof year, local(all_years)
foreach y of local all_years {
    if `y' == 9999 {
        label define yr_lbl `y' "Total", add
    }
    else {
        label define yr_lbl `y' "`y'", add
    }
}
label values year yr_lbl

twoway ///
    (rcap hi lo x, lc(navy%60) lw(medthick)) ///
    (connected b x, lc(navy) mc(navy) msymbol(O) msize(medium) lw(medthick)) ///
    (line ols_line x if year==9999, lc(maroon) lw(medthick)) ///
    (line ols_hi x if year==9999, lc(maroon%60) lp(shortdash)) ///
    (line ols_lo x if year==9999, lc(maroon%60) lp(shortdash)) ///
    , ///
    by(year, rows(2) compact ///
        title("Quantile Regression Effects by Year", size(large) margin(b=3)) ///
        note("")  ///
        graphregion(fcolor(white) margin(medsmall)) legend(off)) ///
    subtitle(, fcolor(eltblue%20) lcolor(none) size(medium)) ///
    xtitle("Quantile", size(medlarge) margin(t=2)) ///
    ytitle("Coefficient", size(medlarge) margin(r=2)) ///
    xscale(range(0.5 5.5)) ///
    xlabel(1 "10" 2 "25" 3 "50" 4 "75" 5 "90", labsize(medsmall)) ///
    yline(0, lc(gs10) lp(dash) lw(vthin)) 

graph export "${OUT}\fig\5_1_qr_1_faceted_panel.png", replace width(3000)

* ---------------------------------------------------------
* Figure B: Pooled QR vs OLS
* Figures A, B, and C can be placed together in the middle of the paper
* This figure shows clear distributional heterogeneity in the effect of parent SES on child ISEI
* The blue line indicates that parent SES has a stronger effect at higher quantiles
* The red line represents the average effect, which can mask heterogeneity
* ---------------------------------------------------------
twoway ///
    (rcap hi lo x if year==9999, lc(navy) lw(thick) msize(large)) ///
    (connected b x if year==9999, lc(navy) mc(navy) ms(O) msize(large) lw(thick)) ///
    , ///
    title("Effect of Parent SES across Child ISEI Distribution", size(large) color(black)) ///
    subtitle("Quantile Regression vs OLS (Pooled Sample)", size(medium) color(gs5)) ///
    note("") ///
    yline(`ols_b', lc(maroon) lw(thick)) ///
    yline(`ols_ul', lc(maroon%70) lp(shortdash) lw(medthick)) ///
    yline(`ols_ll', lc(maroon%70) lp(shortdash) lw(medthick)) ///
    xtitle("Quantiles of Child ISEI", size(medlarge) margin(t=3)) ///
    ytitle("Coefficient of Parent SES (PCA)", size(medlarge) margin(r=3)) ///
    xscale(range(0.5 5.5)) ///
    xlabel(1 "Q10" 2 "Q25" 3 "Q50" 4 "Q75" 5 "Q90", labsize(medium)) ///
    ylabel(0(1)5, labsize(medium) angle(0)) ///
    legend(off) ///
    graphregion(fcolor(white)) plotregion(margin(small))

graph export "${OUT}\fig\5_2_qr_2_pooled_vs_ols.png", replace width(2400)

* ---------------------------------------------------------
* Figure C: Cross-year evolution trend
* Figures A, B, and C can be placed together in the middle of the paper
* This figure can take a central place, showing whether the gap is widening or narrowing
* It can then be followed by mediation analysis to explore the mechanism
* ---------------------------------------------------------

drop if year == 9999

twoway ///
    (connected b year if q==75, lc(maroon) mc(maroon) msymbol(T) msize(medlarge) lw(medthick)) ///
    (connected b year if q==50, lc(gs8) mc(gs8) msymbol(D) lp(dash) msize(medlarge) lw(medthick)) ///
    (connected b year if q==25, lc(navy) mc(navy) msymbol(O) msize(medlarge) lw(medthick)) ///
    , ///
    title("Evolution of Parent SES Effect by Quantiles", size(large) color(navy)) ///
    subtitle("Are impacts widening at the top or bottom?", size(medsmall) color(gs5)) ///
    xtitle("Year", size(medlarge) margin(t=2)) ///
    ytitle("Coefficient of Parent SES", size(medlarge) margin(r=2)) ///
    xlabel(2010(2)2022, labsize(medium)) ///
    ylabel(2(1)7, labsize(medium) angle(0)) ///
    legend(order(1 "75th Quantile (Upper Class)" 2 "50th Quantile (Middle)" 3 "25th Quantile (Lower Class)") ///
           ring(0) pos(11) cols(1) region(lcolor(none) fcolor(none)) size(medium)) ///
    graphregion(fcolor(white)) plotregion(margin(small))

graph export "${OUT}\fig\5_3_qr_3_evolution_trend.png", replace width(2400)

restore 
// End the outermost preserve and return to the original micro-level panel dataset

* ---------------------------------------------------------
* Figure D: Cross-sectional fitted scatter plot for 2022
* This can be placed at the beginning of the paper to illustrate
* that the rise in the median is a phenomenon worth studying
* ---------------------------------------------------------
di "Drawing the 2022 quantile fitted scatter plot..."

* Extract 2022 data
preserve
keep if year == 2022
keep if !missing(child_isei, parent_ses_pca)

qui qreg child_isei parent_ses_pca, q(0.90)
qui predict double y_hat_90

qui qreg child_isei parent_ses_pca, q(0.50)
qui predict double y_hat_50

qui qreg child_isei parent_ses_pca, q(0.10)
qui predict double y_hat_10

* Sort by x-axis variable
sort parent_ses_pca

twoway ///
    (scatter child_isei parent_ses_pca, mcolor(gs12%40) msize(vsmall) msymbol(O)) ///
    (line y_hat_90 parent_ses_pca, lcolor(maroon) lwidth(medthick)) ///
    (line y_hat_50 parent_ses_pca, lcolor(black) lpattern(shortdash) lwidth(medthick)) ///
    (line y_hat_10 parent_ses_pca, lcolor(navy) lwidth(medthick)) ///
    , ///
    title("Quantile Fits: Child ISEI vs Parent SES (2022)", size(large) color(navy)) ///
    xtitle("Parent SES (PCA score)", size(medlarge) margin(t=2)) ///
    ytitle("Child ISEI", size(medlarge) margin(r=2)) ///
    ylabel(20(20)100, labsize(medium) angle(0)) ///
    xlabel(-4(2)4, labsize(medium)) ///
    legend(order(2 "90th Quantile" 3 "50th Quantile (Median)" 4 "10th Quantile") ///
           pos(6) row(1) region(lcolor(none) fcolor(none)) size(medium) margin(t=3)) ///
    graphregion(fcolor(white)) plotregion(margin(small))

graph export "${OUT}\fig\5_4_qr_4_scatter_fits_2022.png", replace width(2400)

restore

*-------------------------------
* 4.5 Main text table: Pooled OLS + Quantile Regression
*-------------------------------
cap which esttab
if _rc ssc install estout, replace

preserve
drop if missing(child_isei, parent_ses_pca, child_province)
foreach v of local must_controls {
    drop if missing(`v')
}

eststo clear

* OLS
quietly regress child_isei parent_ses_pca `must_controls' i.year `fe_full'
eststo OLS
estadd local Controls "Yes"
estadd local YearFE   "Yes"
estadd local ProvFE   "Yes"

* Quantile regressions
foreach qq in 10 25 50 75 90 {
    quietly qreg child_isei parent_ses_pca `must_controls' i.year `fe_full', q(`=`qq'/100')
    eststo Q`qq'
    estadd local Controls "Yes"
    estadd local YearFE   "Yes"
    estadd local ProvFE   "Yes"
}

* Export the RTF table for use in the main text
esttab OLS Q10 Q25 Q50 Q75 Q90 using "${OUT}\tab\5_1_main_qr_table.rtf", replace rtf ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label ///
    mtitles("OLS" "Q10" "Q25" "Q50" "Q75" "Q90") ///
    keep(parent_ses_pca) ///
    order(parent_ses_pca) ///
    coeflabels(parent_ses_pca "Parent SES (PCA)") ///
    stats(Controls YearFE ProvFE N, ///
          labels("Controls" "Year FE" "Province FE" "N") ///
          fmt(%s %s %s %9.0fc)) ///
    title("Effect of Parent SES on Child ISEI: OLS and Quantile Regressions") ///
    note("Dependent variable: Child ISEI. All models include child-level controls. Robust significance: * p<0.10, ** p<0.05, *** p<0.01.")

restore

log close
