********************************************************************************
* Robustness Check
********************************************************************************

clear all
set more off

*-------------------------------
* 0. Paths and environment setup
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
log using "${LOG}\robustness_`c(current_date)'_`t'.log", replace text

set scheme s2color
graph set window fontface "Arial"

* Check required packages
foreach pkg in estout coefplot winsor2 {
    capture which `pkg'
    if _rc {
        di as error "`pkg' not found. Please install it first: ssc install `pkg'"
        exit 499
    }
}

* Local table dictionary
local vlab ///
    parent_ses_pca         "Parent SES (PCA)" ///
    parent_ses_pca_winsor  "Parent SES (PCA, winsorized)" ///
    child_gender           "Child gender" ///
    child_age              "Child age" ///
    child_urban            "Urban" ///
    log_family_income      "Log family income" ///
    child_edu              "Child education" ///
    f_isei                 "Father ISEI" ///
    m_isei                 "Mother ISEI" ///
    f_edu                  "Father education" ///
    m_edu                  "Mother education" ///
    parent_party_any       "Any parent CPC"

*-------------------------------
* 1. Change the fixed-effects specification
*-------------------------------

use "${POUT}\Panel_data.dta", clear

* Check core variables
local must child_isei f_isei m_isei f_edu m_edu f_party m_party
foreach v of local must {
    confirm variable `v'
}

* Set to 1 if either parent is a CPC member; set to missing (.) only if both are missing
gen byte parent_party_any = (f_party > 0 | m_party > 0) if !missing(f_party) | !missing(m_party)
label var parent_party_any "Any parent CPC (1/0; .=both missing)"

label var child_isei "Child ISEI"

*-------------------------------
* Model specification
*-------------------------------
local must_controls  child_gender child_age child_urban
local fe_full        "i.child_province"
local pca_vars       f_isei m_isei parent_party_any f_edu m_edu
local sep_regs_one   f_isei m_isei f_edu m_edu parent_party_any

local minN 20
levelsof year, local(yrs)

*-------------------------------
* Year-by-year regressions: integrated loop for PCA and separate models
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

di "=== Robustness Check 1: Alternative fixed-effects specification ==="

* Add year fixed effects
estimates clear
local fe_year "i.year"
local fe_prov_year "i.child_province i.year"

* Ensure control variables exist in the regression
local must_controls "child_gender child_age child_urban"

* Baseline model (province FE only)
regress child_isei parent_ses_pca `must_controls' i.child_province if !missing(parent_ses_pca)
estimates store base_fe_prov

* Add year FE
regress child_isei parent_ses_pca `must_controls' i.child_province i.year if !missing(parent_ses_pca)
estimates store base_fe_prov_year

* Variables actually present
local kept_vars
foreach var in parent_ses_pca `must_controls' {
    capture confirm variable `var'
    if _rc == 0 {
        estimates restore base_fe_prov
        matrix b = e(b)
        if colnumb(b, "`var'") != 0 {
            local kept_vars `kept_vars' `var'
        }
    }
}

* Export comparison table
esttab base_fe_prov base_fe_prov_year using "${OUT}\tab\6_1_robust_fe_comparison.rtf", replace ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2 adj_r2, fmt(0 3 3)) ///
    keep(`kept_vars') ///  
    varlabels(`vlab') ///
    mtitle("Prov FE" "Prov+Year FE") ///
    title("Robustness: Fixed Effects Comparison") ///
    compress
*-------------------------------
* 2. Exclude extreme values (Winsorization)
*-------------------------------
di "=== Robustness Check 2: Excluding extreme values (Winsorization) ==="

* Generate new variables
gen child_isei_winsor = child_isei
gen parent_ses_pca_winsor = parent_ses_pca

* Set variable labels
label variable parent_ses_pca_winsor "parent_ses_pca"

* Winsorize the new variables
winsor2 child_isei_winsor parent_ses_pca_winsor, cut(1 99) replace

* Baseline model
regress child_isei_winsor parent_ses_pca_winsor child_gender child_age child_urban i.child_province i.year ///
    if !missing(child_isei_winsor, parent_ses_pca_winsor), cluster(child_province)
estimates store winsor

* Compare with the original model
* Automatically check variables actually present
local kept_vars parent_ses_pca parent_ses_pca_winsor child_gender child_age child_urban
foreach var in parent_ses_pca child_gender child_age child_urban {
    capture confirm variable `var'
    if _rc == 0 {
        estimates restore base_fe_prov
        matrix b = e(b)
        if colnumb(b, "`var'") != 0 {
            local kept_vars `kept_vars' `var'
        }
    }
}

* Export comparison table
esttab base_fe_prov winsor using "${OUT}\tab\6_2_robust_winsor.rtf", replace ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2 adj_r2, fmt(0 3 3)) ///
    keep(parent_ses_pca parent_ses_pca_winsor child_gender child_age child_urban) ///
    varlabels( ///
        parent_ses_pca        "Parent SES (original)" ///
        parent_ses_pca_winsor "Parent SES (winsorized)" ///
        child_gender          "Child gender" ///
        child_age             "Child age" ///
        child_urban           "Urban" ///
    ) ///
    mtitle("Original" "Winsorized (1%-99%)") ///
    title("Robustness: Winsorization Comparison (1%-99%)") ///
    compress

*-------------------------------
* 3. Subsample analysis: Urban-rural groups
*-------------------------------
di "=== Robustness Check 3: Urban-rural groups ==="

* Urban sample
regress child_isei parent_ses_pca child_gender child_age i.child_province i.year ///
    if child_urban == 1, cluster(child_province)
estimates store urban

* Rural sample
regress child_isei parent_ses_pca child_gender child_age i.child_province i.year ///
    if child_urban == 0, cluster(child_province)
estimates store rural

* Automatically check variables actually present
local kept_vars
foreach var in parent_ses_pca child_gender child_age child_urban {
    capture confirm variable `var'
    if _rc == 0 {
        estimates restore urban
        matrix b = e(b)
        if colnumb(b, "`var'") != 0 {
            local kept_vars `kept_vars' `var'
        }
    }
}

* Export comparison table
esttab urban rural using "${OUT}\tab\6_3_robust_urban_rural.rtf", replace ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2 adj_r2, fmt(0 3 3)) ///
    keep(parent_ses_pca child_gender child_age) ///
    varlabels(`vlab') ///
    mtitle("Urban" "Rural") ///
    title("Robustness: Urban vs Rural Subsamples") ///
    compress

*-------------------------------
* 4. Subsample analysis: Child gender groups
*-------------------------------
di "=== Robustness Check 4: Child gender groups ==="

* Male sample
regress child_isei parent_ses_pca child_gender child_age child_urban i.child_province i.year if child_gender == 1, cluster(child_province)
estimates store male

* Female sample
regress child_isei parent_ses_pca child_gender child_age child_urban i.child_province i.year if child_gender == 0, cluster(child_province)
estimates store female

* Automatically check variables actually present
local kept_vars
foreach var in parent_ses_pca child_gender child_age child_urban {
    capture confirm variable `var'
    if _rc == 0 {
        estimates restore male
        matrix b = e(b)
        if colnumb(b, "`var'") != 0 {
            local kept_vars `kept_vars' `var'
        }
    }
}

* Export comparison table
esttab male female using "${OUT}\tab\6_4_robust_male_female.rtf", replace ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2 adj_r2, fmt(0 3 3)) ///
    keep(`kept_vars') ///
    varlabels(`vlab') ///
    mtitle("Male" "Female") ///
    title("Robustness: Gender Subsamples") ///
    compress

*-------------------------------
* 5. Replace the dependent variable: SIOPS (compared with the original model)
*-------------------------------
di "=== Robustness Check 5: Alternative dependent variable (SIOPS) ==="

* Baseline model
regress child_siops parent_ses_pca child_gender child_age child_urban i.child_province i.year ///
    if !missing(child_siops, parent_ses_pca), cluster(child_province)
estimates store siops

* Check variables actually present
local kept_vars
foreach var in parent_ses_pca child_gender child_age child_urban {
    capture confirm variable `var'
    if _rc == 0 {
        estimates restore base_fe_prov_year
        matrix b = e(b)
        if colnumb(b, "`var'") != 0 {
            local kept_vars `kept_vars' `var'
        }
    }
}

* Export comparison table
esttab base_fe_prov_year siops using "${OUT}\tab\6_5_robust_siops.rtf", replace ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2 adj_r2, fmt(0 3 3)) ///
    keep(`kept_vars') ///
    varlabels(`vlab') ///
    mtitle("Original (child_isei)" "SIOPS") ///
    title("Robustness: Alternative Outcome (SIOPS)") ///
    compress

*-------------------------------
* 6. Replace the dependent variable: EGP category (compared with the original model)
*-------------------------------
di "=== Robustness Check 6: Alternative dependent variable (EGP Category) ==="

* Baseline model
regress child_egp parent_ses_pca child_gender child_age child_urban i.child_province i.year ///
    if !missing(child_egp, parent_ses_pca), cluster(child_province)
estimates store egp

* Check variables actually present
local kept_vars
foreach var in parent_ses_pca child_gender child_age child_urban {
    capture confirm variable `var'
    if _rc == 0 {
        estimates restore base_fe_prov_year
        matrix b = e(b)
        if colnumb(b, "`var'") != 0 {
            local kept_vars `kept_vars' `var'
        }
    }
}

* Export comparison table
esttab base_fe_prov_year egp using "${OUT}\tab\6_6_robust_egp.rtf", replace ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2 adj_r2, fmt(0 3 3)) ///
    keep(`kept_vars') ///
    varlabels(`vlab') ///
    mtitle("Original (child_isei)" "EGP Category") ///
    title("Robustness: Alternative Outcome (EGP Category)") ///
    compress

*-------------------------------
* 7. Adjust control variables: Log family income
*-------------------------------
di "=== Robustness Check 7: Additional control variable (Log family income) ==="

* Generate log family income
gen ln_family_income = ln(family_income) if !missing(family_income)

* Baseline model
regress child_isei parent_ses_pca child_gender child_age child_urban ln_family_income i.child_province i.year, cluster(child_province)
estimates store income_control

* Check variables actually present
local kept_vars
foreach var in parent_ses_pca child_gender child_age child_urban ln_family_income {
    capture confirm variable `var'
    if _rc == 0 {
        estimates restore base_fe_prov_year
        matrix b = e(b)
        if colnumb(b, "`var'") != 0 {
            local kept_vars `kept_vars' `var'
        }
    }
}

* Export comparison table in RTF format
esttab base_fe_prov_year income_control using "${OUT}\tab\6_7_robust_income.rtf", replace ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) stats(N r2 adj_r2, fmt(0 3 3)) ///
    keep(`kept_vars') ///
    varlabels(`vlab') ///
    mtitle("Original (child_isei)" "Income Control") ///
    title("Robustness: Additional Control (ln Family Income)") ///
    compress

*============================================================*
* 8. Summarize all robustness checks in one table: polished version
*============================================================*

* Check core variables
local must child_isei child_siops child_egp parent_ses_pca child_gender child_age child_urban child_province year
foreach v of local must {
    capture confirm variable `v'
    if _rc != 0 {
        di as error "Variable `v' not found."
        exit 198
    }
}

* Winsorize
capture noisily winsor2 child_isei_winsor parent_ses_pca_winsor, cut(1 99) replace

* Log family income
capture drop ln_family_income
gen ln_family_income = ln(family_income) if family_income > 0 & !missing(family_income)

*------------------------------------------------------------*
* 8.1. Construct unified "core variable names" for regressions
*      so that all models display only one row: Parent SES
*------------------------------------------------------------*
capture drop y_core x_core
gen double y_core = .
gen double x_core = .

label var y_core "Outcome"
label var x_core "Parent SES"

*------------------------------------------------------------*
* 8.2. Baseline model: Province FE
*------------------------------------------------------------*
replace y_core = child_isei
replace x_core = parent_ses_pca

eststo m1: regress y_core x_core child_gender child_age child_urban ///
    i.child_province if !missing(y_core, x_core, child_gender, child_age, child_urban), ///
    cluster(child_province)

estadd local FE      "Prov"
estadd local Sample  "Full"
estadd local Outcome "ISEI"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.3. Baseline model: Province + Year FE
*------------------------------------------------------------*
replace y_core = child_isei
replace x_core = parent_ses_pca

eststo m2: regress y_core x_core child_gender child_age child_urban ///
    i.child_province i.year ///
    if !missing(y_core, x_core, child_gender, child_age, child_urban), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Full"
estadd local Outcome "ISEI"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.4. Winsorized model
*------------------------------------------------------------*
replace y_core = child_isei_winsor
replace x_core = parent_ses_pca_winsor

eststo m3: regress y_core x_core child_gender child_age child_urban ///
    i.child_province i.year ///
    if !missing(y_core, x_core, child_gender, child_age, child_urban), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Full"
estadd local Outcome "ISEI"
estadd local Winsor  "Yes"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.5. Urban subsample
*------------------------------------------------------------*
replace y_core = child_isei
replace x_core = parent_ses_pca

eststo m4: regress y_core x_core child_gender child_age ///
    i.child_province i.year ///
    if child_urban == 1 & !missing(y_core, x_core, child_gender, child_age), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Urban"
estadd local Outcome "ISEI"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.6. Rural subsample
*------------------------------------------------------------*
replace y_core = child_isei
replace x_core = parent_ses_pca

eststo m5: regress y_core x_core child_gender child_age ///
    i.child_province i.year ///
    if child_urban == 0 & !missing(y_core, x_core, child_gender, child_age), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Rural"
estadd local Outcome "ISEI"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.7. Male subsample
*------------------------------------------------------------*
replace y_core = child_isei
replace x_core = parent_ses_pca

eststo m6: regress y_core x_core child_age child_urban ///
    i.child_province i.year ///
    if child_gender == 1 & !missing(y_core, x_core, child_age, child_urban), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Male"
estadd local Outcome "ISEI"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.8. Female subsample
*------------------------------------------------------------*
replace y_core = child_isei
replace x_core = parent_ses_pca

eststo m7: regress y_core x_core child_age child_urban ///
    i.child_province i.year ///
    if child_gender == 0 & !missing(y_core, x_core, child_age, child_urban), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Female"
estadd local Outcome "ISEI"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.9. Income control
*------------------------------------------------------------*
replace y_core = child_isei
replace x_core = parent_ses_pca

eststo m8: regress y_core x_core child_gender child_age child_urban ln_family_income ///
    i.child_province i.year ///
    if !missing(y_core, x_core, child_gender, child_age, child_urban, ln_family_income), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Full"
estadd local Outcome "ISEI"
estadd local Winsor  "No"
estadd local Income  "Yes"

*------------------------------------------------------------*
* 8.10. Alternative dependent variable: SIOPS
*------------------------------------------------------------*
replace y_core = child_siops
replace x_core = parent_ses_pca

eststo m9: regress y_core x_core child_gender child_age child_urban ///
    i.child_province i.year ///
    if !missing(y_core, x_core, child_gender, child_age, child_urban), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Full"
estadd local Outcome "SIOPS"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.11. Alternative dependent variable: EGP
*------------------------------------------------------------*
replace y_core = child_egp
replace x_core = parent_ses_pca

eststo m10: regress y_core x_core child_gender child_age child_urban ///
    i.child_province i.year ///
    if !missing(y_core, x_core, child_gender, child_age, child_urban), ///
    cluster(child_province)

estadd local FE      "Prov+Yr"
estadd local Sample  "Full"
estadd local Outcome "EGP"
estadd local Winsor  "No"
estadd local Income  "No"

*------------------------------------------------------------*
* 8.12. Export one polished summary table
*------------------------------------------------------------*
esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 ///
    using "${OUT}\tab\6_8_robustness_summary_pretty.rtf", replace ///
    cells(b(star fmt(2)) se(par fmt(2))) ///
    star(* 0.1 ** 0.05 *** 0.01) ///
    keep(x_core) ///
    coeflabels(x_core "Parent SES") ///
    mtitle("Base" "Prov+Year" "Winsor" "Urban" "Rural" "Male" "Female" "Income" "SIOPS" "EGP") ///
    stats(N r2 FE Sample Outcome Winsor Income, ///
        labels("N" "R2" "FE" "Samp." "Y" "Win." "Inc.") ///
        fmt(0 2)) ///
    title("Robustness Summary") ///
    addnotes("Controls included where applicable.", ///
             "Clustered SEs at province level.") ///
    compress nogaps

*-------------------------------
* 9. Robustness Figure 6-1: Comparison of key coefficients
*-------------------------------
di "=== Robustness Figure 1: Comparison of parent_ses_pca coefficients ==="

coefplot ///
    (base_fe_prov,       label("Prov FE")) ///
    (base_fe_prov_year,  label("Prov + Year FE")) ///
    (winsor,             label("Winsorized")) ///
    (urban,              label("Urban")) ///
    (rural,              label("Rural")) ///
    (male,               label("Male")) ///
    (female,             label("Female")) ///
    (income_control,     label("Income Control")), ///
    keep(parent_ses_pca parent_ses_pca_winsor) ///
    rename(parent_ses_pca_winsor = parent_ses_pca) ///
    xline(0, lpattern(dash) lcolor(red)) ///
    drop(_cons) ///
    vertical ///
    ciopts(recast(rcap) lcolor(navy)) ///
    msymbol(D) ///
    mcolor(navy) ///
    title("Robustness of Parent SES Effect on Child ISEI") ///
    ytitle("Coefficient Estimate") ///
    xtitle("Model Specification") ///
    graphregion(color(white))

graph export "${OUT}\fig\6_1_robust_coefplot_main.png", replace width(2400)

log close

