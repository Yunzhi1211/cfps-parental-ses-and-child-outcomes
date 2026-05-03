********************************************************************************
* Mechanism Analysis (Final English Version)
* Output:
*   Table 7-1. Main mechanism results
*   Table 7-2. Validation and robustness of mechanism analysis
* First Updated on March 30th
* Latest Updated on April 14th
********************************************************************************

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
log using "${LOG}\mechanism_final_`c(current_date)'_`t'.log", replace text

set scheme s2color
graph set window fontface "Arial"

*-------------------------------
* 0.1 Bootstrap settings
*-------------------------------
global BREP 200
global SEED 20260330

*-------------------------------
* 1. Check required packages
*-------------------------------
foreach pkg in estout coefplot winsor2 {
    capture which `pkg'
    if _rc {
        di as error "Package `pkg' not found. Please install it first: ssc install `pkg'"
        exit 499
    }
}

*-------------------------------
* 2. Load data
*-------------------------------
use "${POUT}\Panel_data.dta", clear

*-------------------------------
* 3. Check required variables
*-------------------------------
local mustvars child_isei f_isei m_isei f_edu m_edu f_party m_party ///
               child_gender child_age child_urban child_province year pid

foreach v of local mustvars {
    capture confirm variable `v'
    if _rc {
        di as error "Variable `v' does not exist. Please check Panel_data.dta"
        exit 111
    }
}

*-------------------------------
* 4. Construct parent_party_any
*-------------------------------
capture drop parent_party_any
gen parent_party_any = .
replace parent_party_any = 1 if f_party == 1 | m_party == 1
replace parent_party_any = 0 if (inlist(f_party,0,1) | inlist(m_party,0,1)) ///
    & !(f_party == 1 | m_party == 1)
replace parent_party_any = . if missing(f_party) & missing(m_party)
label var parent_party_any "Either parent is CPC member"

*-------------------------------
* 5. Construct parent_ses_pca
*-------------------------------
capture drop parent_ses_pca
pca f_isei m_isei parent_party_any f_edu m_edu ///
    if !missing(f_isei, m_isei, parent_party_any, f_edu, m_edu), components(1)
predict double parent_ses_pca if e(sample), score
label var parent_ses_pca "Parent SES PCA score (PC1)"

sum parent_ses_pca, detail

tempfile maindata
save `maindata', replace

*-------------------------------
* 5b. Program: rebuild parent SES
*-------------------------------
capture program drop rebuild_parent_ses
program define rebuild_parent_ses
    capture confirm variable parent_ses_pca
    if !_rc exit

    capture confirm variable parent_party_any
    if _rc {
        capture drop parent_party_any
        gen parent_party_any = .
        replace parent_party_any = 1 if f_party == 1 | m_party == 1
        replace parent_party_any = 0 if (inlist(f_party,0,1) | inlist(m_party,0,1)) ///
            & !(f_party == 1 | m_party == 1)
        replace parent_party_any = . if missing(f_party) & missing(m_party)
        label var parent_party_any "Either parent is CPC member"
    }

    capture drop parent_ses_pca
    pca f_isei m_isei parent_party_any f_edu m_edu ///
        if !missing(f_isei, m_isei, parent_party_any, f_edu, m_edu), components(1)
    predict double parent_ses_pca if e(sample), score
    label var parent_ses_pca "Parent SES PCA score (PC1)"
end

*-------------------------------
* 6. Force English variable labels
*-------------------------------
capture label var child_edu           "Child education"
capture label var child_edu_expect    "Educational expectation"
capture label var child_tutor         "Tutoring"
capture label var child_boarding      "Boarding"
capture label var child_mobile        "Own mobile phone"
capture label var child_internet      "Internet access/use"
capture label var child_sleep         "Sleep duration"
capture label var child_income_rank   "Income rank"
capture label var child_status_rank   "Status rank"
capture label var child_life_sat      "Life satisfaction"
capture label var child_future_conf   "Future confidence"
capture label var child_stress        "Stress"
capture label var child_exercise      "Exercise"
capture label var child_religion      "Religion"
capture label var child_party         "Child CPC"
capture label var child_fulltime      "Full-time work"
capture label var child_parttime      "Part-time work"

*-------------------------------
* 6b. Program: always return English labels for display
*-------------------------------
capture program drop get_en_label
program define get_en_label, rclass
    syntax varname

    local v "`varlist'"
    local lab "`v'"

    if "`v'"=="child_edu"         local lab "Child education"
    if "`v'"=="child_edu_expect"  local lab "Educational expectation"
    if "`v'"=="child_tutor"       local lab "Tutoring"
    if "`v'"=="child_boarding"    local lab "Boarding"
    if "`v'"=="child_mobile"      local lab "Own mobile phone"
    if "`v'"=="child_internet"    local lab "Internet access/use"
    if "`v'"=="child_sleep"       local lab "Sleep duration"
    if "`v'"=="child_income_rank" local lab "Income rank"
    if "`v'"=="child_status_rank" local lab "Status rank"
    if "`v'"=="child_life_sat"    local lab "Life satisfaction"
    if "`v'"=="child_future_conf" local lab "Future confidence"
    if "`v'"=="child_stress"      local lab "Stress"
    if "`v'"=="child_exercise"    local lab "Exercise"
    if "`v'"=="child_religion"    local lab "Religion"
    if "`v'"=="child_party"       local lab "Child CPC"
    if "`v'"=="child_fulltime"    local lab "Full-time work"
    if "`v'"=="child_parttime"    local lab "Part-time work"
    if "`v'"=="child_isei"        local lab "Child occupational status"
    if "`v'"=="parent_ses_pca"    local lab "Parental SES (PCA)"

    return local lab "`lab'"
end

*-------------------------------
* 7. Define mediator groups
*-------------------------------
local med_g1 child_edu child_edu_expect child_tutor child_boarding
local med_g2 child_mobile child_internet child_sleep
local med_g3 child_income_rank child_status_rank child_life_sat child_future_conf child_stress
local med_g4 child_exercise child_religion child_party
local med_g5 child_fulltime child_parttime

local med_all `med_g1' `med_g2' `med_g3' `med_g4' `med_g5'

* placebo mediator
local med_placebo child_sleep child_life_sat child_stress ///
                  child_exercise child_income_rank child_religion ///
                  child_mobile child_internet

* placebo outcome
local y_placebo child_sleep child_life_sat child_stress ///
                child_exercise child_income_rank child_religion

* core mechanism variables
local core_mech child_edu child_edu_expect child_party child_status_rank child_future_conf

*-------------------------------
* 8. Keep only existing mediators
*-------------------------------
local med_use
foreach m of local med_all {
    capture confirm variable `m'
    if !_rc {
        local med_use `med_use' `m'
    }
    else {
        di as txt "Skipped: variable `m' does not exist"
    }
}

di as result "Mediators included in the main mechanism analysis: `med_use'"

*-------------------------------
* 9. Controls
*-------------------------------
local ctrl_base  child_gender child_age child_urban i.child_province i.year
local cmiss_base child_gender child_age child_urban child_province year

local ctrl_short  child_gender child_age
local cmiss_short child_gender child_age

capture confirm variable family_income
local has_finc = (_rc==0)
capture confirm variable family_size
local has_fsize = (_rc==0)

if `has_finc' & `has_fsize' {
    local ctrl_full  child_gender child_age child_urban i.child_province i.year family_income family_size
    local cmiss_full child_gender child_age child_urban child_province year family_income family_size
}
else {
    di as txt "Note: family_income or family_size not found. Full controls revert to base controls."
    local ctrl_full  `ctrl_base'
    local cmiss_full `cmiss_base'
}

*-------------------------------
* 10. Generic mediation program
*-------------------------------
capture program drop med_generic
program define med_generic, rclass
    syntax , XVAR(name) YVAR(name) MVAR(name) CTRLS(string) CMISS(string) [MINN(integer 50)]

    local cmiss2 : subinstr local cmiss " " ", ", all

    tempvar touse
    quietly gen byte `touse' = !missing(`xvar', `yvar', `mvar', `cmiss2')

    quietly count if `touse'
    return scalar N = r(N)

    if r(N) < `minn' {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }

    quietly summarize `mvar' if `touse'
    if missing(r(sd)) | r(sd)==0 {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }

    capture quietly regress `mvar' `xvar' `ctrls' if `touse'
    if _rc {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }
    scalar A = _b[`xvar']

    capture quietly regress `yvar' `xvar' `mvar' `ctrls' if `touse'
    if _rc {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }
    scalar B = _b[`mvar']
    scalar D = _b[`xvar']

    capture quietly regress `yvar' `xvar' `ctrls' if `touse'
    if _rc {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }
    scalar T = _b[`xvar']

    return scalar a        = A
    return scalar b        = B
    return scalar direct   = D
    return scalar total    = T
    return scalar indirect = A*B
end

*-------------------------------
* 10b. Quantile-based mediation program
*-------------------------------
capture program drop med_qreg
program define med_qreg, rclass
    syntax , XVAR(name) YVAR(name) MVAR(name) QVAL(real) CTRLS(string) CMISS(string) [MINN(integer 50)]

    local cmiss2 : subinstr local cmiss " " ", ", all

    tempvar touse
    quietly gen byte `touse' = !missing(`xvar', `yvar', `mvar', `cmiss2')

    quietly count if `touse'
    return scalar N = r(N)

    if r(N) < `minn' {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }

    quietly summarize `mvar' if `touse'
    if missing(r(sd)) | r(sd)==0 {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }

    capture quietly regress `mvar' `xvar' `ctrls' if `touse'
    if _rc {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }
    scalar A = _b[`xvar']

    capture quietly qreg `yvar' `xvar' `mvar' `ctrls' if `touse', q(`qval')
    if _rc {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }
    scalar B = _b[`mvar']
    scalar D = _b[`xvar']

    capture quietly qreg `yvar' `xvar' `ctrls' if `touse', q(`qval')
    if _rc {
        return scalar a        = .
        return scalar b        = .
        return scalar direct   = .
        return scalar total    = .
        return scalar indirect = .
        exit
    }
    scalar T = _b[`xvar']

    return scalar a        = A
    return scalar b        = B
    return scalar direct   = D
    return scalar total    = T
    return scalar indirect = A*B
end

********************************************************************************
* PART I. TABLE 7-1 MAIN MECHANISM RESULTS
********************************************************************************

use `maindata', clear
quietly rebuild_parent_ses

tempfile mechres
postfile hmain ///
    str20 group ///
    str32 mediator ///
    str60 mlabel ///
    double N a b direct total indirect ///
    double lb_indirect ub_indirect ///
    double lb_direct ub_direct ///
    double lb_total ub_total ///
    using `mechres', replace

foreach m of local med_use {

    di as text "--------------------------------------------------"
    di as result "Processing mediator: `m'"
    di as text "--------------------------------------------------"

    local gname ""
    if strpos(" `med_g1' ", " `m' ") local gname "Human capital"
    if strpos(" `med_g2' ", " `m' ") local gname "Information/life"
    if strpos(" `med_g3' ", " `m' ") local gname "Subjective"
    if strpos(" `med_g4' ", " `m' ") local gname "Behavior/social"
    if strpos(" `med_g5' ", " `m' ") local gname "Labor market"

    quietly get_en_label `m'
    local lab "`r(lab)'"

    quietly med_generic, ///
        xvar(parent_ses_pca) yvar(child_isei) mvar(`m') ///
        ctrls("`ctrl_base'") cmiss("`cmiss_base'")

    local N0        = r(N)
    local a0        = r(a)
    local b0        = r(b)
    local direct0   = r(direct)
    local total0    = r(total)
    local indirect0 = r(indirect)

    if missing(`indirect0') {
        di as error "Mediator `m' has insufficient sample size or no variation. Bootstrap skipped."
        post hmain ("`gname'") ("`m'") ("`lab'") ///
            (`N0') (`a0') (`b0') (`direct0') (`total0') (`indirect0') ///
            (.) (.) (.) (.) (.) (.)
        continue
    }

    capture noisily bootstrap ///
        r(indirect) r(direct) r(total), ///
        reps($BREP) seed($SEED) nodots: ///
        med_generic, ///
        xvar(parent_ses_pca) yvar(child_isei) mvar(`m') ///
        ctrls("`ctrl_base'") cmiss("`cmiss_base'")

    if _rc {
        di as error "Bootstrap failed for mediator `m'. Only point estimates are recorded."
        post hmain ("`gname'") ("`m'") ("`lab'") ///
            (`N0') (`a0') (`b0') (`direct0') (`total0') (`indirect0') ///
            (.) (.) (.) (.) (.) (.)
        continue
    }

    matrix BB = r(table)
    local lb_ind = BB[5,1]
    local ub_ind = BB[6,1]
    local lb_dir = BB[5,2]
    local ub_dir = BB[6,2]
    local lb_tot = BB[5,3]
    local ub_tot = BB[6,3]

    post hmain ("`gname'") ("`m'") ("`lab'") ///
        (`N0') (`a0') (`b0') (`direct0') (`total0') (`indirect0') ///
        (`lb_ind') (`ub_ind') ///
        (`lb_dir') (`ub_dir') ///
        (`lb_tot') (`ub_tot')
}
postclose hmain

use `mechres', clear
order group mediator mlabel N a b direct total indirect ///
      lb_indirect ub_indirect lb_direct ub_direct lb_total ub_total

gen sig_indirect = (lb_indirect>0 & ub_indirect>0) | (lb_indirect<0 & ub_indirect<0) ///
    if !missing(lb_indirect, ub_indirect)
label var sig_indirect "Indirect effect significant (bootstrap CI excludes 0)"

gen pm_share = indirect / total if !missing(indirect, total) & total != 0
label var pm_share "Proportion mediated = indirect / total"

format N %9.0f
format a b direct total indirect lb_indirect ub_indirect lb_direct ub_direct lb_total ub_total pm_share %9.4f

sort group mediator
export delimited using "${OUT}\tab\7_1_main_mechanism_results.csv", replace



*-------------------------------
* Table 7-1b: Largest mediator by SES class (quantile class)
*-------------------------------
use `maindata', clear
quietly rebuild_parent_ses

local class_qs "25 50 75"
tempfile qclassres
postfile hqclass ///
    str18 class ///
    byte q ///
    str32 mediator ///
    str60 mlabel ///
    double N a b direct total indirect abs_indirect ///
    using `qclassres', replace

foreach qq of local class_qs {
    local qv = `qq'/100
    local cname = "Q`qq'"
    if `qq' == 25 local cname = "Lower class (Q25)"
    if `qq' == 50 local cname = "Middle class (Q50)"
    if `qq' == 75 local cname = "Upper class (Q75)"

    foreach m of local med_use {
        quietly get_en_label `m'
        local lab "`r(lab)'"

        quietly med_qreg, ///
            xvar(parent_ses_pca) yvar(child_isei) mvar(`m') qval(`qv') ///
            ctrls("`ctrl_base'") cmiss("`cmiss_base'")

        local N0        = r(N)
        local a0        = r(a)
        local b0        = r(b)
        local direct0   = r(direct)
        local total0    = r(total)
        local indirect0 = r(indirect)
        local absind0   = .
        if !missing(`indirect0') local absind0 = abs(`indirect0')

        post hqclass ("`cname'") (`qq') ("`m'") ("`lab'") ///
            (`N0') (`a0') (`b0') (`direct0') (`total0') (`indirect0') (`absind0')
    }
}
postclose hqclass

use `qclassres', clear
gsort class -abs_indirect mediator
export delimited using "${OUT}\tab\7_1b_quantile_class_mediation_all.csv", replace

preserve
keep if !missing(abs_indirect)
gsort q -abs_indirect mediator
by q: keep if _n == 1
sort q
format N %9.0f
format a b direct total indirect abs_indirect %9.4f
export delimited using "${OUT}\tab\7_1b_quantile_class_top_mediator.csv", replace

putdocx clear
putdocx begin, pagesize(A4) font("Times New Roman", 10)

putdocx paragraph, halign(center)
putdocx text ("Largest mediator by SES class (quantile mediation)"), bold

putdocx paragraph
putdocx table tab1b = data(class q mlabel indirect abs_indirect N), ///
    varnames border(all, nil)

putdocx table tab1b(1,.), border(top, single, black, 1.2pt)
putdocx table tab1b(1,.), border(bottom, single, black, 0.8pt)
count
local row_count1b = r(N) + 1
putdocx table tab1b(`row_count1b',.), border(bottom, single, black, 1.2pt)

putdocx table tab1b(1,.), bold
putdocx table tab1b(.,1/3), halign(left)
putdocx table tab1b(.,4/6), halign(center)
putdocx table tab1b(.,.), font("Times New Roman", 9)

putdocx paragraph
putdocx paragraph, halign(left)
putdocx text ("Notes: "), italic bold
putdocx text ("Class is defined by child ISEI conditional quantiles (Q25, Q50, Q75). "), italic
putdocx text ("The largest mediator is selected by the absolute indirect effect |a*b| within each class."), italic

putdocx save "${OUT}\tab\7_1b_quantile_class_top_mediator.docx", replace
restore

********************************************************************************
* PART II. TABLE 7-2 VALIDATION AND ROBUSTNESS OF MECHANISM ANALYSIS
********************************************************************************

tempfile big2
postfile hbig ///
    str28 panel ///
    str25 testtype ///
    str60 item ///
    str18 nspec ///
    str60 main_result ///
    str60 add_result ///
    using `big2', replace

*--------------------------------
* Panel A: Placebo mediators
*--------------------------------
use `maindata', clear
quietly rebuild_parent_ses

foreach m of local med_placebo {
    capture confirm variable `m'
    if _rc continue

    quietly get_en_label `m'
    local lab "`r(lab)'"

    quietly med_generic, ///
        xvar(parent_ses_pca) yvar(child_isei) mvar(`m') ///
        ctrls("`ctrl_base'") cmiss("`cmiss_base'")

    local N0 = r(N)
    local I0 = r(indirect)
    local D0 = r(direct)
    local T0 = r(total)

    local mr = cond(missing(`I0'), "Indirect=.", "Indirect=" + string(`I0',"%9.3f"))
    local ar = cond(missing(`D0'), "Direct=.; Total=.", ///
        "Direct=" + string(`D0',"%9.3f") + "; Total=" + string(`T0',"%9.3f"))

    post hbig ("Panel A") ("Placebo mediator") ("`lab'") ///
        (string(`N0',"%9.0f")) ("`mr'") ("`ar'")
}

*--------------------------------
* Panel B: Placebo outcomes
*--------------------------------
use `maindata', clear
quietly rebuild_parent_ses

foreach y of local y_placebo {
    capture confirm variable `y'
    if _rc continue

    quietly get_en_label `y'
    local ylab "`r(lab)'"

    capture noisily regress `y' parent_ses_pca ///
        child_gender child_age child_urban i.child_province i.year ///
        if !missing(`y', parent_ses_pca, child_gender, child_age, child_urban, child_province, year), ///
        cluster(child_province)

    if _rc continue

    local bb = _b[parent_ses_pca]
    local ss = _se[parent_ses_pca]
    local pp = 2*ttail(e(df_r), abs(`bb'/`ss'))
    local nn = e(N)

    local mr = "Coef=" + string(`bb',"%9.3f") + " (SE=" + string(`ss',"%9.3f") + ")"
    local ar = "p=" + string(`pp',"%9.3f")

    post hbig ("Panel B") ("Placebo outcome") ("`ylab'") ///
        (string(`nn',"%9.0f")) ("`mr'") ("`ar'")
}

*--------------------------------
* Panel C: Temporal mediation
*--------------------------------
use "${POUT}\Panel_data.dta", clear
quietly rebuild_parent_ses
xtset pid year, delta(2)

capture drop L1_parent_ses
capture drop F1_child_isei
gen L1_parent_ses = L.parent_ses_pca
gen F1_child_isei = F.child_isei

foreach m of local core_mech {
    capture confirm variable `m'
    if _rc continue

    quietly get_en_label `m'
    local lab "`r(lab)'"

    quietly med_generic, ///
        xvar(L1_parent_ses) yvar(F1_child_isei) mvar(`m') ///
        ctrls("`ctrl_base'") cmiss("`cmiss_base'")

    local N0 = r(N)
    local I0 = r(indirect)
    local D0 = r(direct)
    local T0 = r(total)

    local mr = cond(missing(`I0'), "Indirect=.", "Indirect=" + string(`I0',"%9.3f"))
    local ar = cond(missing(`D0'), "Direct=.; Total=.", ///
        "Direct=" + string(`D0',"%9.3f") + "; Total=" + string(`T0',"%9.3f"))

    post hbig ("Panel C") ("Temporal mediation") ("`lab'") ///
        (string(`N0',"%9.0f")) ("`mr'") ("`ar'")
}

*--------------------------------
* Panel D: Nested controls
*--------------------------------
use `maindata', clear
quietly rebuild_parent_ses

foreach m of local core_mech {
    capture confirm variable `m'
    if _rc continue

    quietly get_en_label `m'
    local lab "`r(lab)'"

    quietly med_generic, ///
        xvar(parent_ses_pca) yvar(child_isei) mvar(`m') ///
        ctrls("`ctrl_short'") cmiss("`cmiss_short'")
    local I_s = r(indirect)

    quietly med_generic, ///
        xvar(parent_ses_pca) yvar(child_isei) mvar(`m') ///
        ctrls("`ctrl_base'") cmiss("`cmiss_base'")
    local I_b = r(indirect)

    quietly med_generic, ///
        xvar(parent_ses_pca) yvar(child_isei) mvar(`m') ///
        ctrls("`ctrl_full'") cmiss("`cmiss_full'")
    local I_f = r(indirect)

    local mr = "Short=" + cond(missing(`I_s'),".",string(`I_s',"%9.3f")) + ///
               "; Base=" + cond(missing(`I_b'),".",string(`I_b',"%9.3f"))
    local ar = "Full=" + cond(missing(`I_f'),".",string(`I_f',"%9.3f"))

    post hbig ("Panel D") ("Nested controls") ("`lab'") ///
        ("3 specs") ("`mr'") ("`ar'")
}

*--------------------------------
* Panel E: Leave-one-wave-out
*--------------------------------
use `maindata', clear
quietly rebuild_parent_ses

tempfile lwraw
postfile hlw ///
    str32 mediator ///
    double omit_year indirect ///
    using `lwraw', replace

levelsof year, local(waves)

foreach yy of local waves {
    foreach m of local core_mech {
        capture confirm variable `m'
        if _rc continue

        preserve
            keep if year != `yy'
            quietly med_generic, ///
                xvar(parent_ses_pca) yvar(child_isei) mvar(`m') ///
                ctrls("`ctrl_base'") cmiss("`cmiss_base'")

            if !missing(r(indirect)) {
                post hlw ("`m'") (`yy') (r(indirect))
            }
        restore
    }
}
postclose hlw

use `lwraw', clear
collapse (count) nrun=indirect ///
         (mean) mean_ind=indirect ///
         (min) min_ind=indirect ///
         (max) max_ind=indirect, by(mediator)

tempfile lwsum
save `lwsum', replace

use `maindata', clear
quietly rebuild_parent_ses

foreach m of local core_mech {
    preserve
        use `lwsum', clear
        keep if mediator == "`m'"
        count
        if r(N) == 1 {
            local nrun = nrun[1]
            local mn   = mean_ind[1]
            local mi   = min_ind[1]
            local mx   = max_ind[1]
        }
        else {
            local nrun = .
            local mn   = .
            local mi   = .
            local mx   = .
        }
    restore

    quietly get_en_label `m'
    local lab "`r(lab)'"

    local mr = cond(missing(`mn'), "Mean=.", "Mean=" + string(`mn',"%9.3f"))
    local ar = cond(missing(`mi') | missing(`mx'), "[., .]", ///
        "[" + string(`mi',"%9.3f") + ", " + string(`mx',"%9.3f") + "]")

    post hbig ("Panel E") ("Leave-one-wave-out") ("`lab'") ///
        (cond(missing(`nrun'), ".", string(`nrun',"%9.0f")) + " omits") ///
        ("`mr'") ("`ar'")
}

postclose hbig

*============================================================*
* Table 7-2: organize display dataset
*============================================================*
use `big2', clear

gen porder = .
replace porder = 1 if panel=="Panel A"
replace porder = 2 if panel=="Panel B"
replace porder = 3 if panel=="Panel C"
replace porder = 4 if panel=="Panel D"
replace porder = 5 if panel=="Panel E"

sort porder item
drop porder

rename panel       Panel
rename testtype    Test
rename item        Item
rename nspec       Nspec
rename main_result Main_result
rename add_result  Additional_result

replace Panel = "A. Placebo mediators" if Panel=="Panel A"
replace Panel = "B. Placebo outcomes" if Panel=="Panel B"
replace Panel = "C. Temporal mediation" if Panel=="Panel C"
replace Panel = "D. Nested controls" if Panel=="Panel D"
replace Panel = "E. Leave-one-wave-out" if Panel=="Panel E"

sort Panel Item
by Panel: replace Panel = "" if _n > 1

label var Panel             "Panel"
label var Test              "Test type"
label var Item              "Variable / mechanism"
label var Nspec             "N / spec"
label var Main_result       "Main result"
label var Additional_result "Additional result"

export delimited using "${OUT}\tab\7_2_validation_robustness_display.csv", replace

*============================================================*
* Table 7-2: export publication-style Word table
*============================================================*
count
local row_count = r(N) + 1

putdocx clear
putdocx begin, pagesize(A4) landscape font("Times New Roman", 8.5)

putdocx paragraph, halign(center)
putdocx text ("Table 7-2. Validation and robustness of mechanism analysis"), bold

putdocx paragraph

putdocx table tab2 = data(Panel Test Item Nspec Main_result Additional_result), ///
    varnames border(all, nil)

putdocx table tab2(1,.), border(top, single, black, 1.2pt)
putdocx table tab2(1,.), border(bottom, single, black, 0.8pt)
putdocx table tab2(`row_count',.), border(bottom, single, black, 1.2pt)

putdocx table tab2(1,.), bold
putdocx table tab2(.,1/3), halign(left)
putdocx table tab2(.,4/6), halign(center)
putdocx table tab2(.,.), font("Times New Roman", 8)

putdocx paragraph
putdocx paragraph, halign(left)
putdocx text ("Notes: "), italic bold
putdocx text ("Panel A reports placebo mediator tests. Panel B reports placebo outcome regressions. "), italic
putdocx text ("Panel C reports temporal mediation using lagged parental SES and lead child occupational status. "), italic
putdocx text ("Panel D reports indirect effects under nested control sets. "), italic
putdocx text ("Panel E reports leave-one-wave-out summaries using the mean and range of indirect effects."), italic

putdocx save "${OUT}\tab\7_2_validation_robustness_bigtable.docx", replace

log close
