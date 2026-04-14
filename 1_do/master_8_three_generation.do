********************************************************************************
* Three-generation extension analysis
* Topic: Grandparent SES and Grandchild Occupational Choice
* Based on standardized Panel_data.dta
* Latest updated: 2026/03/29
********************************************************************************

clear all
set more off

*-------------------------------
* 1. Path and environment setup
*-------------------------------
global PROJ "D:\一个文件夹\学习\学习\hku\8001\CFPS Project"
global POUT  "${PROJ}\3_output\panel"
global OUT   "${PROJ}\3_output"
global LOG   "${PROJ}\4_log"

* Automatically create folders
foreach dir in "${OUT}" "${OUT}\tab" "${OUT}\fig" "${LOG}" {
    cap mkdir "`dir'"
}

cap log close
local t = subinstr("`c(current_time)'", ":", "_", .)
log using "${LOG}\analysis_three_generation_`c(current_date)'_`t'.log", replace text

set scheme s2color
graph set window fontface "Arial"

* Check required packages
foreach pkg in estout {
    capture which `pkg'
    if _rc ssc install `pkg', replace
}

di "============================================================"
di "Three-generation analysis starts"
di "============================================================"

********************************************************************************
* 2. Load the main dataset
********************************************************************************
use "${POUT}\Panel_data.dta", clear

keep if inlist(year, 2010, 2012, 2014, 2016, 2018, 2020, 2022)

di "Current sample size in panel data: " _N

*--------------------------------------------------
* Check required variables
*--------------------------------------------------
local must_have ///
    pid year father_pid mother_pid ///
    child_isei child_edu child_party ///
    child_gender child_age child_urban child_province ///
    f_isei m_isei f_edu m_edu f_party m_party

foreach v of local must_have {
    capture confirm variable `v'
    if _rc {
        di as error "Missing required variable: `v'"
        log close
        exit 198
    }
}

tempfile panel_all
save `panel_all', replace


********************************************************************************
* 3. Basic cleaning
********************************************************************************
use `panel_all', clear

* Uniformly recode clearly invalid negative values as missing
local cleanvars ///
    child_isei child_edu child_party child_gender child_age child_urban child_province ///
    f_isei m_isei f_edu m_edu f_party m_party

foreach v of local cleanvars {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
    }
}

* Normalize party-membership variables to 0/1 if they are not already coded that way
capture replace child_party = 1 if child_party > 1 & !missing(child_party)
capture replace f_party     = 1 if f_party > 1 & !missing(f_party)
capture replace m_party     = 1 if m_party > 1 & !missing(m_party)

tempfile panel_clean
save `panel_clean', replace


********************************************************************************
* 4. Construct the "person identity base" (person_base)
* Notes:
* In the standardized panel, each row's child_* variables are essentially
* the respondent's own information.
* Therefore, we directly use child_* to build each person's SES base.
********************************************************************************
use `panel_clean', clear

keep pid year father_pid mother_pid ///
     child_isei child_edu child_party ///
     child_gender child_age child_urban child_province

rename child_isei     self_isei
rename child_edu      self_edu
rename child_party    self_party
rename child_gender   self_gender
rename child_age      self_age
rename child_urban    self_urban
rename child_province self_province

* The same pid may appear in multiple years
* Keep the most informative and most recent record
gen byte info_score = !missing(self_isei) + !missing(self_edu) + !missing(self_party)
gsort pid -info_score -year
by pid: keep if _n == 1
drop info_score

duplicates drop pid, force

tempfile person_base
save `person_base', replace

di "Person base built. Unique persons = " _N


********************************************************************************
* 5. Construct the child-generation sample for the three-generation analysis
* Here, the child generation refers to the respondents originally analyzed in the panel
********************************************************************************
use `panel_clean', clear

keep pid year father_pid mother_pid ///
     child_isei child_gender child_age child_urban child_province ///
     f_isei m_isei f_edu m_edu f_party m_party

* Keep only observations with non-missing key child outcome variable
keep if !missing(pid, child_isei)

* Availability of parent IDs
gen has_father_id = !missing(father_pid)
gen has_mother_id = !missing(mother_pid)

tempfile child_sample
save `child_sample', replace

di "Child sample built. N = " _N

********************************************************************************
* 6. Construct father base / mother base / grandparent base
* Notes:
* person_base already contains pid father_pid mother_pid self_isei self_edu self_party
* Therefore, we first rename the original father_pid/mother_pid and then
* rename pid as the matching key
********************************************************************************

*-------------------------------
* 6.1 Father matching base
*-------------------------------
use `person_base', clear
rename father_pid father_father_pid
rename mother_pid father_mother_pid
rename pid        father_pid
rename self_isei  father_self_isei
rename self_edu   father_self_edu
rename self_party father_self_party
tempfile father_base
save `father_base', replace

*-------------------------------
* 6.2 Mother matching base
*-------------------------------
use `person_base', clear
rename father_pid mother_father_pid
rename mother_pid mother_mother_pid
rename pid        mother_pid
rename self_isei  mother_self_isei
rename self_edu   mother_self_edu
rename self_party mother_self_party
tempfile mother_base
save `mother_base', replace

*-------------------------------
* 6.3 General grandparent matching base
*-------------------------------
use `person_base', clear
rename pid        gp_pid
rename father_pid gp_father_pid
rename mother_pid gp_mother_pid
rename self_isei  gp_isei
rename self_edu   gp_edu
rename self_party gp_party
tempfile gp_base
save `gp_base', replace


********************************************************************************
* 7. Match parents and grandparents to child records
********************************************************************************
use `child_sample', clear

*-------------------------------
* 7.1 Match father's own record
*-------------------------------
merge m:1 father_pid using `father_base', keep(1 3) nogen

gen has_father_record = ///
    !missing(father_self_isei) | !missing(father_self_edu) | ///
    !missing(father_father_pid) | !missing(father_mother_pid)

*-------------------------------
* 7.2 Match mother's own record
*-------------------------------
merge m:1 mother_pid using `mother_base', keep(1 3) nogen

gen has_mother_record = ///
    !missing(mother_self_isei) | !missing(mother_self_edu) | ///
    !missing(mother_father_pid) | !missing(mother_mother_pid)

di "After matching parent personal records:"
count if has_father_record == 1
di "Has father personal record = " r(N)
count if has_mother_record == 1
di "Has mother personal record = " r(N)

*-------------------------------
* 7.3 Match paternal grandfather
*-------------------------------
gen pgf_pid = father_father_pid
rename pgf_pid gp_pid
merge m:1 gp_pid using `gp_base', keep(1 3) nogen keepusing(gp_isei gp_edu gp_party)
rename gp_pid   pgf_pid
rename gp_isei  pgf_isei
rename gp_edu   pgf_edu
rename gp_party pgf_party

*-------------------------------
* 7.4 Match paternal grandmother
*-------------------------------
gen pgm_pid = father_mother_pid
rename pgm_pid gp_pid
merge m:1 gp_pid using `gp_base', keep(1 3) nogen keepusing(gp_isei gp_edu gp_party)
rename gp_pid   pgm_pid
rename gp_isei  pgm_isei
rename gp_edu   pgm_edu
rename gp_party pgm_party

*-------------------------------
* 7.5 Match maternal grandfather
*-------------------------------
gen mgf_pid = mother_father_pid
rename mgf_pid gp_pid
merge m:1 gp_pid using `gp_base', keep(1 3) nogen keepusing(gp_isei gp_edu gp_party)
rename gp_pid   mgf_pid
rename gp_isei  mgf_isei
rename gp_edu   mgf_edu
rename gp_party mgf_party

*-------------------------------
* 7.6 Match maternal grandmother
*-------------------------------
gen mgm_pid = mother_mother_pid
rename mgm_pid gp_pid
merge m:1 gp_pid using `gp_base', keep(1 3) nogen keepusing(gp_isei gp_edu gp_party)
rename gp_pid   mgm_pid
rename gp_isei  mgm_isei
rename gp_edu   mgm_edu
rename gp_party mgm_party


*-------------------------------
* 7.7 Coverage of grandparent information
*-------------------------------
gen has_any_grandparent = ///
    !missing(pgf_isei) | !missing(pgm_isei) | !missing(mgf_isei) | !missing(mgm_isei) | ///
    !missing(pgf_edu)  | !missing(pgm_edu)  | !missing(mgf_edu)  | !missing(mgm_edu)

tempfile threegen_raw
save `threegen_raw', replace

count if has_any_grandparent == 1
di "Has at least one grandparent record = " r(N)

********************************************************************************
* 8. Construct parental SES and grandparent SES
* Given the relatively small sample size, a simplified approach is used here:
* standardize each component and then take the mean
********************************************************************************
use `threegen_raw', clear

*--------------------------------------------------
* 8.1 Parent party-membership dummy
*--------------------------------------------------
capture drop parent_party_any
gen byte parent_party_any = (f_party > 0 | m_party > 0) if !missing(f_party) | !missing(m_party)

*--------------------------------------------------
* 8.2 Grandparent party-membership dummy
*--------------------------------------------------
foreach v in pgf_party pgm_party mgf_party mgm_party {
    capture replace `v' = 1 if `v' > 1 & !missing(`v')
}

egen gp_party_any = rowmax(pgf_party pgm_party mgf_party mgm_party)

*--------------------------------------------------
* 8.3 Parent SES: mean of standardized components
*--------------------------------------------------
capture drop z_f_isei z_m_isei z_f_edu z_m_edu z_parent_party
egen z_f_isei      = std(f_isei)
egen z_m_isei      = std(m_isei)
egen z_f_edu       = std(f_edu)
egen z_m_edu       = std(m_edu)
egen z_parent_party = std(parent_party_any)

egen parent_ses = rowmean(z_f_isei z_m_isei z_f_edu z_m_edu z_parent_party)

*--------------------------------------------------
* 8.4 Grandparent SES: mean of standardized components
*--------------------------------------------------
capture drop z_pgf_isei z_pgm_isei z_mgf_isei z_mgm_isei
capture drop z_pgf_edu z_pgm_edu z_mgf_edu z_mgm_edu z_gp_party

egen z_pgf_isei = std(pgf_isei)
egen z_pgm_isei = std(pgm_isei)
egen z_mgf_isei = std(mgf_isei)
egen z_mgm_isei = std(mgm_isei)

egen z_pgf_edu  = std(pgf_edu)
egen z_pgm_edu  = std(pgm_edu)
egen z_mgf_edu  = std(mgf_edu)
egen z_mgm_edu  = std(mgm_edu)

egen z_gp_party = std(gp_party_any)

egen grandparent_ses = rowmean( ///
    z_pgf_isei z_pgm_isei z_mgf_isei z_mgm_isei ///
    z_pgf_edu  z_pgm_edu  z_mgf_edu  z_mgm_edu ///
    z_gp_party )

*--------------------------------------------------
* 8.5 Count the coverage of grandparent information
*--------------------------------------------------
egen gp_nonmiss_isei = rownonmiss(pgf_isei pgm_isei mgf_isei mgm_isei)
egen gp_nonmiss_edu  = rownonmiss(pgf_edu  pgm_edu  mgf_edu  mgm_edu)

gen gp_nonmiss_total = gp_nonmiss_isei + gp_nonmiss_edu

*--------------------------------------------------
* 8.6 Core analysis sample
*--------------------------------------------------
gen threegen_core = !missing(child_isei, parent_ses, grandparent_ses, ///
                             child_gender, child_age, child_urban, child_province)

keep if threegen_core == 1

tempfile threegen_analysis
save `threegen_analysis', replace

di "============================================================"
di "Three-generation analysis sample built"
count
di "Final N = " r(N)
sum child_isei parent_ses grandparent_ses
di "============================================================"


********************************************************************************
* 9. Sample flow
********************************************************************************
use `threegen_raw', clear

gen stage1 = !missing(child_isei)
gen stage2 = stage1 & (has_father_record==1 | has_mother_record==1)
gen stage3 = stage2 & has_any_grandparent==1

count if stage1
local n1 = r(N)

count if stage2
local n2 = r(N)

count if stage3
local n3 = r(N)

use `threegen_analysis', clear
count
local n4 = r(N)

clear
set obs 4
gen str50 stage = ""
gen N = .

replace stage = "1. Child sample with child ISEI" in 1
replace N = `n1' in 1

replace stage = "2. Linked to at least one parent record" in 2
replace N = `n2' in 2

replace stage = "3. Linked to at least one grandparent record" in 3
replace N = `n3' in 3

replace stage = "4. Final three-generation analysis sample" in 4
replace N = `n4' in 4

export delimited using "${OUT}\tab\8_1_three_generation_sample_flow.csv", replace


********************************************************************************
* 10. Descriptive statistics
********************************************************************************
use `threegen_analysis', clear

label var child_isei        "Child ISEI"
label var parent_ses        "Parent SES"
label var grandparent_ses   "Grandparent SES"
label var child_gender      "Male"
label var child_age         "Age"
label var child_urban       "Urban"
label var gp_nonmiss_isei   "Number of grandparents with ISEI"
label var gp_nonmiss_edu    "Number of grandparents with education"
label var gp_nonmiss_total  "Grandparent information count"

est clear
estpost summarize ///
    child_isei parent_ses grandparent_ses ///
    child_gender child_age child_urban ///
    gp_nonmiss_isei gp_nonmiss_edu gp_nonmiss_total

esttab using "${OUT}\tab\8_2_three_generation_descriptive.rtf", replace ///
    cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) count(fmt(0))") ///
    label nonumber noobs nomtitle ///
    collabels("Mean" "SD" "Min" "Max" "N") ///
    title("Descriptive Statistics: Three-generation Sample")


********************************************************************************
* 11. Simple figure
********************************************************************************
use `threegen_analysis', clear

twoway ///
    (scatter child_isei grandparent_ses, ///
        mcolor(navy%25) msymbol(o) msize(medsmall) ///
        legend(label(1 "Child ISEI"))) ///
    (lfit child_isei grandparent_ses, ///
        lcolor(maroon) lwidth(medthick) ///
        legend(label(2 "Fitted line"))), ///
    title("Child ISEI and Grandparent SES", size(large)) ///
    xtitle("Grandparent SES", size(medlarge)) ///
    ytitle("Child ISEI", size(medlarge)) ///
    legend(order(1 2) rows(1) pos(6) ring(0) size(medlarge)) ///
    graphregion(fcolor(white)) ///
    plotregion(margin(small))

graph export "${OUT}\fig\8_1_scatter_childisei_grandparent_ses.png", replace width(2400)


********************************************************************************
* 12. Baseline regressions
********************************************************************************
use `threegen_analysis', clear

label var grandparent_ses "Grandparent SES"
label var parent_ses      "Parent SES"
label var child_gender    "Male"
label var child_age       "Age"
label var child_urban     "Urban"

est clear

quietly reg child_isei grandparent_ses, vce(robust)
est store m1
estadd local controls "No"
estadd local yearfe   "No"
estadd local provfe   "No"

quietly reg child_isei grandparent_ses parent_ses, vce(robust)
est store m2
estadd local controls "No"
estadd local yearfe   "No"
estadd local provfe   "No"

quietly reg child_isei grandparent_ses parent_ses ///
    child_gender child_age child_urban i.year, vce(robust)
est store m3
estadd local controls "Yes"
estadd local yearfe   "Yes"
estadd local provfe   "No"

quietly reg child_isei grandparent_ses parent_ses ///
    child_gender child_age child_urban i.year i.child_province, vce(robust)
est store m4
estadd local controls "Yes"
estadd local yearfe   "Yes"
estadd local provfe   "Yes"

esttab m1 m2 m3 m4 using "${OUT}\tab\8_3_three_generation_regression.rtf", replace ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(grandparent_ses parent_ses child_gender child_age child_urban) ///
    order(grandparent_ses parent_ses child_gender child_age child_urban) ///
    label compress nonumbers ///
    mtitle("GP SES" "+ Parent SES" "+ Controls" "+ Province FE") ///
    stats(controls yearfe provfe N r2, ///
          fmt(%s %s %s %12.0fc %9.3f) ///
          labels("Controls" "Year FE" "Province FE" "Observations" "R-squared")) ///
    title("Three-generation Regression Results")



********************************************************************************
* 13. Decomposition by each grandparent
********************************************************************************
use `threegen_analysis', clear

capture drop pgf_ses_raw pgm_ses_raw mgf_ses_raw mgm_ses_raw
capture drop pgf_ses pgm_ses mgf_ses mgm_ses

egen pgf_ses_raw = rowmean(pgf_isei pgf_edu)
egen pgm_ses_raw = rowmean(pgm_isei pgm_edu)
egen mgf_ses_raw = rowmean(mgf_isei mgf_edu)
egen mgm_ses_raw = rowmean(mgm_isei mgm_edu)

egen pgf_ses = std(pgf_ses_raw)
egen pgm_ses = std(pgm_ses_raw)
egen mgf_ses = std(mgf_ses_raw)
egen mgm_ses = std(mgm_ses_raw)

label var pgf_ses      "Paternal grandfather SES"
label var pgm_ses      "Paternal grandmother SES"
label var mgf_ses      "Maternal grandfather SES"
label var mgm_ses      "Maternal grandmother SES"
label var parent_ses   "Parent SES"
label var child_gender "Male"
label var child_age    "Age"
label var child_urban  "Urban"

est clear

quietly reg child_isei pgf_ses parent_ses child_gender child_age child_urban i.year, vce(robust)
est store g1
estadd local gp "Paternal grandfather"
estadd local yearfe "Yes"
estadd local provfe "No"

quietly reg child_isei pgm_ses parent_ses child_gender child_age child_urban i.year, vce(robust)
est store g2
estadd local gp "Paternal grandmother"
estadd local yearfe "Yes"
estadd local provfe "No"

quietly reg child_isei mgf_ses parent_ses child_gender child_age child_urban i.year, vce(robust)
est store g3
estadd local gp "Maternal grandfather"
estadd local yearfe "Yes"
estadd local provfe "No"

quietly reg child_isei mgm_ses parent_ses child_gender child_age child_urban i.year, vce(robust)
est store g4
estadd local gp "Maternal grandmother"
estadd local yearfe "Yes"
estadd local provfe "No"

esttab g1 g2 g3 g4 using "${OUT}\tab\8_4_each_grandparent.rtf", replace ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(pgf_ses pgm_ses mgf_ses mgm_ses parent_ses child_gender child_age child_urban) ///
    order(pgf_ses pgm_ses mgf_ses mgm_ses parent_ses child_gender child_age child_urban) ///
    label compress nonumbers ///
    mtitle("(1)" "(2)" "(3)" "(4)") ///
    stats(gp yearfe provfe N r2, ///
          fmt(%s %s %s %12.0fc %9.3f) ///
          labels("Grandparent" "Year FE" "Province FE" "Observations" "R-squared")) ///
    title("Decomposition by Each Grandparent")


********************************************************************************
* 14. Coefficient plot for the four grandparents
********************************************************************************	

coefplot ///
    (g1, keep(pgf_ses) label("PGF")) ///
    (g2, keep(pgm_ses) label("PGM")) ///
    (g3, keep(mgf_ses) label("MGF")) ///
    (g4, keep(mgm_ses) label("MGM")), ///
    vertical ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    msymbol(O) msize(medium) mcolor(navy) ///
    ciopts(recast(rcap) lcolor(navy) lwidth(medthin)) ///
    coeflabels( ///
        pgf_ses = "PGF" ///
        pgm_ses = "PGM" ///
        mgf_ses = "MGF" ///
        mgm_ses = "MGM") ///
    ylabel(0(2)12, labsize(medium) angle(0)) ///
    yscale(range(-1 13)) ///
    xlabel(, labsize(large)) ///
    legend(off) ///
    title("Grandparent-specific Effects on Child ISEI", size(large)) ///
    ytitle("Coefficient estimate", size(medlarge) margin(medium)) ///
    xtitle("") ///
    note( ///
        "PGF = paternal grandfather; PGM = paternal grandmother;" ///
        "MGF = maternal grandfather; MGM = maternal grandmother", ///
        size(vsmall) span justification(center)) ///
	graphregion(margin(medium vlarge medium medium) fcolor(white)) ///
    plotregion(margin(medium))

graph export "${OUT}\fig\8_2_coefplot_each_grandparent.png", replace width(2800)

********************************************************************************
* 15. Simple robustness checks
********************************************************************************
use `threegen_analysis', clear

label var grandparent_ses "Grandparent SES"
label var parent_ses      "Parent SES"
label var child_gender    "Male"
label var child_age       "Age"
label var child_urban     "Urban"

est clear

quietly reg child_isei grandparent_ses parent_ses ///
    child_gender child_age child_urban i.year ///
    if gp_nonmiss_total >= 2, vce(robust)
est store r1
estadd local sample  "GP info >= 2"
estadd local yearfe  "Yes"
estadd local provfe  "No"

quietly reg child_isei grandparent_ses parent_ses ///
    child_gender child_age i.year ///
    if child_urban == 1, vce(robust)
est store r2
estadd local sample  "Urban only"
estadd local yearfe  "Yes"
estadd local provfe  "No"

quietly reg child_isei grandparent_ses parent_ses ///
    child_gender child_age i.year ///
    if child_urban == 0, vce(robust)
est store r3
estadd local sample  "Rural only"
estadd local yearfe  "Yes"
estadd local provfe  "No"

esttab r1 r2 r3 using "${OUT}\tab\8_5_three_generation_robustness.rtf", replace ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(grandparent_ses parent_ses child_gender child_age child_urban) ///
    order(grandparent_ses parent_ses child_gender child_age child_urban) ///
    label compress nonumbers ///
	mtitle("GP info >=2" "Urban" "Rural") ///
    stats(sample yearfe provfe N r2, ///
          fmt(%s %s %s %12.0fc %9.3f) ///
          labels("Sample" "Year FE" "Province FE" "Observations" "R-squared")) ///
    title("Robustness Checks")


********************************************************************************
* 16. Save the three-generation analysis sample
********************************************************************************
use `threegen_analysis', clear
compress
save "${POUT}\three_generation_analysis_sample.dta", replace

di "Saved: ${POUT}\three_generation_analysis_sample.dta"


********************************************************************************
* 17. End
********************************************************************************
di "============================================================"
di "Three-generation analysis completed successfully."
di "Output:"
di "  8_1_three_generation_sample_flow.csv"
di "  8_2_three_generation_descriptive.rtf"
di "  8_3_three_generation_regression.rtf"
di "  8_4_each_grandparent.rtf"
di "  8_5_three_generation_robustness.rtf"
di "============================================================"

log close
