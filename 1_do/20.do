/*==================================================
  CFPS 2020 Intergenerational Matching + Outlier Cleaning
  Family database already merged
  First Updated on Feb 21st
  Latest Updated on April 9th
==================================================*/

local wave 2020

*--- Raw data paths
local adult   "${RAW}\cfps2020person_202306.dta"
local famecon "${RAW}\cfps2020famecon_202306.dta"

*--- Check whether files exist before running
*--- Stop immediately if any file is missing to avoid empty outputs
confirm file "`adult'"
confirm file "`famecon'"

*==================================================
* Step 0: Raw adult sample
*==================================================

use "`adult'", clear
di "Original sample size: " _N
flowappend, year(2020) step("Raw adult sample")

/*==================================================
  Step 1: Prepare child data
==================================================*/

keep ///
    pid pid_a_f pid_a_m fid20 ///
    qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
    party ///
    age gender ///
    cfps2020eduy incomea ///
    urban20 provcd20 ///
    qc201 qea1 qea2 ///
    qga1 qga101 ///
    qu11 qu201 ///
    qn8011 qn8012 ///
    qn12012 qn12016 ///
    qp202 qp702 qq4010 qn4004 ///
    pt1 qs1001 qs502 qs801_b_2code

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        pid_a_f pid_a_m fid20 ///
        qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
        party age gender cfps2020eduy incomea urban20 provcd20 ///
        qc201 qea1 qea2 qga1 qga101 qu11 qu201 ///
        qn8011 qn8012 qn12012 qn12016 ///
        qp202 qp702 qq4010 qn4004 ///
        pt1 qs1001 qs502 qs801_b_2code )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

di "Child sample size after deduplication (one row per pid): " _N
flowappend, year(2020) step("Valid unique respondents")

gen religion = .
replace religion = 1 if qn4004 == 1
replace religion = 3 if qn4004 == 0

label define religlbl 1 "Important" 3 "Not important", replace
label values religion religlbl

*—— Child PID
rename pid_a_f father_pid
rename pid_a_m mother_pid
rename fid20   fid

preserve
    keep if !missing(father_pid) | !missing(mother_pid)
    flowappend, year(2020) step("Intergenerational link identified")
restore

*—— Child basic characteristics
rename age              child_age
rename gender           child_gender
rename cfps2020eduy     child_edu
rename incomea          child_income
rename urban20          child_urban
rename provcd20         child_province
rename party            child_party

*—— Child attitudes / behaviors
rename qc201            child_edu_expect
rename qea1             child_marriage_now
rename qea2             child_marriage_prev
rename qga1             child_fulltime
rename qga101           child_parttime
rename qu11             child_mobile
rename qu201            child_internet

*—— Child subjective status
rename qn8011           child_income_rank
rename qn8012           child_status_rank
rename qn12012          child_life_sat
rename qn12016          child_future_conf

*—— Child health / lifestyle
rename qp202            child_health_change
rename qp702            child_exercise
rename qq4010           child_sleep
rename religion         child_religion

*—— Child educational outcomes
rename pt1              child_tutor
rename qs1001           child_boarding
rename qs502            child_stress
rename qs801_b_2code    child_job_asp

*—— Child occupational outcomes
rename qg303code_isco   child_isco
rename qg303code_isei   child_isei
rename qg303code_siops  child_siops
rename qg303code_egp    child_egp

tempfile child_data
save `child_data', replace

di "Child sample size: " _N

/*==================================================
  Step 2: Prepare father data
==================================================*/

use "`adult'", clear

keep ///
    pid qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
    age gender ///
    cfps2020eduy incomea ///
    party ///
    urban20 provcd20 ///
    qc201 qea1 qea2 ///
    qga1 qga101 ///
    qu11 qu201 ///
    qn8011 qn8012 ///
    qn12012 qn12016 ///
    qp202 qp702 qq4010 qn4004 ///
    pt1 qs1001 qs502 qs801_b_2code

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
        age gender cfps2020eduy incomea party urban20 provcd20 ///
        qc201 qea1 qea2 qga1 qga101 qu11 qu201 ///
        qn8011 qn8012 qn12012 qn12016 ///
        qp202 qp702 qq4010 qn4004 ///
        pt1 qs1001 qs502 qs801_b_2code )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

gen religion = .
replace religion = 1 if qn4004 == 1
replace religion = 3 if qn4004 == 0

label define religlbl 1 "Important" 3 "Not important", replace
label values religion religlbl

rename age              f_age
rename gender           f_gender
rename cfps2020eduy     f_edu
rename incomea          f_income
rename urban20          f_urban
rename provcd20         f_province
rename party            f_party

rename qc201            f_edu_expect
rename qea1             f_marriage_now
rename qea2             f_marriage_prev
rename qga1             f_fulltime
rename qga101           f_parttime
rename qu11             f_mobile
rename qu201            f_internet

rename qn8011           f_income_rank
rename qn8012           f_status_rank
rename qn12012          f_life_sat
rename qn12016          f_future_conf

rename qp202            f_health_change
rename qp702            f_exercise
rename qq4010           f_sleep
rename religion         f_religion

rename pt1              f_tutor
rename qs1001           f_boarding
rename qs502            f_stress
rename qs801_b_2code    f_job_asp

*—— Occupational outcomes
rename qg303code_isco   f_isco
rename qg303code_isei   f_isei
rename qg303code_siops  f_siops
rename qg303code_egp    f_egp

rename pid father_pid

drop if missing(father_pid)
capture noisily isid father_pid
if _rc {
    egen finfo = rownonmiss( ///
        f_isco f_isei f_siops f_egp ///
        f_age f_gender f_edu f_income f_party f_urban f_province ///
        f_edu_expect f_marriage_now f_marriage_prev ///
        f_fulltime f_parttime f_mobile f_internet ///
        f_income_rank f_status_rank f_life_sat f_future_conf ///
        f_health_change f_exercise f_sleep f_religion ///
        f_tutor f_boarding f_stress f_job_asp )
    bysort father_pid (finfo): keep if _n == _N
    drop finfo
}
isid father_pid

tempfile father_data
save `father_data', replace

di "Father sample size: " _N

/*==================================================
  Step 3: Prepare mother data
==================================================*/

use "`adult'", clear

keep ///
    pid qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
    age gender ///
    cfps2020eduy incomea ///
    party ///
    urban20 provcd20 ///
    qc201 qea1 qea2 ///
    qga1 qga101 ///
    qu11 qu201 ///
    qn8011 qn8012 ///
    qn12012 qn12016 ///
    qp202 qp702 qq4010 qn4004 ///
    pt1 qs1001 qs502 qs801_b_2code

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
        age gender cfps2020eduy incomea party urban20 provcd20 ///
        qc201 qea1 qea2 qga1 qga101 qu11 qu201 ///
        qn8011 qn8012 qn12012 qn12016 ///
        qp202 qp702 qq4010 qn4004 ///
        pt1 qs1001 qs502 qs801_b_2code )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

gen religion = .
replace religion = 1 if qn4004 == 1
replace religion = 3 if qn4004 == 0

label define religlbl 1 "Important" 3 "Not important", replace
label values religion religlbl

rename age              m_age
rename gender           m_gender
rename cfps2020eduy     m_edu
rename incomea          m_income
rename urban20          m_urban
rename provcd20         m_province
rename party            m_party

rename qc201            m_edu_expect
rename qea1             m_marriage_now
rename qea2             m_marriage_prev
rename qga1             m_fulltime
rename qga101           m_parttime
rename qu11             m_mobile
rename qu201            m_internet

rename qn8011           m_income_rank
rename qn8012           m_status_rank
rename qn12012          m_life_sat
rename qn12016          m_future_conf

rename qp202            m_health_change
rename qp702            m_exercise
rename qq4010           m_sleep
rename religion         m_religion

rename pt1              m_tutor
rename qs1001           m_boarding
rename qs502            m_stress
rename qs801_b_2code    m_job_asp

*—— Occupational outcomes
rename qg303code_isco   m_isco
rename qg303code_isei   m_isei
rename qg303code_siops  m_siops
rename qg303code_egp    m_egp

rename pid mother_pid

drop if missing(mother_pid)
capture noisily isid mother_pid
if _rc {
    egen minfo = rownonmiss( ///
        m_isco m_isei m_siops m_egp ///
        m_age m_gender m_edu m_income m_party m_urban m_province ///
        m_edu_expect m_marriage_now m_marriage_prev ///
        m_fulltime m_parttime m_mobile m_internet ///
        m_income_rank m_status_rank m_life_sat m_future_conf ///
        m_health_change m_exercise m_sleep m_religion ///
        m_tutor m_boarding m_stress m_job_asp )
    bysort mother_pid (minfo): keep if _n == _N
    drop minfo
}
isid mother_pid

tempfile mother_data
save `mother_data', replace

di "Mother sample size: " _N

/*==================================================
  Step 4: Matching
==================================================*/

use `child_data', clear
describe father_pid mother_pid

merge m:1 father_pid using `father_data', keep(1 3)
gen has_father = (_merge==3)
drop _merge

merge m:1 mother_pid using `mother_data', keep(1 3)
gen has_mother = (_merge==3)
drop _merge

keep if has_father==1 | has_mother==1
flowappend, year(2020) step("Matched to at least one parent")

*==================================================*
* Merge family economic data
*==================================================*

preserve
    use "`famecon'", clear

    keep ///
        fid20 ///
        total_asset house_debts resivalue ///
        fincome1 familysize ///
        fswt_natcs20n

    rename fid20           fid
    rename total_asset     family_total_asset
    rename house_debts     family_house_debts
    rename resivalue       family_resivalue
    rename fincome1        family_income
    rename familysize      family_size
    rename fswt_natcs20n   family_cs

    duplicates drop fid, force
    tempfile fe_ses
    save `fe_ses', replace
restore

merge m:1 fid using `fe_ses', keep(1 3)
gen has_famecon = (_merge==3)

preserve
    keep if has_famecon == 1
    flowappend, year(2020) step("Matched to family economic data")
restore

tab _merge
gen miss_fid = missing(fid)
tab miss_fid _merge, row

drop _merge

tab has_famecon

/*==================================================
  Step 5: Check results
==================================================*/

di _newline(2)
di "============================================================"
di "Detailed summary of matching results"
di "============================================================"
di "Total sample size: " _N
di "Number of variables: " c(k)

di _newline "Cross-tabulation of matching status:"
tab has_father has_mother

di "Sample size with complete key variables: " r(N) " (" %4.1f 100*r(N)/_N "%)"

/*==================================================
  Step 6: Data cleaning —— outliers / invalid values
  Data structure:
    - child_* : child generation
    - f_*     : father
    - m_*     : mother
==================================================*/

local dropneg ///
    child_edu child_income child_edu_expect ///
    child_marriage_now child_marriage_prev ///
    child_mobile child_sleep ///
    child_income_rank child_status_rank child_life_sat child_future_conf ///
    child_health_change child_exercise child_religion ///
    child_tutor child_boarding child_stress ///
    child_isco child_isei child_siops child_egp ///
    f_isco f_isei f_siops f_egp f_edu f_income f_age ///
    m_isco m_isei m_siops m_egp m_edu m_income m_age ///
    family_total_asset family_house_debts family_resivalue family_income family_size family_cs

foreach v of local dropneg {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
    }
}

preserve
    keep if !missing(child_isei)
    flowappend, year(2020) step("With child occupational outcome")
restore

****************************************************
* Cleaning completed message
****************************************************

di _newline(2)
di "============================================================"
di "Outlier cleaning completed (based on the child_/f_/m_ variable system)"
di "============================================================"

gen parent_type = .
replace parent_type = 1 if has_father == 1 & has_mother == 1
replace parent_type = 2 if has_father == 1 & has_mother == 0
replace parent_type = 3 if has_father == 0 & has_mother == 1
label define pt 1 "Both parents" 2 "Father only" 3 "Mother only", replace
label values parent_type pt

/*==================================================
  Step 7: Save
==================================================*/

compress
flowappend, year(2020) step("Yearly file saved")
save "${YOUT}\20_data.dta", replace

di _newline(2)
di "============================================================"
di "Data saved successfully!"
di "============================================================"
di "File: 20_data.dta"
di "Sample size: " _N
di "Number of variables: " c(k)
di "============================================================"
