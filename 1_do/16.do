/*==================================================
  CFPS 2016 Intergenerational Matching + Data Outlier Cleaning
  Special issue in this wave:
    - The adult database does not contain pid and fid together, so they need to be merged from the famconf file
  First Updated on Feb 20th
  Latest Updated on April 9th
==================================================*/

local wave 2016

global RAW    "${PROJ}\0_raw"
local famconf "${RAW}\cfps2016famconf_201804.dta"
local adult   "${RAW}\cfps2016adult_201906.dta"
local famecon "${RAW}\cfps2016famecon_201807.dta"

*--- Check whether files exist before running
confirm file "`famconf'"
confirm file "`adult'"
confirm file "`famecon'"

*==================================================*
* Step 0: Raw adult sample
*==================================================*
use "`adult'", clear
di "Original sample size: " _N
flowappend, year(2016) step("Raw adult sample")

*==================================================*
* 0. Build the child pid -> parent pid link file (pid_f/pid_m)
*==================================================*
use "`famconf'", clear
keep pid pid_f pid_m
rename pid_f father_pid
rename pid_m mother_pid

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss(father_pid mother_pid)
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

tempfile link2016
save `link2016', replace

*==================================================*
* 1. Child data: adult + parent pid
*==================================================*
use "`adult'", clear
merge 1:1 pid using `link2016', keep(1 3) nogen

di "Original child candidate sample size: " _N

keep ///
    pid father_pid mother_pid fid16 ///
    qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
    qn4001 ///
    cfps_age cfps_gender ///
    cfps2016eduy income ///
    urban16 provcd16 ///
    qc201 qea1 qea2 qm603 ///
    qga1 qga101 ///
    ku1m ku201 ///
    qn8011 qn8012 ///
    qn12012 qn12014 ///
    qp202 qp702 qq4010 ///
    pt1 ks502 ks801code ps1001

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        father_pid mother_pid fid16 ///
        qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
        qn4001 cfps_age cfps_gender cfps2016eduy income ///
        urban16 provcd16 qc201 qea1 qea2 qm603 ///
        qga1 qga101 ku1m ku201 ///
        qn8011 qn8012 qn12012 qn12014 ///
        qp202 qp702 qq4010 pt1 ks502 ks801code ps1001 )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

di "Child sample size after deduplication (one row per pid): " _N
flowappend, year(2016) step("Valid unique respondents")

gen ex = qp702*60 if qp702>=0
label var ex "一周锻炼时长(分钟)"

preserve
    keep if !missing(father_pid) | !missing(mother_pid)
    flowappend, year(2016) step("Intergenerational link identified")
restore

*—— Child basic characteristics
rename cfps_age        child_age
rename cfps_gender     child_gender
rename cfps2016eduy    child_edu
rename income          child_income
rename urban16         child_urban
rename provcd16        child_province
rename qn4001          child_party

*—— Child attitudes/behaviors
rename qc201           child_edu_expect
rename qea1            child_marriage_now
rename qea2            child_marriage_prev
rename qga1            child_fulltime
rename qga101          child_parttime
rename ku1m            child_mobile
rename ku201           child_internet

*—— Child subjective ranking/satisfaction
rename qn8011          child_income_rank
rename qn8012          child_status_rank
rename qn12012         child_life_sat
rename qn12014         child_future_conf

*—— Child health/lifestyle
rename qp202           child_health_change
rename ex              child_exercise
rename qq4010          child_sleep
rename qm603           child_religion

*—— Child education/aspiration
rename pt1             child_tutor
rename ps1001          child_boarding
rename ks502           child_stress
rename ks801code       child_job_asp

*—— Child occupational status
rename qg303code_isco  child_isco
rename qg303code_isei  child_isei
rename qg303code_siops child_siops
rename qg303code_egp   child_egp

rename fid16 fid

tempfile child_data
save `child_data', replace

*==================================================*
* 2. Father data: extract from adult file (using pid as father key)
*==================================================*
use "`adult'", clear

keep ///
    pid ///
    qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
    qn4001 ///
    cfps_age cfps_gender ///
    cfps2016eduy income ///
    urban16 provcd16 ///
    qc201 qea1 qea2 qm603 ///
    qga1 qga101 ///
    ku1m ku201 ///
    qn8011 qn8012 ///
    qn12012 qn12014 ///
    qp202 qp702 qq4010 ///
    pt1 ks502 ks801code ps1001

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
        qn4001 cfps_age cfps_gender cfps2016eduy income ///
        urban16 provcd16 qc201 qea1 qea2 qm603 ///
        qga1 qga101 ku1m ku201 ///
        qn8011 qn8012 qn12012 qn12014 ///
        qp202 qp702 qq4010 pt1 ks502 ks801code ps1001 )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

rename pid father_pid

gen ex = qp702*60 if qp702>=0
label var ex "一周锻炼时长(分钟)"

rename cfps_age        f_age
rename cfps_gender     f_gender
rename cfps2016eduy    f_edu
rename income          f_income
rename urban16         f_urban
rename provcd16        f_province
rename qn4001          f_party

rename qc201           f_edu_expect
rename qea1            f_marriage_now
rename qea2            f_marriage_prev
rename qga1            f_fulltime
rename qga101          f_parttime
rename ku1m            f_mobile
rename ku201           f_internet

rename qn8011          f_income_rank
rename qn8012          f_status_rank
rename qn12012         f_life_sat
rename qn12014         f_future_conf

rename qp202           f_health_change
rename ex              f_exercise
rename qq4010          f_sleep
rename qm603           f_religion

rename pt1             f_tutor
rename ps1001          f_boarding
rename ks502           f_stress
rename ks801code       f_job_asp

rename qg303code_isco  f_isco
rename qg303code_isei  f_isei
rename qg303code_siops f_siops
rename qg303code_egp   f_egp

drop if missing(father_pid)
capture noisily isid father_pid
if _rc {
    egen finfo = rownonmiss( ///
        f_isco f_isei f_siops f_egp ///
        f_age f_gender f_edu f_income f_urban f_province f_party ///
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

*==================================================*
* 3. Mother data
*==================================================*
use "`adult'", clear

keep ///
    pid ///
    qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
    qn4001 ///
    cfps_age cfps_gender ///
    cfps2016eduy income ///
    urban16 provcd16 ///
    qc201 qea1 qea2 qm603 ///
    qga1 qga101 ///
    ku1m ku201 ///
    qn8011 qn8012 ///
    qn12012 qn12014 ///
    qp202 qp702 qq4010 ///
    pt1 ks502 ks801code ps1001

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        qg303code_isco qg303code_isei qg303code_siops qg303code_egp ///
        qn4001 cfps_age cfps_gender cfps2016eduy income ///
        urban16 provcd16 qc201 qea1 qea2 qm603 ///
        qga1 qga101 ku1m ku201 ///
        qn8011 qn8012 qn12012 qn12014 ///
        qp202 qp702 qq4010 pt1 ks502 ks801code ps1001 )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

rename pid mother_pid

gen ex = qp702*60 if qp702>=0
label var ex "一周锻炼时长(分钟)"

rename cfps_age        m_age
rename cfps_gender     m_gender
rename cfps2016eduy    m_edu
rename income          m_income
rename urban16         m_urban
rename provcd16        m_province
rename qn4001          m_party

rename qc201           m_edu_expect
rename qea1            m_marriage_now
rename qea2            m_marriage_prev
rename qga1            m_fulltime
rename qga101          m_parttime
rename ku1m            m_mobile
rename ku201           m_internet

rename qn8011          m_income_rank
rename qn8012          m_status_rank
rename qn12012         m_life_sat
rename qn12014         m_future_conf

rename qp202           m_health_change
rename ex              m_exercise
rename qq4010          m_sleep
rename qm603           m_religion

rename pt1             m_tutor
rename ps1001          m_boarding
rename ks502           m_stress
rename ks801code       m_job_asp

rename qg303code_isco  m_isco
rename qg303code_isei  m_isei
rename qg303code_siops m_siops
rename qg303code_egp   m_egp

drop if missing(mother_pid)
capture noisily isid mother_pid
if _rc {
    egen minfo = rownonmiss( ///
        m_isco m_isei m_siops m_egp ///
        m_age m_gender m_edu m_income m_urban m_province m_party ///
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

*==================================================*
* 4. Intergenerational matching
*==================================================*
use `child_data', clear

merge m:1 father_pid using `father_data', keep(1 3)
gen has_father = (_merge==3)
drop _merge

merge m:1 mother_pid using `mother_data', keep(1 3)
gen has_mother = (_merge==3)
drop _merge

keep if has_father==1 | has_mother==1
flowappend, year(2016) step("Matched to at least one parent")

*==================================================*
* Merge family economic data
*==================================================*
preserve
    use "`famecon'", clear

    keep ///
        fid16 ///
        total_asset house_debts resivalue ///
        fincome2 familysize ///
        fswt_natcs16 fswt_natpn1016

    rename fid16           fid
    rename total_asset     family_total_asset
    rename house_debts     family_house_debts
    rename resivalue       family_resivalue
    rename fincome2        family_income
    rename familysize      family_size
    rename fswt_natcs16    family_cs
    rename fswt_natpn1016  family_pn

    duplicates drop fid, force
    tempfile fe_ses
    save `fe_ses', replace
restore

merge m:1 fid using `fe_ses', keep(1 3)
gen has_famecon = (_merge==3)

preserve
    keep if has_famecon == 1
    flowappend, year(2016) step("Matched to family economic data")
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
di "Detailed Summary of Matching Results"
di "============================================================"
di "Total sample size: " _N
di "Number of variables: " c(k)

di _newline "Cross-tabulation of matching status:"
tab has_father has_mother

di "Sample with complete key variables: " r(N) " (" %4.1f 100*r(N)/_N "%)"

/*==================================================
  Step 6: Data cleaning — outliers / invalid values
  Data structure:
    - child_* : Child generation
    - f_*     : Father
    - m_*     : Mother
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
    family_total_asset family_house_debts family_resivalue family_income family_size family_cs family_pn

foreach v of local dropneg {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
    }
}

preserve
    keep if !missing(child_isei)
    flowappend, year(2016) step("With child occupational outcome")
restore

****************************************************
* Cleaning completion notice
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
flowappend, year(2016) step("Yearly file saved")
save "${YOUT}\16_data.dta", replace

di _newline(2)
di "============================================================"
di "Data saved successfully!"
di "============================================================"
di "File: 16_data.dta"
di "Sample size: " _N
di "Number of variables: " c(k)
di "============================================================"
