/*==================================================
  CFPS 2010 Intergenerational Matching + Data Outlier Cleaning
  Family database already merged
  First Updated on Feb 21st
  Latest Updated on April 9th
==================================================*/

local wave 2010

*--- Raw data paths
local adult   "${RAW}\cfps2010adult_201906.dta"
local famecon "${RAW}\cfps2010famecon_201906.dta"

*--- Check whether files exist before running
confirm file "`adult'"
confirm file "`famecon'"

*==================================================*
* Step 0: Raw adult sample
*==================================================*
use "`adult'", clear
di "Original sample size: " _N
flowappend, year(2010) step("Raw adult sample")

/*==================================================
  Step 1: Prepare Child Data
==================================================*/

use "`adult'", clear

keep ///
    pid pid_f pid_m fid /// 
    qa701 /// Year joined the Party
    qa1age gender ///
    cfps2010eduy_best income ///
    urban provcd ///
    qc8 qe1 /// qc8 minimum expected education level, qe1 current marital status
    /// Group 1
    ku1 ku2 kt101_a_1 ///
    /// Group 2: subjective status
    qm401 qm402 qm403 qm404 /// 
    /// Group 3: behavioral variables
    qp301 qp801 /// 
    kr410 kr405 ks502 ///
    qg307isco qg307isei qg307siops qg307egp 

* 2010如有缺失 pid，先删掉
drop if missing(pid)

* 如果有重复 pid，保留信息更完整的一条
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        pid_f pid_m fid qa701 qa1age gender cfps2010eduy_best income ///
        urban provcd qc8 qe1 ku1 ku2 kt101_a_1 ///
        qm401 qm402 qm403 qm404 qp301 qp801 kr410 kr405 ks502 ///
        qg307isco qg307isei qg307siops qg307egp )
    bysort pid (info): keep if _n == _N
    drop info
}

isid pid
di "Child sample size after deduplication (one row per pid): " _N
flowappend, year(2010) step("Valid unique respondents")

gen party = inrange(qa701, 1921, 2026)
label define party 0 "非党员/不适用" 1 "党员", replace
label values party party

tab party, missing
tab qa701 party, missing

*—— Child PID
rename pid_f father_pid
rename pid_m mother_pid

preserve
    keep if !missing(father_pid) | !missing(mother_pid)
    flowappend, year(2010) step("Intergenerational link identified")
restore

*—— Child basic characteristics
rename qa1age            child_age
rename gender            child_gender
rename cfps2010eduy_best child_edu
rename income            child_income
rename urban             child_urban
rename provcd            child_province
rename party             child_party
rename qc8               child_edu_expect

*—— Group 1: child attitudes / behaviors
rename qe1       child_marriage_now
rename ku1       child_mobile
rename ku2       child_internet
rename kt101_a_1 child_sleep

*—— Group 2: child subjective status
rename qm401   child_income_rank
rename qm402   child_status_rank
rename qm403   child_life_sat
rename qm404   child_future_conf

*—— Group 3: child health / lifestyle
rename qp301   child_health_change
rename qp801   child_exercise

*—— Child educational outcomes
rename kr410   child_tutor
rename kr405   child_boarding
rename ks502   child_stress

*—— Child occupational outcomes
rename qg307isco  child_isco
rename qg307isei  child_isei
rename qg307siops child_siops
rename qg307egp   child_egp

tempfile child_data
save `child_data', replace

di "Child sample size: " _N


/*==================================================
  Step 2: Prepare Father Data
==================================================*/

use "`adult'", clear

keep ///
    pid qg307isco qg307isei qg307siops qg307egp /// 
    qa701 ///
    qa1age gender ///
    cfps2010eduy_best income ///
    urban provcd ///
    qc8 ///
    qe1 ku1 ku2 kt101_a_1 ///
    qm401 qm402 qm403 qm404 /// 
    qp301 qp801 /// 
    kr410 kr405 ks502 

drop if missing(pid)

capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        qg307isco qg307isei qg307siops qg307egp qa701 qa1age gender ///
        cfps2010eduy_best income urban provcd qc8 ///
        qe1 ku1 ku2 kt101_a_1 qm401 qm402 qm403 qm404 ///
        qp301 qp801 kr410 kr405 ks502 )
    bysort pid (info): keep if _n == _N
    drop info
}

isid pid

*—— Basic characteristics
rename qa1age            f_age
rename gender            f_gender
rename cfps2010eduy_best f_edu
rename income            f_income
rename urban             f_urban
rename provcd            f_province
rename qa701             f_party
rename qc8               f_expect

*—— Group 1: attitudes / behaviors
rename qe1       f_marriage_now
rename ku1       f_mobile
rename ku2       f_internet
rename kt101_a_1 f_sleep

*—— Group 2: subjective status
rename qm401   f_income_rank
rename qm402   f_status_rank
rename qm403   f_life_sat
rename qm404   f_future_conf

*—— Group 3: health / lifestyle
rename qp301   f_health_change
rename qp801   f_exercise

*—— Educational outcomes
rename kr410   f_tutor
rename kr405   f_boarding
rename ks502   f_stress

*—— Occupational outcomes
rename qg307isco  f_isco
rename qg307isei  f_isei
rename qg307siops f_siops
rename qg307egp   f_egp
rename pid father_pid

* Remove duplicates
drop if missing(father_pid)
capture noisily isid father_pid
if _rc {
    egen finfo = rownonmiss( ///
        f_isco f_isei f_siops f_egp f_party f_age f_gender f_edu f_income ///
        f_urban f_province f_expect f_marriage_now f_mobile f_internet ///
        f_sleep f_income_rank f_status_rank f_life_sat f_future_conf ///
        f_health_change f_exercise f_tutor f_boarding f_stress )
    bysort father_pid (finfo): keep if _n == _N
    drop finfo
}
isid father_pid

tempfile father_data
save `father_data', replace

di "Father sample size: " _N


/*==================================================
  Step 3: Prepare Mother Data
==================================================*/

use "`adult'", clear

keep ///
    pid qg307isco qg307isei qg307siops qg307egp /// 
    qa701 ///
    qa1age gender ///
    cfps2010eduy_best income ///
    urban provcd ///
    qc8 ///
    qe1 ku1 ku2 kt101_a_1 ///
    qm401 qm402 qm403 qm404 /// 
    qp301 qp801 /// 
    kr410 kr405 ks502

drop if missing(pid)

capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        qg307isco qg307isei qg307siops qg307egp qa701 qa1age gender ///
        cfps2010eduy_best income urban provcd qc8 ///
        qe1 ku1 ku2 kt101_a_1 qm401 qm402 qm403 qm404 ///
        qp301 qp801 kr410 kr405 ks502 )
    bysort pid (info): keep if _n == _N
    drop info
}

isid pid

*—— Basic characteristics
rename qa1age            m_age
rename gender            m_gender
rename cfps2010eduy_best m_edu
rename income            m_income
rename urban             m_urban
rename provcd            m_province
rename qa701             m_party
rename qc8               m_expect

*—— Group 1: attitudes / behaviors
rename qe1       m_marriage_now
rename ku1       m_mobile
rename ku2       m_internet
rename kt101_a_1 m_sleep

*—— Group 2: subjective status
rename qm401   m_income_rank
rename qm402   m_status_rank
rename qm403   m_life_sat
rename qm404   m_future_conf

*—— Group 3: health / lifestyle
rename qp301   m_health_change
rename qp801   m_exercise

*—— Educational outcomes
rename kr410   m_tutor
rename kr405   m_boarding
rename ks502   m_stress

*—— Occupational outcomes
rename qg307isco  m_isco
rename qg307isei  m_isei
rename qg307siops m_siops
rename qg307egp   m_egp
rename pid mother_pid

* Remove duplicates
drop if missing(mother_pid)
capture noisily isid mother_pid
if _rc {
    egen minfo = rownonmiss( ///
        m_isco m_isei m_siops m_egp m_party m_age m_gender m_edu m_income ///
        m_urban m_province m_expect m_marriage_now m_mobile m_internet ///
        m_sleep m_income_rank m_status_rank m_life_sat m_future_conf ///
        m_health_change m_exercise m_tutor m_boarding m_stress )
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
flowappend, year(2010) step("Matched to at least one parent")

*==================================================*
* Merge family economic data
*==================================================*
preserve
    use "`famecon'", clear

    keep ///
        fid ///
        total_asset house_debts resivalue_new ///
        faminc_net familysize ///
        fswt_nat 

    rename total_asset   family_totalasset
    rename house_debts   family_house_debts
    rename resivalue_new family_resivalue
    rename faminc_net    family_income
    rename familysize    family_size
    rename fswt_nat      family_cs

    duplicates drop fid, force
    tempfile fe_ses
    save `fe_ses', replace
restore

merge m:1 fid using `fe_ses', keep(1 3)
gen has_famecon = (_merge==3)

preserve
    keep if has_famecon == 1
    flowappend, year(2010) step("Matched to family economic data")
restore

tab _merge
gen miss_fid = missing(fid)
tab miss_fid _merge, row

drop _merge

tab has_famecon

/*==================================================
  Step 5: Check Results
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
  Step 6: Data Cleaning — Outliers / Invalid Values
  Data structure:
    - child_* : Child generation
    - f_*     : Father
    - m_*     : Mother
==================================================*/

* 1) Variables requiring cleanup for negative-value coding
local dropneg ///
    child_edu child_income child_edu_expect ///
    child_marriage_now ///
    child_mobile child_sleep ///
    child_income_rank child_status_rank child_life_sat child_future_conf ///
    child_health_change child_exercise ///
    child_tutor child_boarding child_stress ///
    child_isco child_isei child_siops child_egp ///
    f_isco f_isei f_siops f_egp f_edu f_income f_age ///
    m_isco m_isei m_siops m_egp m_edu m_income m_age ///
    family_totalasset family_house_debts family_resivalue family_income family_size family_cs

* 2) Recode values <0 as missing
foreach v of local dropneg {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
    }
}

preserve
    keep if !missing(child_isei)
    flowappend, year(2010) step("With child occupational outcome")
restore

di _newline(2)
di "============================================================"
di "异常值清洗完成（基于 child_/f_/m_ 变量体系）"
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
flowappend, year(2010) step("Yearly file saved")
save "${YOUT}\10_data.dta", replace

di _newline(2)
di "============================================================"
di "Data saved successfully!"
di "============================================================"
di "File: 10_data.dta"
di "Sample size: " _N
di "Number of variables: " c(k)
di "============================================================"
