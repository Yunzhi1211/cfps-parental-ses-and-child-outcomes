/*==================================================
  CFPS 2012 Occupation Code Generation + Intergenerational Matching + Data Outlier Cleaning
  Occupation codes include:
    - ISCO/ISEI/SIOPS/EGP
  Variable naming system:
    - child_* : Child generation
    - f_*     : Father
    - m_*     : Mother
  Data issue in this wave:
    - Due to changes in the 2012 occupational survey logic, only job2012mn_occu has a relatively large number of observations, and its classification logic is consistent with other years
    - job2012mn_occu is coded in CSCO, with 13,956 observations and a value range from five-digit codes 10101-900000
  Family database already merged
  First Updated on Feb 21st
  Latest Updated on April 9th
==================================================*/

local wave 2012

*--- Raw data paths
local adult   "${RAW}\cfps2012adult_202505.dta"
local famecon "${RAW}\cfps2012famecon_201906.dta"

*--- Check whether files exist before running (stop immediately if any file is missing to avoid empty results)
confirm file "`adult'"
confirm file "`famecon'"

*==================================================
* Step 0: Raw adult sample
*==================================================

use "`adult'", clear
di "Original sample size: " _N
flowappend, year(2012) step("Raw adult sample")

*==================================================
* A. Build occupation-code crosswalk: occ5 -> isco/isei/siops
*==================================================

tempfile map map_u

preserve
    clear
    import excel using "${RAW}\职业与职业威望转化说明.xlsx", firstrow clear

    rename 国标数值代码 occ5
    rename ISCO88_CFPS  isco
    rename ISEI         isei
    rename SIOPS        siops

    keep occ5 isco isei siops
    drop if missing(occ5)

    compress
    save `map', replace

    use `map', clear
    sort occ5 isco isei siops
    duplicates drop occ5, force
    save `map_u', replace
restore

*==================================================
* B. First generate isco/isei/siops/egp in the 2012 adult file
*    Then use them for merging into child/father/mother datasets
*==================================================

use "`adult'", clear

*—— 1) occ5: recode negative/special values as missing
capture drop occ5
recode job2012mn_occu (min/-1=.) (997=.) (999=.), gen(occ5)

*—— 2) Merge isco/isei/siops
merge m:1 occ5 using `map_u', keep(1 3) nogen

*—— 3) Construct variables required for EGP and generate egp
adopath ++ "${RAW}"
which iskoegp

* 3.1) supvis: number of supervisees/subordinates
capture drop supvis
gen supvis = qg707
replace supvis = . if supvis < 0
replace supvis = 1 if missing(supvis) & qg706==1
replace supvis = 0 if missing(supvis) & qg706==5

* 3.2) sempl: self-employed/operator (1) vs employee (0)
capture drop sempl
gen sempl = .
replace sempl = 0 if employ==1
replace sempl = 1 if employ==1 & qg502==1
replace sempl = 1 if employ==1 & qg601==1

* 3.3) Generate egp
capture drop egp
iskoegp egp, isko(isco) sempl(sempl) supvis(supvis)

* Keep only fields needed for matching and save as "occupation conversion result table"
keep pid occ5 isco isei siops egp sempl supvis employ

* 建议这里顺手去一下 pid 缺失和重复
drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss(occ5 isco isei siops egp sempl supvis employ)
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

tempfile occ2012
save `occ2012', replace

*==================================================
* Step 1: Prepare child data
*==================================================

use "`adult'", clear
di "Original child candidate sample size: " _N

keep ///
    pid pid_f pid_m fid12 ///
    cfps2012_age cfps2012_gender_best ///
    urban12 provcd ///
    eduy2012 income ///
    cfps_party qc201 ///
    qe101 qe104 ///
    qn8011 qn8012 qn12012 qn12014 ///
    qp202 qp701 qm603 kr410 kr405a ks502 ks8 

* 去掉缺失 pid，并按信息完整度去重
drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        pid_f pid_m fid12 ///
        cfps2012_age cfps2012_gender_best urban12 provcd eduy2012 income ///
        cfps_party qc201 qe101 qe104 ///
        qn8011 qn8012 qn12012 qn12014 ///
        qp202 qp701 qm603 kr410 kr405a ks502 ks8 )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

di "Child sample size after deduplication (one row per pid): " _N
flowappend, year(2012) step("Valid unique respondents")

*—— Child PID
rename pid_f father_pid
rename pid_m mother_pid
rename fid12 fid

preserve
    keep if !missing(father_pid) | !missing(mother_pid)
    flowappend, year(2012) step("Intergenerational link identified")
restore

*—— Child basic characteristics
rename cfps2012_age          child_age
rename cfps2012_gender_best  child_gender
rename eduy2012              child_edu
rename income                child_income
rename urban12               child_urban
rename provcd                child_province
rename cfps_party            child_party

*—— Child attitudes/behaviors
rename qc201                 child_edu_expect
rename qe104                 child_marriage_now
rename qe101                 child_marriage_prev

*—— Child subjective status
rename qn8011                child_income_rank
rename qn8012                child_status_rank
rename qn12012               child_life_sat
rename qn12014               child_future_conf

*—— Child health/lifestyle
rename qp202                 child_health_change
rename qp701                 child_exercise
rename qm603                 child_religion

*—— Child educational outcomes
rename kr410                 child_tutor
rename kr405a                child_boarding
rename ks502                 child_stress
rename ks8                   child_job_asp

*—— Merge occupation variables calculated from the 2012 adult file into the child data (by pid)
merge 1:1 pid using `occ2012', keep(1 3) nogen

rename occ5                  child_occ5
rename isco                  child_isco
rename isei                  child_isei
rename siops                 child_siops
rename egp                   child_egp
rename sempl                 child_sempl
rename supvis                child_supvis

tempfile child_data
save `child_data', replace
di "Child sample size: " _N

*==================================================
* Step 2: Prepare father data
*==================================================

use "`adult'", clear

keep ///
    pid ///
    cfps2012_age cfps2012_gender_best ///
    urban12 provcd ///
    eduy2012 income ///
    cfps_party qc201 ///
    qe101 qe104 ///
    qn8011 qn8012 qn12012 qn12014 ///
    qp202 qp701 qm603 kr410 kr405a ks502 ks8 

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        cfps2012_age cfps2012_gender_best urban12 provcd eduy2012 income ///
        cfps_party qc201 qe101 qe104 ///
        qn8011 qn8012 qn12012 qn12014 ///
        qp202 qp701 qm603 kr410 kr405a ks502 ks8 )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

rename pid                   father_pid

rename cfps2012_age          f_age
rename cfps2012_gender_best  f_gender
rename eduy2012              f_edu
rename income                f_income
rename urban12               f_urban
rename provcd                f_province
rename cfps_party            f_party

rename qc201                 f_edu_expect
rename qe104                 f_marriage_now
rename qe101                 f_marriage_prev

rename qn8011                f_income_rank
rename qn8012                f_status_rank
rename qn12012               f_life_sat
rename qn12014               f_future_conf

rename qp202                 f_health_change
rename qp701                 f_exercise
rename qm603                 f_religion

rename kr410                 f_tutor
rename kr405a                f_boarding
rename ks502                 f_stress
rename ks8                   f_job_asp

*—— Merge occupation indicators
preserve
    use `occ2012', clear
    rename pid father_pid
    tempfile occ2012_f
    save `occ2012_f', replace
restore

merge 1:1 father_pid using `occ2012_f', keep(1 3) nogen

rename occ5                  f_occ5
rename isco                  f_isco
rename isei                  f_isei
rename siops                 f_siops
rename egp                   f_egp
rename sempl                 f_sempl
rename supvis                f_supvis

drop if missing(father_pid)
capture noisily isid father_pid
if _rc {
    egen finfo = rownonmiss( ///
        f_occ5 f_isco f_isei f_siops f_egp f_sempl f_supvis ///
        f_age f_gender f_edu f_income f_urban f_province f_party ///
        f_edu_expect f_marriage_now f_marriage_prev ///
        f_income_rank f_status_rank f_life_sat f_future_conf ///
        f_health_change f_exercise f_religion f_tutor f_boarding f_stress f_job_asp )
    bysort father_pid (finfo): keep if _n == _N
    drop finfo
}
isid father_pid

tempfile father_data
save `father_data', replace
di "Father sample size: " _N

*==================================================
* Step 3: Prepare mother data
*==================================================

use "`adult'", clear

keep ///
    pid ///
    cfps2012_age cfps2012_gender_best ///
    urban12 provcd ///
    eduy2012 income ///
    cfps_party qc201 ///
    qe101 qe104 ///
    qn8011 qn8012 qn12012 qn12014 ///
    qp202 qp701 qm603 kr410 kr405a ks502 ks8 

drop if missing(pid)
capture noisily isid pid
if _rc {
    egen info = rownonmiss( ///
        cfps2012_age cfps2012_gender_best urban12 provcd eduy2012 income ///
        cfps_party qc201 qe101 qe104 ///
        qn8011 qn8012 qn12012 qn12014 ///
        qp202 qp701 qm603 kr410 kr405a ks502 ks8 )
    bysort pid (info): keep if _n == _N
    drop info
}
isid pid

rename pid                   mother_pid

rename cfps2012_age          m_age
rename cfps2012_gender_best  m_gender
rename eduy2012              m_edu
rename income                m_income
rename urban12               m_urban
rename provcd                m_province
rename cfps_party            m_party

rename qc201                 m_edu_expect
rename qe104                 m_marriage_now
rename qe101                 m_marriage_prev

rename qn8011                m_income_rank
rename qn8012                m_status_rank
rename qn12012               m_life_sat
rename qn12014               m_future_conf

rename qp202                 m_health_change
rename qp701                 m_exercise
rename qm603                 m_religion

rename kr410                 m_tutor
rename kr405a                m_boarding
rename ks502                 m_stress
rename ks8                   m_job_asp

*—— Merge occupation indicators
preserve
    use `occ2012', clear
    rename pid mother_pid
    tempfile occ2012_m
    save `occ2012_m', replace
restore

merge 1:1 mother_pid using `occ2012_m', keep(1 3) nogen

rename occ5                  m_occ5
rename isco                  m_isco
rename isei                  m_isei
rename siops                 m_siops
rename egp                   m_egp
rename sempl                 m_sempl
rename supvis                m_supvis

drop if missing(mother_pid)
capture noisily isid mother_pid
if _rc {
    egen minfo = rownonmiss( ///
        m_occ5 m_isco m_isei m_siops m_egp m_sempl m_supvis ///
        m_age m_gender m_edu m_income m_urban m_province m_party ///
        m_edu_expect m_marriage_now m_marriage_prev ///
        m_income_rank m_status_rank m_life_sat m_future_conf ///
        m_health_change m_exercise m_religion m_tutor m_boarding m_stress m_job_asp )
    bysort mother_pid (minfo): keep if _n == _N
    drop minfo
}
isid mother_pid

tempfile mother_data
save `mother_data', replace
di "Mother sample size: " _N

*==================================================
* Step 4: Matching (child-father-mother)
*==================================================

use `child_data', clear
describe father_pid mother_pid

merge m:1 father_pid using `father_data', keep(1 3)
gen has_father = (_merge==3)
drop _merge

merge m:1 mother_pid using `mother_data', keep(1 3)
gen has_mother = (_merge==3)
drop _merge

keep if has_father==1 | has_mother==1
flowappend, year(2012) step("Matched to at least one parent")

*==================================================*
* Merge 2012 family economic data
*==================================================*

preserve
    use "`famecon'", clear

    keep ///
        fid12 total_asset house_debts resivalue_new ///
        fincome2_adj familysize ///
        fswt_natcs12 fswt_natpn1012

    rename fid12            fid
    rename total_asset      family_totalasset
    rename house_debts      family_house_debts
    rename resivalue_new    family_resivalue
    rename fincome2_adj     family_income
    rename familysize       family_size
    rename fswt_natcs12     family_cs
    rename fswt_natpn1012   family_pn

    duplicates drop fid, force

    tempfile fe_ses
    save `fe_ses', replace
restore

merge m:1 fid using `fe_ses', keep(1 3)
gen has_famecon = (_merge==3)

preserve
    keep if has_famecon == 1
    flowappend, year(2012) step("Matched to family economic data")
restore

tab _merge
gen miss_fid = missing(fid)
tab miss_fid _merge, row

drop _merge

tab has_famecon

*==================================================
* Step 5: Summary of matching results
*==================================================

di _newline(2)
di "============================================================"
di "Detailed summary of matching results (2012)"
di "============================================================"
di "Total sample size: " _N
di "Number of variables: " c(k)

tab has_father has_mother

gen parent_type = .
replace parent_type = 1 if has_father==1 & has_mother==1
replace parent_type = 2 if has_father==1 & has_mother==0
replace parent_type = 3 if has_father==0 & has_mother==1
label define pt 1 "Both parents" 2 "Father only" 3 "Mother only", replace
label values parent_type pt

*==================================================
* Step 6: Clean variables
*==================================================

local dropneg ///
    child_edu child_income child_edu_expect ///
    child_marriage_now child_marriage_prev ///
    child_income_rank child_status_rank child_life_sat child_future_conf ///
    child_health_change child_exercise child_religion ///
    child_tutor child_boarding child_stress ///
    child_occ5 child_isco child_isei child_siops child_egp ///
    f_occ5 f_isco f_isei f_siops f_egp f_edu f_income f_age ///
    m_occ5 m_isco m_isei m_siops m_egp m_edu m_income m_age ///
    family_totalasset family_house_debts family_resivalue family_income family_size family_cs family_pn

foreach v of local dropneg {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
    }
}

preserve
    keep if !missing(child_isei)
    flowappend, year(2012) step("With child occupational outcome")
restore

****************************************************
* Cleaning completion notice
****************************************************

di _newline(2)
di "============================================================"
di "Outlier cleaning completed (based on the child_/f_/m_ variable system)"
di "============================================================"

*==================================================
* Step 7: Save
*==================================================

compress
flowappend, year(2012) step("Yearly file saved")
save "${YOUT}\12_data.dta", replace

di _newline(2)
di "============================================================"
di "Data saved successfully!"
di "============================================================"
di "File: 12_data.dta"
di "Sample size: " _N
di "Number of variables: " c(k)
di "============================================================"
