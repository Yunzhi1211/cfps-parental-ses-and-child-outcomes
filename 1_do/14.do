/*==================================================
  CFPS 2014 Occupation Code Conversion + Intergenerational Matching + Data Outlier Cleaning
  Similar to 2012, CSCO codes need to be converted into ISCO, ISEI, SIOPS, and EGP codes
  Family database already merged
  First Updated on Feb 21st
  Latest Updated on April 9th
==================================================*/

local wave 2014

*--- Raw data paths
local adult   "${RAW}\cfps2014adult_201906.dta"
local famecon "${RAW}\cfps2014famecon_201906.dta"

*--- Check whether files exist before running (stop immediately if any file is missing to avoid empty results)
confirm file "`adult'"
confirm file "`famecon'"

*==================================================
* Step 0: Raw adult sample
*==================================================

use "`adult'", clear
di "Original sample size: " _N
flowappend, year(2014) step("Raw adult sample")

/*==================================================
  Step 0: 2014 occupation code conversion
  Reason for code conversion:
    - 2014 is similar to 2012
    - Only one variable has sufficient observations and appears frequently in previous work: qg303code, with 25,878 observations
  Code conversion process:
    - qg303code -> isco/isei/siops/egp
    - Also generate a pid-level occupation conversion result file occ2014 for subsequent merges
==================================================*/

*==================================================
* A. Build occupation-code crosswalk: occ5 -> isco/isei/siops
*==================================================

tempfile map map_u

preserve
    clear
    import excel using "${RAW}\职业与职业威望转化说明.xlsx", firstrow clear

    * Make sure this is the column for "CSCO / 5-digit national standard code"
    rename 国标数值代码 occ5
    rename ISCO88_CFPS  isco
    rename ISEI         isei
    rename SIOPS        siops

    keep occ5 isco isei siops
    drop if missing(occ5)

    compress
    save `map', replace

    use `map', clear
    sort occ5
    duplicates drop occ5, force
    save `map_u', replace
restore

*==================================================
* B. In the 2014 adult file: generate isco/isei/siops/egp from qg303code
*==================================================

use "`adult'", clear

*—— 1) occ5: from qg303code (5-digit CSCO), recode negative/special values as missing
capture drop occ5
gen occ5 = qg303code
replace occ5 = . if occ5 < 0
replace occ5 = . if occ5 == 997 | occ5 == 999

*—— 2) Merge isco/isei/siops
merge m:1 occ5 using `map_u', keep(1 3) nogen

*—— 3) Construct variables required for EGP and generate egp
adopath ++ "${RAW}"
which iskoegp

* 3.1) supvis: number of supervisees/subordinates (2014: qg1701 + qg17)
capture drop supvis
gen supvis = qg1701
replace supvis = . if supvis < 0
replace supvis = 1 if missing(supvis) & qg17==5
replace supvis = 0 if missing(supvis) & qg17==4

* 3.2) sempl: self-employed/operator (1) vs employee (0) —— 2014: qg2 + qg201, restricted to those "having a job" (employ==1)
capture drop sempl
gen sempl = .

* Determine status only for those "having a job"
replace sempl = . if employ != 1

* For those with a job: clean qg2
replace sempl = . if employ==1 & (qg2 < 0 | missing(qg2))

* (A) Clearly self-employed/family business: individual/family
replace sempl = 1 if employ==1 & qg2 == 7

* (B) Private enterprise/self-employed business: use qg201 to distinguish self-employed households
capture drop _qg201_yes _qg201_no
gen _qg201_yes = (qg201==1) if employ==1 & qg201>=0 & !missing(qg201)
gen _qg201_no  = (qg201==0 | qg201==5) if employ==1 & qg201>=0 & !missing(qg201)

replace sempl = 1 if employ==1 & qg2==4 & _qg201_yes==1
replace sempl = 0 if employ==1 & qg2==4 & _qg201_no==1

drop _qg201_yes _qg201_no

* (C) Clearly organization-employed: government/public institution/SOE/foreign enterprise/other enterprise/social organization => employee
replace sempl = 0 if employ==1 & inlist(qg2, 1,2,3,5,6,8)

* Cannot determine / other: keep missing
replace sempl = . if employ==1 & inlist(qg2, 9, 77)

label var sempl "Self-employed indicator (from qg2/qg201; only employ==1)"

capture drop egp
iskoegp egp, isko(isco) sempl(sempl) supvis(supvis)

tempfile occ2014
keep pid occ5 isco isei siops egp sempl supvis employ

drop if missing(pid)

egen info = rownonmiss(occ5 isco isei siops egp sempl supvis employ)
bysort pid (info): keep if _n==_N
drop info

isid pid
save `occ2014', replace

* Output results of code conversion
describe
count
summ isco isei siops egp supvis sempl, detail
tab egp, missing

/*==================================================
  Step 1: Prepare child data
==================================================*/

use "`adult'", clear
di "Reload adult file for child-sample construction: " _N

drop if missing(pid)

* Check whether there are duplicate cases
duplicates report pid

* Rule: within each pid, keep the observation with the most complete information
egen info = rownonmiss( ///
    pid_f pid_m cfps_party ///
    cfps2014_age cfps_gender cfps2014eduy income urban14 provcd qc201 ///
    qea1 qea2 qga1 qga101 ku1m ku2 qq4010 ///
    qn8011 qn8012 qn12012 qn12014 ///
    qp202 qp702 qm603 kr410 kr405a ks502 ks801code)

bysort pid (info): keep if _n==_N
drop info

isid pid
di "Child sample size after deduplication (one row per pid): " _N
flowappend, year(2014) step("Valid unique respondents")

* Then merge in the occupation conversion results
merge 1:1 pid using `occ2014', keep(1 3) nogen

keep ///
    pid pid_f pid_m fid14 ///
    cfps_party ///
    cfps2014_age cfps_gender ///
    cfps2014eduy income ///
    urban14 provcd qc201 ///
    qea1 qea2 qga1 qga101 ku1m ku2 qq4010 ///
    qn8011 qn8012 qn12012 qn12014 ///
    qp202 qp702 qm603 kr410 kr405a ks502 ks801code ///
    occ5 isco isei siops egp sempl supvis

gen ex = qp702*60 if qp702>=0
label var ex "一周锻炼时长(分钟)"

*—— Child PID
rename pid_f father_pid
rename pid_m mother_pid

preserve
    keep if !missing(father_pid) | !missing(mother_pid)
    flowappend, year(2014) step("Intergenerational link identified")
restore

*—— Child basic characteristics
rename cfps2014_age     child_age
rename cfps_gender      child_gender
rename cfps2014eduy     child_edu
rename income           child_income
rename urban14          child_urban
rename provcd           child_province
rename cfps_party       child_party
rename qc201            child_edu_expect

*—— Child attitudes / behaviors (Group 1)
rename qea1             child_marriage_now
rename qea2             child_marriage_prev
rename qga1             child_fulltime
rename qga101           child_parttime
rename ku1m             child_mobile
rename ku2              child_internet
rename qq4010           child_sleep

*—— Child subjective status (Group 2: subjective variables)
rename qn8011           child_income_rank
rename qn8012           child_status_rank
rename qn12012          child_life_sat
rename qn12014          child_future_conf

*—— Child health / lifestyle (Group 3: behavioral variables)
rename qp202            child_health_change
rename ex               child_exercise
rename qm603            child_religion

*—— Child educational outcomes (Group 3: behavioral variables)
rename kr410            child_tutor
rename kr405a           child_boarding
rename ks502            child_stress
rename ks801code        child_job_asp

*—— Rename occupation variables with child_ prefix to avoid conflicts with parents
rename occ5             child_occ5
rename isco             child_isco
rename isei             child_isei
rename siops            child_siops
rename egp              child_egp
rename sempl            child_sempl
rename supvis           child_supvis

rename fid14            fid

tempfile child_data
save `child_data', replace

di "Child sample size: " _N

/*==================================================
  Step 2: Prepare father data
==================================================*/

use "`adult'", clear

drop if missing(pid)
ds pid pid_f pid_m cfps_party cfps2014_age cfps_gender cfps2014eduy income urban14 provcd qc201 ///
   qea1 qea2 qga1 qga101 ku1m ku2 qq4010 ///
   qn8011 qn8012 qn12012 qn12014 ///
   qp202 qp702 qm603 kr410 kr405a ks502 ks801code
egen info = rownonmiss(`r(varlist)')
bysort pid (info): keep if _n==_N
drop info
isid pid

merge 1:1 pid using `occ2014', keep(1 3) nogen

keep ///
    pid ///
    cfps2014_age cfps_gender ///
    cfps2014eduy income ///
    cfps_party ///
    urban14 provcd ///
    qc201 ///
    qea1 qea2 qga1 qga101 ku1m ku2 qq4010 ///
    qn8011 qn8012 qn12012 qn12014 ///
    qp202 qp702 qm603 kr410 kr405a ks502 ks801code ///
    occ5 isco isei siops egp sempl supvis

gen ex = qp702*60 if qp702>=0
label var ex "一周锻炼时长(分钟)"

rename cfps2014_age     f_age
rename cfps_gender      f_gender
rename cfps2014eduy     f_edu
rename income           f_income
rename urban14          f_urban
rename provcd           f_province
rename cfps_party       f_party
rename qc201            f_edu_expect

rename qea1             f_marriage_now
rename qea2             f_marriage_prev
rename qga1             f_fulltime
rename qga101           f_parttime
rename ku1m             f_mobile
rename ku2              f_internet
rename qq4010           f_sleep

rename qn8011           f_income_rank
rename qn8012           f_status_rank
rename qn12012          f_life_sat
rename qn12014          f_future_conf

rename qp202            f_health_change
rename ex               f_exercise
rename qm603            f_religion

rename kr410            f_tutor
rename kr405a           f_boarding
rename ks502            f_stress
rename ks801code        f_child_job_asp

rename pid              father_pid

*—— Rename occupation variables with f_ prefix
rename occ5             f_occ5
rename isco             f_isco
rename isei             f_isei
rename siops            f_siops
rename egp              f_egp
rename sempl            f_sempl
rename supvis           f_supvis

drop if missing(father_pid)
egen finfo = rownonmiss(f_isei f_occ5 f_isco f_siops f_egp f_sempl f_supvis f_age f_edu f_income)
bysort father_pid (finfo): keep if _n==_N
drop finfo
isid father_pid

tempfile father_data
save `father_data', replace

di "Father sample size: " _N

/*==================================================
  Step 3: Prepare mother data
==================================================*/

use "`adult'", clear

drop if missing(pid)
ds pid pid_f pid_m cfps_party cfps2014_age cfps_gender cfps2014eduy income urban14 provcd qc201 ///
   qea1 qea2 qga1 qga101 ku1m ku2 qq4010 ///
   qn8011 qn8012 qn12012 qn12014 ///
   qp202 qp702 qm603 kr410 kr405a ks502 ks801code
egen info = rownonmiss(`r(varlist)')
bysort pid (info): keep if _n==_N
drop info
isid pid

merge 1:1 pid using `occ2014', keep(1 3) nogen

keep ///
    pid ///
    cfps2014_age cfps_gender ///
    cfps2014eduy income ///
    cfps_party ///
    urban14 provcd ///
    qc201 ///
    qea1 qea2 qga1 qga101 ku1m ku2 qq4010 ///
    qn8011 qn8012 qn12012 qn12014 ///
    qp202 qp702 qm603 kr410 kr405a ks502 ks801code ///
    occ5 isco isei siops egp sempl supvis

gen ex = qp702*60 if qp702>=0
label var ex "一周锻炼时长(分钟)"

rename cfps2014_age     m_age
rename cfps_gender      m_gender
rename cfps2014eduy     m_edu
rename income           m_income
rename urban14          m_urban
rename provcd           m_province
rename cfps_party       m_party
rename qc201            m_edu_expect

rename qea1             m_marriage_now
rename qea2             m_marriage_prev
rename qga1             m_fulltime
rename qga101           m_parttime
rename ku1m             m_mobile
rename ku2              m_internet
rename qq4010           m_sleep

rename qn8011           m_income_rank
rename qn8012           m_status_rank
rename qn12012          m_life_sat
rename qn12014          m_future_conf

rename qp202            m_health_change
rename ex               m_exercise
rename qm603            m_religion

rename kr410            m_tutor
rename kr405a           m_boarding
rename ks502            m_stress
rename ks801code        m_child_job_asp

*—— Rename occupation variables with m_ prefix
rename occ5             m_occ5
rename isco             m_isco
rename isei             m_isei
rename siops            m_siops
rename egp              m_egp
rename sempl            m_sempl
rename supvis           m_supvis

rename pid              mother_pid

drop if missing(mother_pid)
egen minfo = rownonmiss(m_isei m_occ5 m_isco m_siops m_egp m_sempl m_supvis m_age m_edu m_income)
bysort mother_pid (minfo): keep if _n==_N
drop minfo
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
flowappend, year(2014) step("Matched to at least one parent")

label var child_isei "子代ISEI"
label var f_isei     "父亲ISEI"
label var m_isei     "母亲ISEI"

*==================================================*
* Merge family economic data
*==================================================*

preserve
    use "`famecon'", clear

    keep ///
        fid14 ///
        total_asset house_debts resivalue ///
        fincome2 familysize ///
        fswt_natcs14 fswt_natpn1014

    rename fid14           fid
    rename total_asset     family_total_asset
    rename house_debts     family_house_debts
    rename resivalue       family_resivalue
    rename fincome2        family_income
    rename familysize      family_size
    rename fswt_natcs14    family_cs
    rename fswt_natpn1014  family_pn

    duplicates drop fid, force
    tempfile fe_ses
    save `fe_ses', replace
restore

merge m:1 fid using `fe_ses', keep(1 3)
gen has_famecon = (_merge==3)

preserve
    keep if has_famecon == 1
    flowappend, year(2014) step("Matched to family economic data")
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
    child_tutor child_boarding ///
    child_occ5 child_isco child_isei child_siops child_egp ///
    f_occ5 f_isco f_isei f_siops f_egp f_edu f_income f_age ///
    m_occ5 m_isco m_isei m_siops m_egp m_edu m_income m_age ///
    family_total_asset family_house_debts family_resivalue family_income family_size family_cs family_pn

foreach v of local dropneg {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
    }
}

preserve
    keep if !missing(child_isei)
    flowappend, year(2014) step("With child occupational outcome")
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
flowappend, year(2014) step("Yearly file saved")
save "${YOUT}\14_data.dta", replace

di _newline(2)
di "============================================================"
di "Data saved successfully!"
di "============================================================"
di "File: 14_data.dta"
di "Sample size: " _N
di "Number of variables: " c(k)
di "============================================================"
