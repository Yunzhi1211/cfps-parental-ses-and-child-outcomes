********************************************************************************
* Descriptive Statistics and Maps 
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
global SHP   "${PROJ}\0_raw\gadm41_CHN_1.shp"

* Automatically create folders
foreach dir in "${OUT}" "${OUT}\tab" "${OUT}\fig" "${OUT}\RDataUse" "${OUT}\maptemp" "${LOG}" {
    cap mkdir "`dir'"
}

cap log close
local t = subinstr("`c(current_time)'", ":", "_", .)
log using "${LOG}\analysis_descriptive_`c(current_date)'_`t'.log", replace text

set scheme s2color
graph set window fontface "Arial"

* Check and install required packages
foreach pkg in spmap shp2dta heatplot estout {
    capture which `pkg'
    if _rc ssc install `pkg', replace
}

*-------------------------------
* 2. Load the main dataset and save as a temporary file
*-------------------------------
use "${POUT}\Panel_data.dta", clear
keep if inlist(year, 2010,2012,2014,2016,2018,2020,2022) // Keep target years only
tempfile maindata
save `maindata', replace

*-------------------------------
* 3. Figure 3-1: Boxplot by year
*-------------------------------
graph box child_isei if !missing(child_isei), ///
    over(year, sort(1) label(angle(0) labsize(small))) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    title("Child ISEI by Year") ytitle("Child ISEI") ///
    graphregion(fcolor(white)) plotregion(margin(small))
graph export "${OUT}\fig\3_1_box_childisei_by_year.png", replace width(2400)

*-------------------------------
* 4. Figures 3-2 to 3-9: Seven yearly maps and one faceted combined map
*-------------------------------
* 4.1 Prepare geographic data
cap mkdir "${OUT}\maptemp"

shp2dta using "${SHP}", ///
    database("${OUT}\maptemp\map_prov_db") ///
    coordinates("${OUT}\maptemp\map_prov_coord") ///
    genid(_ID) replace



* 4.2 Compute provincial means and generate province codes
use `maindata', clear
keep if !missing(child_isei, child_province)
collapse (mean) mean_isei=child_isei (count) N=child_isei, by(child_province year)

* Apply a basic sample size threshold (filter out provinces with extremely small samples)
replace mean_isei = . if N < 50

* Selectively exclude frontier provinces with severe representativeness problems (force to blank/Missing)
* 54 = Tibet, 65 = Xinjiang
* Because Tibet, Xinjiang, and some other remote provinces have very small CFPS samples
* and substantial urban sampling bias (with insufficient coverage of herder/farmer populations),
* following existing literature, this study excludes them from province-level analysis in the map.
* Otherwise, the figure may misleadingly show Tibet as the darkest blue region,
* with child occupational status far above eastern provinces.
replace mean_isei = . if inlist(child_province, 54, 65) 

* Generate HASC_1 geographic matching code
gen str5 HASC_1 = ""
local provcode 11 12 13 14 15 21 22 23 31 32 33 34 35 36 37 41 42 43 44 45 46 50 51 52 53 54 61 62 63 64 65
local provstr "BJ TJ HE SX NM LN JL HL SH JS ZJ AH FJ JX SD HA HB HN GD GX HI CQ SC GZ YN XZ SN GS QH NX XJ"
local n : word count `provcode'
forvalues i = 1/`n' {
    local c : word `i' of `provcode'
    local s : word `i' of `provstr'
    replace HASC_1="CN.`s'" if child_province==`c'
}
drop if missing(HASC_1)

* Save the dataset with provincial means as a temporary file for maps and heatmaps
tempfile mapdata
save `mapdata', replace

* Also save a formal copy for R
save "${OUT}\RDataUse\mapdata_for_r.dta", replace
export delimited using "${OUT}\RDataUse\mapdata_for_r.csv", replace

* 4.3 Compute global cutoffs and colors
pctile brk = mean_isei, nq(6)
sum mean_isei, d
local min : di %4.1f r(min)
local max : di %4.1f r(max)
forval b = 1/5 {
    local b`b' : di %4.1f brk[`b']
}
local spmap_brks "`min' `b1' `b2' `b3' `b4' `b5' `max'"

local c1 "239 243 255"
local c2 "198 219 239"
local c3 "158 202 225"
local c4 "107 174 214"
local c5 "49 130 189"
local c6 "8 81 156"

*-------------------------------
* 4.4 Loop over years to generate maps
*-------------------------------
local i = 2
local glist ""

foreach yy in 2010 2012 2014 2016 2018 2020 2022 {
    
    * Step 1: Extract data for the current year and save as a temporary file
    use `mapdata', clear
    keep if year == `yy'
    tempfile temp_mapdata_`yy'
    save `temp_mapdata_`yy'', replace
    
    * Step 2: Load the map base file and merge with the year-specific temporary file
    use "${OUT}\maptemp\map_prov_db.dta", clear
    merge m:1 HASC_1 using `temp_mapdata_`yy'', keep(1 3) nogen

    * Year-specific map (with legend, exported as Figures 3-2 to 3-8)
    spmap mean_isei using "${OUT}\maptemp\map_prov_coord.dta", id(_ID) ///
        fcolor("`c1'" "`c2'" "`c3'" "`c4'" "`c5'" "`c6'") ///
        ocolor(gs10 ..) osize(vthin ..) ///
        ndfcolor(white) ndocolor(gs10) ndsize(vthin) ///
        clmethod(custom) clbreaks(`spmap_brks') ///
        legend(title("Mean ISEI", size(small)) pos(4) ring(0) size(vsmall) ///
               symy(2.5) symx(3) col(1) ///
               order(2 3 4 5 6 7 1) ///
               label(1 "Missing (N<50)") ///
               label(2 "`min' to `b1'") ///
               label(3 "`b1' to `b2'") ///
               label(4 "`b2' to `b3'") ///
               label(5 "`b3' to `b4'") ///
               label(6 "`b4' to `b5'") ///
               label(7 "`b5' to `max'") ///
               region(fcolor(white) lcolor(white))) ///
        title("Mean Child ISEI by Province (`yy')", size(medlarge) color(black)) ///
        graphregion(fcolor(white) lcolor(white)) name(map_`yy', replace)

    graph export "${OUT}/fig/3_`i'_map_simple_mean_childisei_`yy'.png", replace width(2400)
    
    * Year-specific map (without legend, stored in memory for combination)
    spmap mean_isei using "${OUT}\maptemp\map_prov_coord.dta", id(_ID) ///
        fcolor("`c1'" "`c2'" "`c3'" "`c4'" "`c5'" "`c6'") ///
        ocolor(gs10 ..) osize(vthin ..) ///
        ndfcolor(white) ndocolor(gs10) ndsize(vthin) ///
        clmethod(custom) clbreaks(`spmap_brks') ///
        legend(off) title("`yy'", size(medsmall) color(black)) ///
        graphregion(fcolor(white) lcolor(white)) name(g`yy', replace)
        
    local glist "`glist' g`yy'"
    local ++i
}


* 4.5 Create a combined legend and merge the faceted maps
twoway (scatteri 0 0, ms(S) mc("`c1'")) (scatteri 0 0, ms(S) mc("`c2'")) ///
       (scatteri 0 0, ms(S) mc("`c3'")) (scatteri 0 0, ms(S) mc("`c4'")) ///
       (scatteri 0 0, ms(S) mc("`c5'")) (scatteri 0 0, ms(S) mc("`c6'")) ///
       (scatteri 0 0, ms(S) mc(white) mlc(gs10) mlw(vthin)), ///
       legend(order(1 "`min' to `b1'" 2 "`b1' to `b2'" 3 "`b2' to `b3'" ///
                    4 "`b3' to `b4'" 5 "`b4' to `b5'" 6 "`b5' to `max'" 7 "Missing (N<50)") ///
              col(1) pos(0) size(small) symxsize(4) symysize(4) ///
              region(lcolor(none) fcolor(white)) title("Mean ISEI", size(medsmall) color(black))) ///
       xscale(off) yscale(off) graphregion(fcolor(white) lcolor(white)) name(g_legend, replace)

graph combine `glist' g_legend, rows(2) cols(4) ///
    title("Mean Child ISEI by Province, 2010–2022") ///
    graphregion(fcolor(white) lcolor(white))

graph export "${OUT}\fig\3_9_map_faceted_mean_childisei_2010_2022.png", replace width(5200)

*-------------------------------
* 5. Descriptive statistics tables (3 RTF files)
*-------------------------------
use `maindata', clear

* Get the list of variables to summarize
ds, has(type numeric)
local all_num_vars `r(varlist)'
local exclude_vars "year child_province pid fid father_province mother_province f_pid m_pid"
local sumvars : list all_num_vars - exclude_vars

* Table 3-1: Overall descriptive statistics for the pooled sample
est clear
estpost summarize `sumvars', detail

esttab using "${OUT}\tab\3_1_Descriptive_Stats_Pooled.rtf", replace ///
    cells("count(fmt(%12.0fc) label(N)) mean(fmt(%9.2f) label(Mean)) sd(fmt(%9.2f) label(SD)) min(fmt(%9.2f) label(Min)) max(fmt(%9.2f) label(Max))") ///
    nolabel nomtitle nonumber noobs /// 
    title("Descriptive Statistics (Pooled 2010-2022)")

* Table 3-2: Variable means by year
est clear
foreach y in 2010 2012 2014 2016 2018 2020 2022 {
    quietly estpost summarize `sumvars' if year == `y'
    est store yr`y'
}

esttab yr* using "${OUT}\tab\3_2_Descriptive_Stats_ByYear.rtf", replace ///
    cells(mean(fmt(%9.2f))) nolabel nonumber ///
    mtitle("2010" "2012" "2014" "2016" "2018" "2020" "2022") ///
    title("Variable Means by Year (2010-2022)") ///
    noobs

* Table 3-3: Main descriptive statistics table used in the paper
use "${POUT}\Panel_data.dta", clear
keep if inlist(year, 2010,2012,2014,2016,2018,2020,2022)

capture drop parent_party_any
gen byte parent_party_any = (f_party > 0 | m_party > 0) if !missing(f_party) | !missing(m_party)

capture drop parent_ses_pca
gen double parent_ses_pca = .

local pca_vars f_isei m_isei parent_party_any f_edu m_edu
local minN 20
levelsof year, local(yrs)

foreach yy of local yrs {
    tempvar touse_pca
    mark `touse_pca' if year == `yy'
    markout `touse_pca' child_isei `pca_vars' child_province

    quietly count if `touse_pca'
    if r(N) >= `minN' {
        capture quietly pca `pca_vars' if `touse_pca', components(1)
        if !_rc {
            tempvar pca_tmp
            quietly predict double `pca_tmp' if `touse_pca', score
            replace parent_ses_pca = `pca_tmp' if `touse_pca'
        }
    }
}

keep if !missing(child_isei, parent_ses_pca, child_gender, child_age, child_urban, child_province)

* English labels
label var child_isei       "Child ISEI"
label var parent_ses_pca   "Parent SES (PCA)"
label var f_isei           "Father ISEI"
label var m_isei           "Mother ISEI"
label var f_edu            "Father education"
label var m_edu            "Mother education"
label var parent_party_any "Any parent CPC"
label var child_gender     "Male"
label var child_age        "Age"
label var child_urban      "Urban"

estpost summarize child_isei parent_ses_pca f_isei m_isei f_edu m_edu parent_party_any ///
                  child_gender child_age child_urban

cap erase "${OUT}\tab\3_3_main_descriptive.rtf"
esttab using "${OUT}\tab\3_3_main_descriptive.rtf", replace ///
    cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) count(fmt(0))") ///
    label nonumber noobs nomtitle ///
    collabels("Mean" "SD" "Min" "Max" "N") ///
    title("Descriptive Statistics of Main Variables")
	
log close
