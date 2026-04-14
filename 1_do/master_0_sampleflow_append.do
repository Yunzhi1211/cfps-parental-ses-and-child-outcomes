capture program drop flowappend
program define flowappend
    syntax , YEAR(integer) STEP(string)

    preserve

        quietly count
        local n_rows = r(N)

        local n_pid = .
        capture confirm variable pid
        if !_rc {
            egen __tag_pid = tag(pid)
            quietly count if __tag_pid == 1
            local n_pid = r(N)
            drop __tag_pid
        }

        local n_fid = .
        capture confirm variable fid
        if !_rc {
            egen __tag_fid = tag(fid)
            quietly count if __tag_fid == 1
            local n_fid = r(N)
            drop __tag_fid
        }

        tempfile newobs

        clear
        set obs 1
        gen int year = `year'
        gen str60 step = "`step'"
        gen long n_rows = `n_rows'
        gen long n_pid  = `n_pid'
        gen long n_fid  = `n_fid'
        save `newobs', replace

        capture confirm file "${TAB}\sampleflow_all_years.dta"
        if _rc {
            use `newobs', clear
            save "${TAB}\sampleflow_all_years.dta", replace
        }
        else {
            use "${TAB}\sampleflow_all_years.dta", clear
            append using `newobs'
            save "${TAB}\sampleflow_all_years.dta", replace
        }

    restore
end


capture program drop flowexport
program define flowexport
    capture confirm file "${TAB}\sampleflow_all_years.dta"
    if _rc {
        di as error "sampleflow_all_years.dta not found."
        exit 601
    }

    use "${TAB}\sampleflow_all_years.dta", clear
    order year step n_rows n_pid n_fid
    sort year step
    save "${TAB}\sampleflow_all_years.dta", replace
    export excel using "${TAB}\sampleflow_all_years.xlsx", firstrow(variables) replace
    export delimited using "${TAB}\sampleflow_all_years.csv", replace

    preserve
        collapse (sum) n_rows n_pid n_fid, by(step)

        gen order = .
        replace order = 1  if step == "Raw adult sample"
        replace order = 2  if step == "Valid unique respondents"
        replace order = 3  if step == "Intergenerational link identified"
        replace order = 4  if step == "Matched to at least one parent"
        replace order = 5  if step == "Matched to family economic data"
        replace order = 6  if step == "With child occupational outcome"
        replace order = 7  if step == "Yearly file saved"
        replace order = 8  if step == "Pooled panel sample"
        replace order = 9  if step == "Pooled: with child ISEI"
        replace order = 10 if step == "Pooled: with parental SES"
        replace order = 11 if step == "Final baseline regression sample"

        sort order
        drop order

        save "${TAB}\sampleflow_total.dta", replace
        export excel using "${TAB}\sampleflow_total.xlsx", firstrow(variables) replace
        export delimited using "${TAB}\sampleflow_total.csv", replace
    restore
end
