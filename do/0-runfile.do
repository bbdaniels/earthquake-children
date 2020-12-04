** Runfile

* Set directory: the location of the downloaded repository

  global directory "/Users/bbdaniels/GitHub/earthquake-children"

* Graph scheme: https://graykimbrough.github.io/uncluttered-stata-graphs/

  cd "${directory}/ado/"
  set scheme uncluttered

* Load adofiles only from here

  sysdir set PLUS "${directory}/ado/"

* Global options

	global graph_opts ///
		title(, justification(left) color(black) span pos(11)) ///
		graphregion(color(white) lc(white) la(center)) ///
		ylab(,angle(0) nogrid) xtit(,placement(left) justification(left)) ///
		yscale(noline) xscale(noline) legend(region(lc(none) fc(none)))

	global comb_opts graphregion(color(white) lc(white) la(center))
	global hist_opts ylab(, format(%9.0f) angle(0) axis(2)) yscale(noline alt axis(2)) ytit("Frequency (Histogram)", axis(2)) ytit(, axis(1)) yscale(alt)
	global xpct `" 0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%" "'
	global numbering `""(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)""'

/* Get all required data from Box and install packages // TODO: DELETE WHEN FINALIZED

  // Packages
    ssc install xml_tab  , replace
    net install forest   , from(https://github.com/bbdaniels/stata/raw/master/) replace
    net install sumstats   , from(https://github.com/bbdaniels/stata/raw/master/) replace
    net install grc1leg  , from(http://www.stata.com/users/vwiggins) replace
    net install st0085_2 , from(http://www.stata-journal.com/software/sj14-2) replace
    net install st0030_2 , from(http://www.stata-journal.com/software/sj5-4) replace
    net install st0364   , from(http://www.stata-journal.com/software/sj14-4) replace
    net install gr32_1   , from(http://www.stata-journal.com/software/sj4-3) replace

  // Data
  global data "/Users/bbdaniels/Box/Earthquake/Constructed"
	qui do "${directory}/ado/iecodebook.ado"

	  foreach dta in analysis_hh analysis_children analysis_all {

	    iecodebook export ///
	      "${data}/`dta'.dta" ///
	    using "${directory}/data/`dta'.xlsx" ///
	    , replace reset copy hash ///
	      trim("${directory}/do/0-runfile.do" ///
	        "${directory}/do/1-figures.do" ///
	        "${directory}/do/2-tables.do" ///
	        "${directory}/do/3-appendix-figures.do" ///
	        "${directory}/do/4-appendix-tables.do")
	  }

    // Food Prices Data
    copy "${data}/food-prices.dta" "${directory}/data/prices.dta" , replace
    copy "${data}/school-density.dta" "${directory}/data/schools.dta" , replace

    // Analytical Cleaning
		use "${directory}/data/analysis_all.dta" , clear
		  keep if indiv_head_relation == 1 & indiv_occupation != .
			keep censusid indiv_occupation
			ren indiv_occupation hh_occupation
			  lab var hh_occupation "Head of Household Occupation"
				
		  tempfile occupation
			save `occupation' 
		
    use "${directory}/data/analysis_children.dta" , clear
      merge m:1 censusid using "${data}/mercalli.dta" , nogen keep(3)
      merge m:1 censusid using "`occupation'" , nogen 

      gen m_edu_fault = m_indiv_edu_binary * hh_faultdist
        lab var m_edu_fault "Fault-Edu Interaction"
      clonevar agecat = indiv_agecat
      recode indiv_education_level (55=0)(20=.) , gen(indiv_edu)
        lab var indiv_edu "Highest Completed Education"
      clonevar disr = indiv_school_disruption
      gen hh_aid = hh_aid_total / 10000
        lab var hh_aid "Total Reported Aid (Rs. 10,000s)"
    save "${directory}/data/analysis_children.dta" , replace
*/

* Run all program files by setting flag to (1)
if (1) qui {
  do "${directory}/do/1-figures.do"
  do "${directory}/do/2-tables.do"
  do "${directory}/do/3-appendix-figures.do"
  do "${directory}/do/4-appendix-tables.do"
}

* End of master do-file
