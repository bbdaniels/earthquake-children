** Earthquake appendix

* Table A1a. Testing coverage

	use "${directory}/data/analysis_children.dta", clear

	keep if indiv_dead == 0 & indiv_age < 16 & indiv_age > 6

	replace indiv_test_status = 5 if indiv_test_status == .
	replace indiv_test_status = 6 if m_missing == 1 & indiv_test_status == 0
		label def indiv_test_status 6 "Tested but Missing Mother" , modify

	ta indiv_test_status indiv_age ///
	, matcell(test_completion)

	mat i = J(colsof(test_completion),1,1)
		mat test_completion = test_completion , test_completion*i
	mat i = J(1,rowsof(test_completion),1)
		mat test_completion = test_completion \ i*test_completion

	matrix rownames test_completion = "Tested" "Temporarily Away" "No Longer in HH" "Disabled" "Working" "No Reason Given" "Tested but Missing Mother" "Total"
	matrix colnames test_completion = 7 8 9 10 11 12 13 14 15 Total

	mat test_completion = test_completion'

	xml_tab test_completion ///
	, save("${directory}/appendix/TA1a_tested.xls") replace ///
		title("Table A1. Test Completion by Age") sheet("Table A1") ///
		lines(COL_NAMES 3 9 2 LAST_ROW 3)  format( (SCLB0)  (SCCB0 NCRR0))


* Table A1b. Measurement Coverage

	use "${directory}/data/analysis_children.dta", clear
	cap mat drop _all
	keep if indiv_dead == 0 & indiv_age < 16

	gen measnomiss = indiv_measured == 1 & m_missing == 1

	forvalues i = 3/15 {
		qui count if indiv_age == `i'
		mat theTotal = nullmat(theTotal) \ [`r(N)']
		qui count if indiv_measured == 1 & m_missing == 0 & indiv_age == `i'
		mat theMeasured = nullmat(theMeasured) \ [`r(N)']
		qui count if indiv_measured == 1 & m_missing == 1 & indiv_age == `i'
		mat theMissing = nullmat(theMissing) \ [`r(N)']
		qui sum indiv_health_height if indiv_age == `i'
		mat theCM = nullmat(theCM) \ [`r(mean)']
		qui sum indiv_health_zanthro_height if indiv_age == `i'
		mat theHA = nullmat(theHA) \ [`r(mean)']
		qui sum indiv_health_weight if indiv_age == `i'
		mat theKG = nullmat(theKG) \ [`r(mean)']
		qui sum indiv_health_zanthro_weight if indiv_age == `i'
		mat theWA = nullmat(theWA) \ [`r(mean)']
		}

	qui count if indiv_age > 2
		mat more = [`r(N)']
	qui count if indiv_measured == 1 & m_missing == 0
		mat more = more , [`r(N)']
	qui count if indiv_measured == 1 & m_missing == 1
		mat more = more , [`r(N)']

	qui sum indiv_health_height
		mat more = more , [`r(mean)']
	qui sum indiv_health_zanthro_height
		mat more = more , [`r(mean)']
	qui sum indiv_health_weight
		mat more = more , [`r(mean)']
	qui sum indiv_health_zanthro_weight
		mat more = more , [`r(mean)']

	mat measured = theTotal , theMeasured , theMissing , theCM , theHA , theKG , theWA
	mat measured = measured \ more

	matrix colnames measured = "Eligible" "Measured and Matched" "Measured and Unmatched" "Mean Height (cm)" "Mean Height-for-Age" "Mean Weight (kg)" "Mean Weight-for-Age"
	matrix rownames measured = 3 4 5 6 7 8 9 10 11 12 13 14 15 Total

	xml_tab measured ///
	, save("${directory}/appendix/TA1b_measured.xls")	replace ///
		title("Table A1b. Measurement Completion by Age") sheet("Table A1b") ///
		lines(COL_NAMES 3 13 2 LAST_ROW 3)  format( (SCLB0)  (SCCB0 NCRR2))

* Table A1c. Tested Representative Sample

	use "${directory}/data/analysis_children.dta", clear

	keep if m_missing == 0 & indiv_dead == 0 & indiv_age < 16 & indiv_age > 6

	* Create false observations for representative sample checks (tested vs all)
		expand 2 if indiv_tested == 1, gen(false)
		replace indiv_tested = 0 if false == 1

		local stats_to_tab ///
			indiv_male indiv_age indiv_health_height indiv_health_weight ///
			hh_assets_pca_post indiv_edu_binary indiv_father_edu m_indiv_edu_binary m_indiv_age ///
			indiv_school_enrolled_pre_t indiv_school_enrolled_post_t indiv_school_pri_bi_pre_t indiv_school_pri_bi_post_t

		reftab `stats_to_tab'	 ///
		using "${directory}/appendix/TA1c_tested_rep.xls" ///
		, controls(hh_epidist hh_slope hh_district_1 hh_district_2 hh_district_3 i.indiv_male i.indiv_age) ///
			by(indiv_tested) refcat(0) se n replace ///
			title("Table 4A1c. Tested Children Representative Sample") sheet("Table A1c") ///
			lines(COL_NAMES 3 LAST_ROW 3)  dec(2 2 2 2 2 2 2 2 2 2 2 2 2 )

* Table A1d. Measured Sample

	use "${directory}/data/analysis_children.dta", clear
	cap mat drop _all
	keep if m_missing == 0 & indiv_dead == 0 & indiv_age < 16 & indiv_age > 2

	* Create false observations for representative sample checks (tested vs all)
		expand 2 if indiv_measured == 1, gen(false)
		replace indiv_measured = 0 if false == 1

		local stats_to_tab ///
			indiv_male indiv_age ///
			hh_assets_pca_post indiv_edu_binary indiv_father_edu m_indiv_edu_binary m_indiv_age ///
			indiv_school_enrolled_pre_t indiv_school_enrolled_post_t indiv_school_pri_bi_pre_t indiv_school_pri_bi_post_t

		reftab `stats_to_tab'	 ///
		using "${directory}/appendix/TA1d_measured_rep.xls" ///
		, 	controls(hh_epidist hh_slope hh_district_1 hh_district_2 hh_district_3 i.indiv_male i.indiv_age) ///
			by(indiv_measured) refcat(0) se n replace ///
			title("Table A1d. Measured Children Representative Sample") sheet("Table A1d") ///
			lines(COL_NAMES 3 LAST_ROW 3)  dec(2 2 2 2 2 2 2 2 2 2 2 2 2 )

* Table A2a. Migration and mortality selection

  use "${directory}/data/analysis_all.dta" if indiv_age >=3, clear

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
		local other_controls "indiv_male i.indiv_age hh_assets_pca_pre"


    gen isdead = indiv_time_of_death == 1
    gen isdead2 = indiv_time_of_death == 2 | indiv_time_of_death == 3 if indiv_time_of_death != 1
    gen agesq = indiv_age*indiv_age
      lab var agesq "Age Squared"
    gen interaction = hh_assets_pca_pre * hh_faultdist

    reg isdead hh_faultdist indiv_male hh_assets_pca_pre interaction indiv_age agesq `fault_controls' , cl(village_code)

      est sto dead
      su isdead if e(sample)
      local mean = `r(mean)'
      estadd scalar mean = `mean'

    reg isdead2 hh_faultdist indiv_male hh_assets_pca_pre interaction indiv_age agesq `fault_controls' , cl(village_code)

      est sto dead2
      su isdead2 if e(sample)
      local mean = `r(mean)'
      estadd scalar mean = `mean'

	local theVarlist ///
		 indiv_movein indiv_moveout

	foreach var in `theVarlist'  {

		use "${directory}/data/analysis_all.dta", clear

			xi: reg `var' hh_faultdist `fault_controls' `other_controls'  ///
				if indiv_age >= 16 ///
				, cl(village_code)
				est sto `var'_ad

				qui sum `var' if indiv_age >= 16
				local mean = `r(mean)'
				estadd scalar mean = `mean'

		use "${directory}/data/analysis_children.dta" if indiv_age < 16 & indiv_age >=3, clear

			gen indiv_moveout = indiv_in_hh_pre == 1 & indiv_in_hh_post == 0 if indiv_in_hh_pre != . & indiv_dead == 0
				label var indiv_moveout "Migrated Out"
			gen indiv_movein = indiv_in_hh_pre == 0 & indiv_in_hh_post == 1 if indiv_in_hh_pre != . & indiv_dead == 0
				replace indiv_movein = 0 if indiv_age < 4 & indiv_in_hh_pre != . & indiv_dead == 0
				label var indiv_movein "Migrated In"

			xi: reg `var' hh_faultdist `fault_controls' `other_controls'  ///
				, cl(village_code)
				est sto `var'_ch

				qui sum `var'
				local mean = `r(mean)'
				estadd scalar mean = `mean'

				local theLabel : var label `var'
		}

	xml_tab dead dead2 indiv_moveout_ad indiv_movein_ad indiv_moveout_ch indiv_movein_ch ///
		, save("${directory}/appendix/TA2a_selection.xls") replace below pvalue ///
		lines(COL_NAMES 3 LAST_ROW 3) format((SCLB0) (SCCB0 NCRR3))  ///
		cnames("Earthquake Mortality" "Later Mortality" "Adult Out Migration" "Adult In Migration" "Child Out Migration" "Child In Migration" ) showeq ceq(${numbering}) ///
		c("Constant") stats(N r2 mean) title("Table A2. Migration after the Earthquake") sheet("Table A2") drop(o.* _Iindiv_age_* _Ihh_distri_* hh_fault_minimum)

* Table A2b. Aid Distribution Regressions

	use "${directory}/data/analysis_all.dta", clear

		bys censusid : egen check = min(hh_aid_total)
			replace hh_aid_total = check
		clonevar hh_aid_any_7 = hh_aid_total
			replace hh_aid_any_7 = 0 if hh_aid_total == .

	 local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	 local mother_controls "m_height m_heightmiss "
	 local other_controls "indiv_male i.indiv_age "
	 local aid_controls "hh_head_female hh_mom_edu hh_dad_edu hh_familysize hh_assets_pca_pre hh_stats_destroyed hh_*_eligible hh_n_children_u6"

	local theList ""

	su hh_aid_any if tag_hh
		local theMean = `r(mean)'
	reg hh_aid_any hh_faultdist `fault_controls' `aid_controls' if tag_hh, cluster(village_code)
		est sto hh_aid_any
			estadd scalar mean = `theMean'

  qui su hh_aid_any_?

	forvalues i= 1/7 {
		su hh_aid_any_`i' if tag_hh
			local theMean = `r(mean)'
		reg hh_aid_any_`i' hh_faultdist `fault_controls' `aid_controls' if tag_hh, cluster(village_code)
			est sto hh_aid_any_`i'
			estadd scalar mean = `theMean'

		local theList = "`theList' hh_aid_any_`i'"

		local theLabel : var label hh_aid_any_`i'
			local theLabels `"`theLabels' "`theLabel'""'
		}

	xml_tab hh_aid_any `theList' ///
		, save("${directory}/appendix/TA2b_aid.xls") replace below ///
		lines(COL_NAMES 3 LAST_ROW 3) format((SCLB0) (SCCB0 NCRR2)) cnames("Any Aid" `theLabels') showeq ceq(${numbering}) ///
		keep(hh_faultdist `aid_controls') ///
		c("Constant") stats(N r2 mean) title("Table. Aid to Households") sheet("Table")

* Table A2c. Regressions per each subject
use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_age < 16

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "indiv_male i.indiv_age"

  foreach type in indiv_theta_mean indiv_theta_pv_eng indiv_theta_pv_mth indiv_theta_pv_urd {

    reg `type' hh_faultdist ///
      `fault_controls' `other_controls' ///
    if indiv_age >= 9 ///
    , cl(village_code)

    est sto `type'
      qui sum `type'
      local mean = `r(mean)'
      estadd scalar mean = `mean'
  }

  	xml_tab indiv_theta_mean indiv_theta_pv_eng indiv_theta_pv_mth indiv_theta_pv_urd ///
	  , save("${directory}/appendix/TA2c_subject.xls") ///
			replace below c("Constant") stats(mean r2 N) ///
      lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
      keep(hh_faultdist hh_epidist hh_slope indiv_male _cons)

* Table A2d. Lee bounding

	cap mat drop results
	use "${directory}/data/analysis_children.dta" , clear

	leebounds indiv_theta_mean hh_near_quake ///
		if indiv_age >= 9 & indiv_age <= 15 & m_missing == 0
		mat results = r(table)

	leebounds indiv_health_zanthro_height hh_near_quake ///
		if indiv_age <= 6 & indiv_age >= 3 &  m_missing == 0
		mat results = results , r(table)

	mat results_STARS = J(rowsof(results),colsof(results),0)
	xml_tab results ///
	,	save ("${directory}/appendix/TA2d_bounds.xls")  replace


* Table A3a. Instrument, robustness, falsification

	use "${directory}/data/analysis_children.dta", clear

	keep if indiv_tested == 1 & indiv_age >= 9
	egen tag_mother = tag(censusid indiv_mother_id)

	keep if tag_mother == 1 // Keep mothers of tested children

	local fault_controls "hh_epidist hh_slope hh_district_1 hh_district_2 hh_district_3 hh_district_4"
	local other_controls "indiv_male i.indiv_age"
	local mother_controls "m_indiv_momedu_birthvil_logpop i.m_indiv_momedu_birthteh i.m_indiv_age"

	replace m_indiv_education_level = 0 if m_indiv_education_level == 55
	reg m_indiv_education_level m_eligible_2 `mother_controls' hh_faultdist , cl(village_code)

	// Regresssions
	reg m_indiv_edu_binary m_eligible_2 `mother_controls' hh_faultdist , cl(village_code)
		estimates store fal1
		test m_eligible_2
		local f = round(r(F),.01)
		estadd scalar f = `f'

	xi: reg m_indiv_edu_binary m_eligible_2 `mother_controls' hh_faultdist if m_indiv_momedu_false8!=0 , cl(village_code)
		estimates store fal2
		test m_eligible_2
		local f = round(r(F),.01)
		estadd scalar f = `f'

	reg m_indiv_edu_binary m_eligible_2 `fault_controls' `mother_controls' hh_faultdist , cl(village_code)
		estimates store fal3
		test m_eligible_2
		local f = round(r(F),.01)
		estadd scalar f = `f'

	reg m_indiv_edu_binary m_indiv_sb8 `fault_controls' `mother_controls' hh_faultdist hh_logcons if m_eligible_2!=. , cl(village_code)
		estimates store fal4
		test m_indiv_sb8
		local f = round(r(F),.01)
		estadd scalar f = `f'

	reg m_indiv_edu_binary m_eligible_2 m_eligible_3 m_eligible_4 `fault_controls' `mother_controls' hh_faultdist hh_logcons if instrument!=., cl(village_code)
		estimates store fal5
		test m_eligible_2
		local f = round(r(F),.01)
		estadd scalar f = `f'

	areg m_indiv_edu_binary m_eligible_2 `fault_controls' `mother_controls' hh_faultdist hh_logcons if instrument!=., cl(village_code) a(m_birthvil)
		estimates store fal6
		test m_eligible_2
		local f = round(r(F),.01)
		estadd scalar f = `f'

	// xi: reg m_indiv_edu_binary m_eligible_2 `fault_controls' `mother_controls' hh_faultdist hh_logcons i.m_birthvil if instrument!=., cl(village_code)

	gen f = 0
		label var f "F-statistic for Age 9 School Availability" // labelling for output

		xml_tab fal1 fal2 fal3 fal4 fal5 fal6 ///
		, save ("${directory}/appendix/TA3a_instrument.xls") ///
      title("Table A. Instrument Falsification Tests and First Stage F-tests (Dependent variable: Probability of completing primary school)") ///
			replace below cnames("Instrument" "Recieved School Sometime" "Geographical Controls" "Boys' School" "Girls' School (Other Ages)" "Birth Village FE") c("Constant") ///
			showeq ceq($numbering) stats(N f) format((S2110) (SCCB0 N2303)) lines(COL_NAMES 3 LAST_ROW 3) ///
			keep( hh_faultdist m_eligible_2 m_indiv_sb8 m_eligible_3 m_eligible_4  _cons) drop(o.*) ///
			note("Controlled for individual and geographical characterics.", "Standard errors clustered by village.")

// A3b. Maternal Education correlates

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_tested == 1
	set matsize 2000

	* Setup

		clonevar i_d = i_instrument_faultdist

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
		local other_controls "indiv_male i.indiv_age "
		local mother_controls "m_indiv_momedu_birthvil_logpop i.m_indiv_momedu_birthteh i.m_indiv_age"


		local stats_to_tab ///
				indiv_age indiv_health_height indiv_health_weight indiv_edu_binary indiv_father_edu ///
				indiv_school_enrolled_pre indiv_school_enrolled_post indiv_school_pri_bi_pre indiv_school_pri_bi_post ///
				hh_assets_pca_post ///
				hh_stats_loggschool_post hh_stats_logmarket_post hh_stats_logdistrict_post hh_stats_logmedical_post hh_stats_logprischool_post

	* OLS

		reftab `stats_to_tab'	 ///
			 using "${directory}/appendix/TA3b_correlates_1.xls" ///
			if indiv_childage < 16 & indiv_age >= 9 ///
			, 	by(m_indiv_edu_binary) controls(hh_faultdist `fault_controls' `other_controls') refcat(0) se n replace ///
				title("Table 5c. Child Characteristics by Maternal Primary Education") sheet("Table rc") ///
				lines(COL_NAMES 3 LAST_ROW 3)  dec(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)

	* IV

		qui ivregress 2sls indiv_theta_mean hh_faultdist ///
		 	( m_indiv_edu_binary m_edu_fault  = instrument i_d ) ///
			`fault_controls' `other_controls' `mother_controls' ///
			if indiv_age >= 9, cl(village_code)

			keep if e(sample) == 1

		local theCols ""
		qui foreach var of varlist  `stats_to_tab' {

				local theLabel : var label `var'
				local theCols `"`theCols' "`theLabel'" "`theLabel'""'

				local temp_controls = regexr("`other_controls'","`var'","")
				local temp_controls = regexr("`temp_controls'","i. ","")
				xi: ivregress 2sls `var' hh_faultdist ///
					( m_indiv_edu_binary m_edu_fault  = instrument i_d ) ///
					`fault_controls' `temp_controls' `mother_controls', cl(village_code)


				est sto `var'

				}

		xml_tab `stats_to_tab' ///
      , save("${directory}/appendix/TA3b_correlates_2.xls") replace ///
			lines(COL_NAMES 3 LAST_ROW 3) format((SCLB0) (SCCB0 NCRR2)) cnames(`theCols') showeq ceq(${numbering}) ///
			c("Constant") keep(m_indiv_edu_binary) stats(N r2 mean) title("Table 5c+. Child Characteristics by Maternal Primary Education (IV)") sheet("Table 5c+") ///
			note("Controlled for gender, age, household consumption per capita, and geographical characteristics.")

* A3c. Regressions with birth village FE

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0
	cap drop m_age
	clonevar m_age = m_indiv_age
  gen agesq = m_indiv_age*m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "indiv_male b9.indiv_age "
	local mother_controls "i.bv m_birthvil_logpop b19.m_age"

  egen bv = group(m_indiv_momedu_birthvil) if m_indiv_momedu_birthvil < .
  clonevar id = i_instrument_faultdist

  // No clustering
    reg indiv_theta_mean hh_faultdist instrument  ///
      `fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9

      est sto rf1
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

    reg indiv_theta_mean hh_faultdist instrument id ///
      `fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9

      est sto rf2
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

  	ivregress 2sls indiv_theta_mean hh_faultdist  ///
  		( m_indiv_edu_binary   = instrument  )  ///
  		`fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9

      est sto iv1
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

    ivregress 2sls indiv_theta_mean hh_faultdist  ///
  		( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
  		`fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9

      est sto iv2
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

    // Clustering
    reg indiv_theta_mean hh_faultdist instrument  ///
      `fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9 , cl(village_code)

      est sto rf12
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

    reg indiv_theta_mean hh_faultdist instrument id ///
      `fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9 , cl(village_code)

      est sto rf22
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

  	ivregress 2sls indiv_theta_mean hh_faultdist  ///
  		( m_indiv_edu_binary   = instrument  )  ///
  		`fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9 , cl(village_code)

      est sto iv12
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

    ivregress 2sls indiv_theta_mean hh_faultdist  ///
  		( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
  		`fault_controls' `other_controls' `mother_controls' ///
  		if indiv_age >= 9 , cl(village_code)

      est sto iv22
  		su `e(depvar)' if e(sample)
  		estadd scalar m = `r(mean)'

	gen m = 0
		label var m "Dependent Variable Mean"

	xml_tab ///
		rf1 rf2 rf12 rf22 iv1 iv2 iv12 iv22 ///
	, save("${directory}/appendix/TA3c_bvfe.xls") ///
	  replace below stats(N f m ) ///
		keep( hh_faultdist m_indiv_edu_binary m_edu_fault instrument id)

* Table A3d. Mother's Education IV - no school choice

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & vil_schooluse == 1
	cap drop m_age
	clonevar m_age = m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male i.indiv_age "
	local mother_controls "m_birthvil_logpop i.m_indiv_momedu_birthteh i.m_age"

	xi: reg indiv_theta_mean hh_faultdist ///
		m_indiv_edu_binary  ///
		`fault_controls' `other_controls'  ///
		if indiv_age >= 9, cl(village_code)

		est sto reg1
    su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	xi: reg indiv_theta_mean hh_faultdist ///
		m_indiv_edu_binary m_edu_fault   ///
		`fault_controls' `other_controls'  ///
		if indiv_age >= 9, cl(village_code)

		est sto reg2
    su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	xi: ivregress 2sls indiv_theta_mean hh_faultdist ///
		( m_indiv_edu_binary = instrument )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age >= 9,  cl(village_code)

		est sto reg3
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	xi: ivregress 2sls indiv_theta_mean hh_faultdist ///
		( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age >= 9,  cl(village_code)

		est sto reg4
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	gen m = 0
		label var m "Dependent Variable Mean"

	xml_tab ///
		reg1 reg2 reg3 reg4 ///
		, save("${directory}/appendix/TA3d_schoolchoice.xls") replace below stats(N m ) ///
			keep( hh_faultdist m_indiv_edu_binary m_edu_fault _Iindiv_mal_1)

* Table A3e. Alternative mitigation

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0

	clonevar a = m_indiv_edu_binary
		label var a "Yes"
	xtile hhalf = hh_aid_total , n(2)
		replace hhalf = hhalf - 1

	rename hh_faultdist f

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "indiv_male i.indiv_age"

		replace a = m_mentalhealth_binary

		xi3: reg indiv_theta_mean i.a*f ///
			`fault_controls' `other_controls' m_indiv_edu_binary ///
			if indiv_age >= 9 ///
			, cluster(village_code)
			est sto miti1

			qui sum indiv_theta_mean if e(sample)
			local mean = `r(mean)'
			estadd scalar mean = `mean'

		replace a = hh_elevation_binary

		xi3: reg indiv_theta_mean i.a*f ///
			`fault_controls' `other_controls' m_indiv_edu_binary ///
			if indiv_age >= 9 ///
			, cluster(village_code)
			est sto miti2

			qui sum indiv_theta_mean if e(sample)
			local mean = `r(mean)'
			estadd scalar mean = `mean'

		replace a = hh_assets_upper

		xi3: reg indiv_theta_mean i.a*f ///
			`fault_controls' `other_controls' m_indiv_edu_binary ///
			if indiv_age >= 9 ///
			, cluster(village_code)
			est sto miti3

			qui sum indiv_theta_mean if e(sample)
			local mean = `r(mean)'
			estadd scalar mean = `mean'

			replace a = m_mentalhealth_binary

		xi3: reg indiv_health_zanthro_height i.a*f ///
			`fault_controls' `other_controls' m_indiv_edu_binary ///
			if indiv_age <= 6 ///
			, cluster(village_code)
			est sto miti4

			qui sum indiv_health_zanthro_height if e(sample)
			local mean = `r(mean)'
			estadd scalar mean = `mean'

		replace a = hh_elevation_binary

		xi3: reg indiv_health_zanthro_height i.a*f ///
			`fault_controls' `other_controls' m_indiv_edu_binary ///
			if indiv_age <= 6 ///
			, cluster(village_code)
			est sto miti5

			qui sum indiv_health_zanthro_height if e(sample)
			local mean = `r(mean)'
			estadd scalar mean = `mean'

		replace a = hh_assets_upper

		xi3: reg indiv_health_zanthro_height i.a*f ///
			`fault_controls' `other_controls' m_indiv_edu_binary ///
			if indiv_age <= 6 ///
			, cluster(village_code)
			est sto miti6

			qui sum indiv_health_zanthro_height if e(sample)
			local mean = `r(mean)'
			estadd scalar mean = `mean'

	xilab i.a*f  , three

	xml_tab miti1 miti2 miti3 miti4  miti5 miti6 ///
	, save("${directory}/appendix/TA3e_mitigation.xls") ///
    title("Table A. Mitigation") sheet("Table 5d") replace below stats(N r2 mean) ///
		cnames("Maternal Mental Health" "Household Elevation" "Household Assets" "Maternal Mental Health" "Household Elevation" "Household Assets") ///
		format((S2110) (SCCB0 N2303)) lines(COL_NAMES 3 LAST_ROW 3) showeq ceq(${numbering})  c("Constant") ///
		keep(f _Ia_1 _Ia1Xf m_indiv_edu_binary indiv_male) drop(o.*)  ///
		note("Controlled for geographical characteristics. Includes age dummies.", "Standard errors clustered by village.")

// Appendix table A4a. Alternative specifications of Table 3: Aid as control.
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  replace hh_aid_total = hh_aid_total/100000
    lab var hh_aid_total "Total Cash Aid Reported (PKR 100,000s)"

  char indiv_age[omit] 9

  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "hh_aid_total i.indiv_male i.indiv_age"

  xi: reg indiv_health_zanthro_weight hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg1
    su `e(depvar)' if e(sample) == 1
    estadd scalar mean = `r(mean)'

  xi: reg indiv_health_zanthro_height hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg2
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_school_enrolled_post hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg3
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_edu hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg4
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg5
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    disr `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg6
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_male*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg7
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_age*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg8
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 ///
  , save("${directory}/appendix/TA4a_aid.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist disr _IageXhh_fa_1 _IageXhh_fa_2 hh_aid_total _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)


// Appendix table A4b. Alternative specifications of Table 3: Control sensitivity
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  // Reference
  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "i.indiv_male i.indiv_age"

    xi: reg indiv_health_zanthro_height hh_faultdist ///
      i.agecat*hh_faultdist `fault_controls' `other_controls' ///
    , cl(village_code)

      estimates store reg1
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

    xi: reg indiv_theta_mean hh_faultdist ///
      `fault_controls' `other_controls' ///
    if indiv_age >= 9 ///
    , cl(village_code)

      estimates store reg2
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'
			
  // Remove gender
  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "i.indiv_age"

    xi: reg indiv_health_zanthro_height hh_faultdist ///
      i.agecat*hh_faultdist `fault_controls' `other_controls' ///
    , cl(village_code)

      estimates store reg1a
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

    xi: reg indiv_theta_mean hh_faultdist ///
      `fault_controls' `other_controls' ///
    if indiv_age >= 9 ///
    , cl(village_code)

      estimates store reg2a
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'
			
	// Remove Age
	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls ""

		xi: reg indiv_health_zanthro_height hh_faultdist ///
			i.agecat*hh_faultdist `fault_controls' `other_controls' ///
		, cl(village_code)

			estimates store reg1b
			su `e(depvar)' if e(sample)
			estadd scalar mean = `r(mean)'

		xi: reg indiv_theta_mean hh_faultdist ///
			`fault_controls' `other_controls' ///
		if indiv_age >= 9 ///
		, cl(village_code)

			estimates store reg2b
			su `e(depvar)' if e(sample)
			estadd scalar mean = `r(mean)'

  // Remove Geography
  local fault_controls "hh_district_1 hh_district_2 hh_district_3"
  local other_controls ""

	  xi: reg indiv_health_zanthro_height hh_faultdist ///
	    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
	  , cl(village_code)

	    estimates store reg1c
	    su `e(depvar)' if e(sample)
	    estadd scalar mean = `r(mean)'

	  xi: reg indiv_theta_mean hh_faultdist ///
	     hh_epidist `fault_controls' `other_controls' ///
	  if indiv_age >= 9 ///
	  , cl(village_code)

	    estimates store reg2c
	    su `e(depvar)' if e(sample)
	    estadd scalar mean = `r(mean)'
			
	// Remove District FE
  local fault_controls ""
  local other_controls ""

    xi: reg indiv_health_zanthro_height hh_faultdist ///
      i.agecat*hh_faultdist `fault_controls' `other_controls' ///
    , cl(village_code)

      estimates store reg1d
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

    xi: reg indiv_theta_mean hh_faultdist ///
      `fault_controls' `other_controls' ///
    if indiv_age >= 9 ///
    , cl(village_code)

      estimates store reg2d
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'
		
  // Output results

	  xml_tab ///
		  reg2 reg2a reg2b reg2c reg2d   ///
	  , save("${directory}/appendix/TA4b_controls1.xls") ///
	    replace below c("Constant") stats(mean r2 N) ///
	    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
	    keep(hh_faultdist )
			
	  xml_tab ///
		  reg1 reg1a reg1b reg1c reg1d   ///
	  , save("${directory}/appendix/TA4b_controls2.xls") ///
	    replace below c("Constant") stats(mean r2 N) ///
	    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
	    keep(hh_faultdist _IageXhh_fa_1 _IageXhh_fa_2)

// Appendix table A4c. Alternative specifications of Table 3: Intensity replacing distance.
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  replace hh_faultdist = hh_intensity

  char indiv_age[omit] 9

  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "i.indiv_male i.indiv_age"

  xi: reg indiv_health_zanthro_weight hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg1
    su `e(depvar)' if e(sample) == 1
    estadd scalar mean = `r(mean)'

  xi: reg indiv_health_zanthro_height hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg2
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_school_enrolled_post hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg3
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_edu hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg4
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg5
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    disr `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg6
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_male*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg7
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_age*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg8
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 ///
  , save("${directory}/appendix/TA4c_mercalli.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist disr _IageXhh_fa_1 _IageXhh_fa_2 _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)

// Appendix table A4d. Alternative specifications of Table 3: Birth cohort selection
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  char indiv_age[omit] 9

  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "indiv_father_age indiv_father_edu indiv_father_height m_age m_indiv_edu_binary m_indiv_health_height i.hh_occ_code i.indiv_male i.indiv_age"

  xi: reg indiv_health_zanthro_weight hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg1
    su `e(depvar)' if e(sample) == 1
    estadd scalar mean = `r(mean)'

  xi: reg indiv_health_zanthro_height hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg2
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_school_enrolled_post hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg3
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_edu hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg4
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg5
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    disr `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg6
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_male*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg7
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_age*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg8
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 ///
  , save("${directory}/appendix/TA4d_cohort.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist disr _IageXhh_fa_1 _IageXhh_fa_2 _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)

// Appendix table A4d. Alternative specifications of Table 3: Birth cohort selection
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  char indiv_age[omit] 9

  local fault_controls "density hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "i.indiv_male i.indiv_age"

  xi: reg indiv_health_zanthro_weight hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg1
    su `e(depvar)' if e(sample) == 1
    estadd scalar mean = `r(mean)'

  xi: reg indiv_health_zanthro_height hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg2
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_school_enrolled_post hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg3
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_edu hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg4
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg5
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    disr `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg6
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_male*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg7
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_age*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg8
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 ///
  , save("${directory}/appendix/TA4e_density.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist density disr _IageXhh_fa_1 _IageXhh_fa_2 _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)
				
// End tables
