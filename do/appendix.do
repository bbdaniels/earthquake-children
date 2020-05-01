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
	, save("${directory}/outputs/TA1a_tested.xls") ///
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
	, save("${directory}/outputs/TA1b_measured.xls")	replace ///
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
		using "${directory}/outputs/TA1c_tested_rep.xls" ///
		, controls(hh_epidist hh_slope hh_district_1 hh_district_2 hh_district_3 i.indiv_male hh_logconscap i.indiv_age) ///
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
		using "${directory}/outputs/TA1d_measured_rep.xls" ///
		, 	controls(hh_epidist hh_slope hh_district_1 hh_district_2 hh_district_3 i.indiv_male hh_logconscap i.indiv_age) ///
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
		, save("${directory}/outputs/TA2a_selection.xls") replace below pvalue ///
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
	 local other_controls "hh_logconscap indiv_male i.indiv_age "
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
		, save("${directory}/outputs/TA2b_aid.xls") replace below ///
		lines(COL_NAMES 3 LAST_ROW 3) format((SCLB0) (SCCB0 NCRR2)) cnames("Any Aid" `theLabels') showeq ceq(${numbering}) ///
		keep(hh_faultdist `aid_controls') ///
		c("Constant") stats(N r2 mean) title("Table. Aid to Households") sheet("Table")

* Table A2c. Regressions per each subject
use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_age < 16

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male hh_logconscap i.indiv_age"

  	xisto Base if indiv_age >= 9	 	, clear		 command(regress) ///
  		depvar(indiv_theta_mean) rhs(hh_faultdist `fault_controls' `other_controls') cl(village_code)
  	xisto English if indiv_age >= 9	 	, 		 command(regress) ///
  		depvar(indiv_theta_pv_eng) rhs(hh_faultdist `fault_controls' `other_controls') cl(village_code)
  	xisto Math if indiv_age >= 9	 	, 		 command(regress) ///
  		depvar(indiv_theta_pv_mth) rhs(hh_faultdist `fault_controls' `other_controls') cl(village_code)
  	xisto Urdu if indiv_age >= 9	 	, 		 command(regress) ///
  		depvar(indiv_theta_pv_urd) rhs(hh_faultdist `fault_controls' `other_controls') cl(village_code)

	xitab ///
		using "${directory}/outputs/TA2c_subject.xls" ///
		, replace stats(mean)

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
	,	save ("${directory}/outputs/TA2d_bounds.xls")  replace


* Table A3a. Instrument, robustness, falsification

	use "${directory}/data/analysis_children.dta", clear

	keep if indiv_tested == 1 & indiv_age >= 9
	egen tag_mother = tag(censusid indiv_mother_id)

	keep if tag_mother == 1 // Keep mothers of tested children

	local fault_controls "hh_epidist hh_slope hh_district_1 hh_district_2 hh_district_3 hh_district_4"
	local other_controls "indiv_male hh_logconscap i.indiv_age"
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
		, save ("${directory}/outputs/TA3a_instrument.xls") ///
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
		local other_controls "hh_logconscap indiv_male i.indiv_age "
		local mother_controls "m_indiv_momedu_birthvil_logpop i.m_indiv_momedu_birthteh i.m_indiv_age"


		local stats_to_tab ///
				indiv_age indiv_health_height indiv_health_weight indiv_edu_binary indiv_father_edu ///
				indiv_school_enrolled_pre indiv_school_enrolled_post indiv_school_pri_bi_pre indiv_school_pri_bi_post ///
				hh_logconscap hh_assets_pca_post ///
				hh_stats_loggschool_post hh_stats_logmarket_post hh_stats_logdistrict_post hh_stats_logmedical_post hh_stats_logprischool_post

	* OLS

		reftab `stats_to_tab'	 ///
			 using "${directory}/outputs/TA3b_correlates_1.xls" ///
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
      , save("${directory}/outputs/TA3b_correlates_2.xls") replace ///
			lines(COL_NAMES 3 LAST_ROW 3) format((SCLB0) (SCCB0 NCRR2)) cnames(`theCols') showeq ceq(${numbering}) ///
			c("Constant") keep(m_indiv_edu_binary) stats(N r2 mean) title("Table 5c+. Child Characteristics by Maternal Primary Education (IV)") sheet("Table 5c+") ///
			note("Controlled for gender, age, household consumption per capita, and geographical characteristics.")


qui { // Mixed mitigation

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0

	clonevar a = m_indiv_edu_binary
		label var a "Yes"
	xtile hhalf = hh_aid_total , n(2)
		replace hhalf = hhalf - 1

	rename hh_faultdist f

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "indiv_male hh_logconscap i.indiv_age"

	qui {

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

		}

	xilab i.a*f  , three

	xml_tab miti1 miti2 miti3 miti4  miti5 miti6 ///
	using "${appendix}/A_mitigation.xls"  ///
	, title("Table A. Mitigation") sheet("Table 5d") replace below stats(N r2 mean) ///
		cnames("Maternal Mental Health" "Household Elevation" "Household Assets" "Maternal Mental Health" "Household Elevation" "Household Assets") ///
		format((S2110) (SCCB0 N2303)) lines(COL_NAMES 3 LAST_ROW 3) showeq ceq(${numbering})  c("Constant") ///
		keep(f _Ia_1 _Ia1Xf m_indiv_edu_binary indiv_male hh_logconscap  ) drop(o.*)  ///
		note("Controlled for geographical characteristics. Includes age dummies.", "Standard errors clustered by village.")
}

qui { // Mother's Education IV - no school choice

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & vil_schooluse == 1
	cap drop m_age
	clonevar m_age = m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop
	gen m_edu_fault = m_indiv_edu_binary * hh_faultdist

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male hh_logconscap i.indiv_age "
	local mother_controls "m_birthvil_logpop i.m_indiv_momedu_birthteh i.m_age"

	xi: reg indiv_theta_mean hh_faultdist ///
		m_indiv_edu_binary  ///
		`fault_controls' `other_controls'  ///
		if indiv_age >= 9, first cl(village_code)

		est sto reg1

	xi: reg indiv_theta_mean hh_faultdist ///
		m_indiv_edu_binary m_edu_fault   ///
		`fault_controls' `other_controls'  ///
		if indiv_age >= 9, first cl(village_code)

		est sto reg2

	xi: ivreg2 indiv_theta_mean hh_faultdist ///
		( m_indiv_edu_binary = instrument )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age >= 9, first cl(village_code)

		est sto reg3
		estadd scalar f = `e(widstat)'
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	xi: ivreg2 indiv_theta_mean hh_faultdist ///
		( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age >= 9, first cl(village_code)

		est sto reg4
		estadd scalar f = `e(widstat)'
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	gen f = 0
		label var f "First Stage F-stat"
	gen m = 0
		label var m "Dependent Variable Mean"

	xml_tab ///
		reg1 reg2 reg3 reg4 ///
		using "${appendix}/A_schoolchoice.xls" ///
		, replace below stats(N f m ) ///
			keep( hh_faultdist m_indiv_edu_binary m_edu_fault _Iindiv_mal_1 hh_logconscap)
}

} // end tables

** Figures

qui { // IV Leverage

	/*
	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_tested == 1
	cap drop m_age
	clonevar m_age = m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male hh_logconscap i.indiv_age"
	local mother_controls "m_birthvil_logpop i.m_indiv_momedu_birthteh i.m_age"

	clonevar i_d = i_instrument_faultdist

		xi: ivreg2 indiv_theta_mean hh_faultdist ///
			( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
			`fault_controls' `other_controls' `mother_controls' ///
			if indiv_age >= 9, first cl(village_code)


		gen trueBeta = _b[m_edu_fault]

	keep if e(sample)

	egen mgroup = group(censusid indiv_mother_id)
	egen tag_mother = tag(censusid indiv_mother_id)
	egen group = group(village_code)

	qui su group
		local theN = `r(max)'
	cap drop b_alt
	gen b_alt = .
	gen b_min = .
	gen b_max = .
	qui forv i = 1/`theN' {
		ivregress 2sls indiv_theta_mean hh_faultdist ///
			( m_indiv_edu_binary m_edu_fault  = instrument i_d ) ///
			`fault_controls' `other_controls' `mother_controls' ///
			if group != `i' ///
			, cl(village_code)
			noi noi di "`i'/`theN' done!"

    mat a = r(table)
      local lower = a[5,2]
      local upper = a[6,2]

		replace b_alt = _b[m_edu_fault] if group == `i'
		replace b_min = `lower' if group == `i'
		replace b_max = `upper' if group == `i'
		}

    egen vtag=tag(village_code)
	save "${directory}/Dofiles/shock/leverage.dta" , replace
	*/
		use "${directory}/Dofiles/shock/leverage.dta" , clear

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
		local other_controls "i.indiv_male hh_logconscap i.indiv_age"
		local mother_controls "m_birthvil_logpop i.m_indiv_momedu_birthteh i.m_age"

		sort b_alt
		local theid = group[1723]
      di `theid'

    ivregress 2sls indiv_theta_mean hh_faultdist ///
			( m_indiv_edu_binary m_edu_fault  = instrument i_d ) ///
			`fault_controls' `other_controls' `mother_controls'

      local beta = _b[m_edu_fault]

		ivregress 2sls indiv_theta_mean hh_faultdist ///
			( m_indiv_edu_binary m_edu_fault  = instrument i_d ) ///
			`fault_controls' `other_controls' `mother_controls' ///
			if group != `theid'

			local min = _b[m_edu_fault]
			mat a = r(table)
				local lower = a[5,2]
				local upper = a[6,2]

    cap drop rand
    egen rand = group(b_max)
    tw (rspike b_min b_max rand , lw(thin) lc(gray)) ///
      (scatter b_alt rand , msize(tiny) mc(black) )  ///
      (scatter b_alt rand if b_max > 0 , msize(tiny) mc(red) ) ///
    if vtag == 1  ///
    , yline(0) xscale(off) yscale(noline) xsize(7) ///
      ylab(-0.2 -0.1  `beta' "Main {&beta}" 0 "Zero")

    /* OLD VERSION
		tw (histogram b_alt if vtag == 1 , w(0.001) s(0) freq lc(black) fc(black)) ///
      (pci 20 `lower' 20 `upper', lc(gray)) ///
      (scatteri 20 `min' "Least Significant LOO Estimate & 95% CI",  mc(black)  mlabpos(6)) ///
    ,  ///
			xtit("Estimated {&beta}s for mitigation coefficient") ///
			note("") ytit("Count of villages") ///
			xlab(-0.2 -0.1  `beta' "Main Specification" 0 "Zero")
    */

		graph export "${appendix}/A_leverage.png" , replace width(4000)
}

qui { // IV Leverage -- including maternal birth village

	/*
	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_tested == 1

  tostring m_birthvil, format(%15.0g) gen(temp)
  encode temp, gen(bvil)

	cap drop m_age
	clonevar m_age = m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male hh_logconscap i.indiv_age"
	local mother_controls "m_birthvil_logpop i.bvil i.m_age"

	clonevar i_d = i_instrument_faultdist

		xi: ivreg2 indiv_theta_mean hh_faultdist ///
			( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
			`fault_controls' `other_controls' `mother_controls' ///
			if indiv_age >= 9, first cl(village_code)


		gen trueBeta = _b[m_edu_fault]

	keep if e(sample)

	egen mgroup = group(censusid indiv_mother_id)
	egen tag_mother = tag(censusid indiv_mother_id)
	egen group = group(village_code)

	qui su group
		local theN = `r(max)'
	cap drop b_alt
	gen b_alt = .
	qui forv i = 1/`theN' {
		ivregress 2sls indiv_theta_mean hh_faultdist ///
			( m_indiv_edu_binary m_edu_fault  = instrument i_d ) ///
			`fault_controls' `other_controls' `mother_controls' ///
			if group != `i' ///
			, cl(village_code)
			noi noi di "`i'/`theN' done!"

		replace b_alt = _b[m_edu_fault] if group == `i'
		}

	save "${directory}/Dofiles/shock/leverage-bvil.dta" , replace
	*/
		use "${directory}/Dofiles/shock/leverage-bvil.dta" , clear

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
		local other_controls "i.indiv_male hh_logconscap i.indiv_age"
		local mother_controls "m_birthvil_logpop i.bvil i.m_age"

		sort b_alt
		local theid = group[1716]
      di `theid'

		ivregress 2sls indiv_theta_mean hh_faultdist ///
			( m_indiv_edu_binary m_edu_fault  = instrument i_d ) ///
			`fault_controls' `other_controls' `mother_controls' ///
			if group != `theid'

			local min = _b[m_edu_fault]
			mat a = r(table)
				local lower = a[5,2]
				local upper = a[6,2]

		su trueBeta

		egen vtag = tag(village_code)
		kdensity b_alt if vtag == 1  ///
		, ${graph_opts} lc(black) ///
			xtit("Estimated {&beta} for mitigation coefficient {&rarr}") ///
			title("") note("") ylab(none) ytit("{&larr} Relative Density of LOO Estimates {&rarr}") ///
			xlab(`lower' " " `upper' " " -.15 -.075 `min' " " 0 ) ///
			xline(`min' , lc(red)) xline(`lower',lc(black) lp(dash) ) xline(`upper',lc(black) lp(dash) )

		graph export "${appendix}/A_leverage-bvil.png" , replace width(1000)
}

qui { // Test questions

	use "${directory}/data/analysis_children.dta", clear
	keep indiv_age censusid memid

	merge 1:1 censusid memid using "$directory/Data/Primary/Survey/test_scores/score_sheet.dta", keep(3) nogen

	recode pic* eng* urd* mth* wrd* (9=.)
	local opts lw(thick) degree(1)

	tw (lpoly eng1_a indiv_age, `opts') ///
		(lpoly eng5_b indiv_age, `opts') ///
		(lpoly mth1 indiv_age, `opts') ///
		(lpoly mth4_a indiv_age, `opts') ///
		(lpoly urd2_a indiv_age, `opts') ///
		(lpoly urd4_b indiv_age, `opts') ///
	, $graph_opts ///
		ylab($xpct) xsize(7) xlab(7(1)15) legend(on ring(0) c(1) pos(5) textfirst ///
			order(3 "Indicate which box has more items" 1 `"Match "banana" picture to English word"' 5 "Combine letters correctly in Urdu" ///
			4 "Answer 4 x 5 = ??" 6 `"Use the word "support" in an Urdu sentence"' 2 `"Use the word "play" in an English sentence"')) xtit("Age During Survey {&rarr}")

		graph export "${appendix}/A_questions.png", replace width(4000)
}

qui { // Placebo faultline tests

	* Test scores

		insheet using "${directory}/Outputs/Shock/Placebo/distance_to_faults.csv" , clear

		tempfile distances
			save `distances'

		use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_age < 16

		// gen m_edu_fault = m_indiv_edu_binary * hh_faultdist

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
		local other_controls "indiv_male hh_logconscap i.indiv_age"

		merge m:1 censusid using `distances' , keep(3)


		cap mat drop betas
		forv i = 1/50 {

		gen m_edu_fault`i' = m_indiv_edu_binary * fault_`i'

		qui reg indiv_theta_mean ///
				fault_`i' hh_faultdist ///
				m_edu_fault`i' m_edu_fault ///
				m_indiv_edu_binary `fault_controls' `other_controls' ///
				if indiv_age >= 9	, cl(village_code)

				scalar beta = _b[fault_`i']
				scalar true = _b[hh_faultdist]
				scalar betam = _b[m_edu_fault`i']
				scalar truem = _b[m_edu_fault]

				mat betas = nullmat(betas) \ [beta , true , betam , truem]

		}

		reg indiv_theta_mean ///
				hh_faultdist m_edu_fault ///
				m_indiv_edu_binary `fault_controls' `other_controls' ///
				if indiv_age >= 9	, cl(village_code)

				local trueBeta = _b[hh_faultdist]

	 	clear
		mat tests = betas
		svmat tests

		set obs 51
			replace tests1 = `trueBeta' in 51

		gen v1 = _n

		tempfile tests
			save `tests'

	* Child height

		insheet using "${directory}/Outputs/Shock/Placebo/distance_to_faults.csv" , clear

		tempfile distances
			save `distances'

		use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_age < 16

		// gen m_edu_fault = m_indiv_edu_binary * hh_faultdist

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
		local other_controls "indiv_male hh_logconscap i.indiv_age"

		merge m:1 censusid using `distances' , keep(3)


		cap mat drop betas
		forv i = 1/50 {

		gen m_edu_fault`i' = m_indiv_edu_binary * fault_`i'

		qui reg indiv_health_zanthro_height ///
				fault_`i' hh_faultdist ///
				m_edu_fault`i' m_edu_fault ///
				m_indiv_edu_binary `fault_controls' `other_controls' ///
				if indiv_age <= 6	, cl(village_code)

				scalar beta = _b[fault_`i']
				scalar true = _b[hh_faultdist]
				scalar betam = _b[m_edu_fault`i']
				scalar truem = _b[m_edu_fault]

				mat betas = nullmat(betas) \ [beta , true , betam , truem]

		}


		reg indiv_health_zanthro_height ///
				hh_faultdist m_edu_fault ///
				m_indiv_edu_binary `fault_controls' `other_controls' ///
				if indiv_age <= 6	, cl(village_code)

				local trueBeta = _b[hh_faultdist]

		clear

			insheet using "${directory}/Outputs/Shock/Placebo/faults_length.csv" , clear

			tempfile lengths
				save `lengths'

		clear
		mat height = betas
		svmat height

		set obs 51
			replace height1 = `trueBeta' in 51

		gen v1 = _n

		merge 1:1 v1 using `tests' , nogen

		merge 1:1 v1 using `lengths' , nogen
			replace length = 170.4165 in 51
			replace fault = ""
			replace fault = "{&larr} Balakot-Bagh (Activated)" in 51

		* Balakot-Bagh

      ellip height1 tests1 in 1/50 ///
        , ${graph_opts} constant(6) title("") subtitle("") note("") ///
        plot((scatter height1 tests1 [w = length] in 1/51 ///
					, ms(Oh) msize(*.5) mc(black)) ///
				      (scatter height1 tests1 [w = length] in 51 ///
					, ms(X) msize(vhuge) mc(maroon) ml(fault))) ///
        xtit(,placement(center)) xsize(8) ///
				xtit("{&larr} Test Score Distance Coefficient (SD/km) {&rarr}") ///
				ytit("{&larr} Height Distance Coefficient (SD/km) {&rarr}") ///
				yline(0, lc(gray)) xline(0, lc(gray)) xlab(-0.02(0.01)0.04) ///
				legend(on r(2) pos(5) ring(0) symxsize(small) symysize(small) size(small) textfirst ///
					order(2 "Estimated coefficients from all faults:" 1 "95% joint confidence ellipse boundary:"))

        graph export "${appendix}/A_placebo.png" ///
					, replace width(4000)

    /* Without ellipse
			tw ///
				(scatter height1 tests1 [w = length] in 1/51 ///
					, ms(Oh) msize(*.5) mc(black)) ///
				(scatter height1 tests1 [w = length] in 51 ///
					, ms(X) msize(vhuge) mc(maroon) ml(fault)) ///
			, ${graph_opts} xtit(,placement(center)) xsize(8) ///
				xtit("{&larr} Test Score Distance Coefficient (SD/km) {&rarr}") ///
				ytit("{&larr} Height Distance Coefficient (SD/km) {&rarr}") ///
				yline(0, lc(gray)) xline(0, lc(gray)) ///
				legend(r(2) pos(5) ring(0) symxsize(small) symysize(small) size(small) textfirst ///
					order(2 "Estimated coefficients from activated fault:" 1 "Counterfactual coefficients from other faults:"))

				graph export "${appendix}/A_placebo.png" ///
					, replace width(4000)
    */


	/*
		gen p = 1 - (betas2 > betas1)
			mean p [pweight = length]
			mat theP = e(b)
			local theP = theP[1,1]
			local theP = round(`theP',0.001)
			local theB = round(`trueBeta',0.001)

		kdensity betas1 [aweight=length] ///
		, ${graph_opts} xscale(line)  xline(`trueBeta') title("Height-for-age (In Utero and Age 0-2)") ///
			note("") xtit("Distribution of coefficients estimated from placebo faults {&rarr}") ///
			xlab(0 "Zero" `trueBeta' `""{&beta}=`theB'" "(p=`theP')""') lw(thick) lc(black) ylab(none)

		graph save placebo_height.gph , replace

		kdensity betas1 [aweight=length] ///
		, ${graph_opts} xscale(line) xline(`trueBeta') title("Test Scores") ///
			note("") xtit("Distribution of coefficients estimated from placebo faults {&rarr}") ///
			xlab(0 "Zero" `trueBeta' `""{&beta}=`theB'" "(p=`theP')""') lw(thick) lc(black) ylab(none)


			graph save placebo_scores.gph , replace

	* combine

		graph combine ///
			placebo_scores.gph ///
			placebo_height.gph ///
		, c(2) xcom graphregion(color(white) lc(white) lw(med) la(center)) ${comb_opts} xsize(7)

		graph export "${appendix}/A_placebo.png" ///
			, replace width(4000)

	*/
}

// Effect Size Illustration

  import excel using "${directory}/data/shock_magnitudes.xlsx" , clear

  gen E = -A

  graph hbar E ,  xsize(7) over(C, sort((mean) E)) blab(bar, format(%02.1f)) ///
    ytit("") yline(0.70 ) yline(1.031 0.329 , lp(dash)) ///
    ylab(0 0.70 `" "Estimated" "Effect" "'  1.031  `" "Upper" "Bound" "'  0.329  `" "Lower" "Bound" "' )

    graph export "${appendix}/A_effect.png" , width(4000) replace

qui { // Maternal education interactions

	use "${directory}/data/analysis_children.dta"  if m_missing == 0, clear

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "indiv_male hh_logconscap i.indiv_age"

	cd "$directory/outputs/shock/temp/"

	reg indiv_health_zanthro_height hh_faultdist m_indiv_edu_binary##indiv_agecat##c.hh_faultdist ///
		`fault_controls' `other_controls' , cluster(village_code)

	qui margins m_indiv_edu_binary if indiv_agecat == 1, at(hh_faultdist=(0(1)50))
	marginsplot, recast(line) recastci(rarea)  $graph_opts title("Height (In Utero)" , pos(12))  ///
		xtit("Distance to Activated Fault (km) {&rarr}")  ///
		plot1opts(lc(black) lp(dash) lw(medthick)) plot2opts(lc(black) lp(solid) lw(medthick)) ciopts(lw(thin) lc(gs14) fc(gs14) fi(100)) ///
		legend(on order(4 "Mother Completed Primary School" 3 "No Educated Mother") ring(0) pos(7) c(1))

		graph save a.gph , replace

	qui margins m_indiv_edu_binary if indiv_agecat == 2, at(hh_faultdist=(0(1)50))
	marginsplot, recast(line) recastci(rarea)  $graph_opts title("Height (Age 0-2)" , pos(12))  ///
		xtit("Distance to Activated Fault (km) {&rarr}")  ///
		plot1opts(lc(black) lp(dash) lw(medthick)) plot2opts(lc(black) lp(solid) lw(medthick)) ciopts(lw(thin) lc(gs14) fc(gs14) fi(100)) ///
		legend(on order(4 "Mother Completed Primary School" 3 "No Educated Mother") ring(0) pos(7) c(1))

		graph save b.gph , replace

		reg indiv_theta_mean hh_faultdist m_indiv_edu_binary##c.hh_faultdist ///
			`fault_controls' `other_controls' if indiv_age >= 9 , cluster(village_code)

	qui margins m_indiv_edu_binary , at(hh_faultdist=(0(1)50))
	marginsplot, recast(line) recastci(rarea)  $graph_opts title("Test Scores" , pos(12))  ///
		xtit("Distance to Activated Fault (km) {&rarr}")  ///
		plot1opts(lc(black) lp(dash) lw(medthick)) plot2opts(lc(black) lp(solid) lw(medthick)) ciopts(lw(thin) lc(gs14) fc(gs14) fi(100)) ///
		legend(on order(4 "Mother Completed Primary School" 3 "No Educated Mother") ring(0) pos(7) c(1))

		graph save c.gph , replace

    grc1leg c.gph a.gph b.gph , r(1) ycom

    graph save mitigation.gph , replace

		graph combine mitigation.gph  ///
			, graphregion(color(white)) xsize(7)

		graph export "${appendix}/A_momedu.png" ///
			, replace width(4000)
}

/* OLD Figure 8. Testing and maternal education and age cuts

	use "${directory}/data/analysis_children.dta"  if m_missing == 0, clear

	keep if indiv_theta_mean != .
  recode m_indiv_edu_binary (1=1 "Yes")(0=0 "No") , gen(m)
  egen type = group(indiv_school_enrolled_pre m) , label lname(type)

  tw ///
    (histogram hh_faultdist if indiv_theta_mean != . ///
        , s(0) w(2) gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
    (lfitci indiv_theta_mean hh_faultdist if type == 1 ///
      , lc(black) lw(thick) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
    (lfitci indiv_theta_mean hh_faultdist if type == 2 ///
      , lc(black) lw(thick) lp(longdash) astyle(ci) fc(gray%50) alc(%0)) ///
    (lfitci indiv_theta_mean hh_faultdist if type == 3 ///
      , lc(black) lw(thick) lp(shortdash) astyle(ci) fc(gray%50) alc(%0)) ///
    (lfitci indiv_theta_mean hh_faultdist if type == 4 ///
      , lc(black) lw(thick) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
    if hh_faultdist < 60 ///
    , ${graph_opts} ${hist_opts} xsize(7) ///
      xtit("Distance from Activated Fault (km) {&rarr}") ///
      ylab(0 "Mean Score" .5 "+0.5 SD" -.5 "-0.5 SD" -1 "-1.0 SD") ///
      legend(on span c(1) pos(12) region(lc(none)) size(small) ///
        order(9 "Child Enrolled and Mother Educated" 7 "Child Enrolled, Mother Not Educated" ///
              5 "Child Not Enrolled, Mother Educated" 3 "Child Not Enrolled and Mother Not Educated" ))

		graph save "$directory/Outputs/shock/raw/F8a_edu-enrollment.gph", replace

	* Age cut

		use "/Users/bbdaniels/Dropbox/Research/Earthquake/Constructed/analysis_children.dta", clear

    keep if indiv_theta_mean != .

    gen ageup = indiv_age >= 9
    recode m_indiv_edu_binary (1=1 "Yes")(0=0 "No") , gen(m)

    egen type = group(ageup m) , label lname(type)

    tw ///
      (histogram hh_faultdist if indiv_theta_mean != . ///
        , s(0) w(2) gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
      (lfitci indiv_theta_mean hh_faultdist if type == 1 ///
        , lc(black) lw(thick) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
      (lfitci indiv_theta_mean hh_faultdist if type == 2 ///
        , lc(black) lw(thick) lp(longdash) astyle(ci) fc(gray%50) alc(%0)) ///
      (lfitci indiv_theta_mean hh_faultdist if type == 3 ///
        , lc(black) lw(thick) lp(shortdash) astyle(ci) fc(gray%50) alc(%0)) ///
      (lfitci indiv_theta_mean hh_faultdist if type == 4 ///
        , lc(black) lw(thick) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
      if hh_faultdist < 60 ///
      , ${graph_opts} ${hist_opts} xsize(7) ///
        xtit("Distance from Activated Fault (km) {&rarr}") ///
        ylab(0 "Mean Score" .5 "+0.5 SD" -.5 "-0.5 SD" -1 "-1.0 SD") ///
        legend(on span c(1) pos(12) region(lc(none)) size(small) ///
          order(9 "Child 5+ and Mother Educated" 7 "Child 5+, Mother Not Educated" ///
                5 "Child Under 5, Mother Educated" 3 "Child Under 5 and Mother Not Educated" ))

      graph save "$directory/Outputs/shock/raw/F8b_edu-age.gph", replace

  * Combined

    graph combine ///
      "$directory/Outputs/shock/raw/F8a_edu-enrollment.gph" ///
      "$directory/Outputs/shock/raw/F8b_edu-age.gph" ///
      , c(2) xsize(7)

      graph export "$directory/Outputs/shock/raw/F8_edu-samples.png", replace width(4000)
*/

* Table 4b CLONE. Mother's Education IV with BVFE

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0
	cap drop m_age
	clonevar m_age = m_indiv_age
  gen agesq = m_indiv_age*m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "indiv_male hh_logconscap b9.indiv_age "
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
	, save("${appendix}/A_bvfe.xls") ///
	  replace below stats(N f m ) ///
		keep( hh_faultdist m_indiv_edu_binary m_edu_fault instrument id)



** Have a lovely day!
