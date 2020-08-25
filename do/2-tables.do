* Revision of tables for eathquake shock paper

* Table 1.  Descriptive Statistics
use "${directory}/data/analysis_all.dta"  , clear

	gen u15 = indiv_age < 16 & indiv_age > 2
		label var u15 "In Utero - Age 11 During Earthquake"

	label var touse_shock "Selected for Detailed Survey"
	label var indiv_school_enrolled_post "Enrolled During Survey (Age 1+ During Earthquake)"

	merge m:1 censusid using "${directory}/data/analysis_hh.dta" , keepusing(hh_aid_total) update replace
		label var hh_aid_total "Total Cash Aid"

	label var instrument "Mother's School Access Instrument"
	label var indiv_father_age "Father's Age"
	label var hh_consumption_food "Annual Food Expenditure (PKR)"
	label var hh_consumption_nonfood "Annual Non-Food Expenditure (PKR)"
	label var indiv_school_enrolled_pre "School Enrollment During Earthquake"

	sumstats ///
		/// Geography
			(hh_faultdist hh_epidist hh_fault_minimum if tag_hh == 1) ///
			(hh_slope if tag_uc == 1) /// UC Data – slope
			(hh_district_1 hh_district_2 hh_district_3 hh_district_4 if tag_hh == 1 & touse_shock == 1) ///
		/// Death, Destruction, Aid
			(c_death_quake c_home_dam c_home_des if tag_hh == 1) /// Census
			(hh_aid_any hh_aid_any_1 hh_aid_total if tag_hh == 1 & touse_shock == 1) /// Survey
		/// Socioeconomics
			(hh_familysize hh_consumption_food hh_consumption_nonfood ///
			hh_assets_pca_pre hh_n_children_u6 hh_head_female ///
			if tag_hh == 1 & touse_shock == 1)  ///
		/// Living Individuals
			(indiv_male indiv_age u15 if indiv_dead == 0 ) ///
		/// Children In Utero – Age 11
			(indiv_agecat_2 indiv_agecat_3 indiv_agecat_1 ///
			indiv_health_height indiv_health_weight ///
			indiv_school_enrolled_pre indiv_school_enrolled_post indiv_school_pri_bi_post ///
			if indiv_dead == 0 & touse_shock == 1 & indiv_age > 2 & indiv_age < 16) ///
		/// Childrens' parents
			(m_indiv_edu_binary m_indiv_age m_indiv_health_height instrument ///
				indiv_father_edu  indiv_father_age indiv_father_height ///
			if indiv_dead == 0 & touse_shock == 1 & indiv_age > 2 & indiv_age < 16) ///
		/// Print
		using "$directory/outputs/T1_descriptives.xls" ///
	,  	replace stats(mean sd p25 p50 p75 N)


* Table 2a. Pre-quake Exogeneity (slow)

	use "${directory}/data/analysis_all.dta" , clear

	* Fault LHS regression and F-stat for paper

	/*
		reg vil_uc_dfl_mean vil_t39v3 vil_t39v4 vil_t39v5 vil_t39v11 vil_t39v6 vil_edu_primary vil_fem_secondary vil_t39v23 vil_t39v18 vil_t39v22 vil_t39v21 vil_infra /// village stats
				indiv_age indiv_edu_primary /// indiv stats ()
				hh_stats_electricity_pre hh_water_inhouse_pre hh_perm_house_pre /// hh stats
				hh_stats_market_pre hh_stats_water_dist_pre hh_stats_medical_pre hh_stats_privateschool_pre hh_stats_govtschool_pre  ///
				hh_distance_pre_1 hh_distance_pre_2 hh_distance_pre_6 hh_distance_pre_4 hh_distance_pre_5 ///
			 , cl(village_code)

		test vil_t39v3 vil_t39v4 vil_t39v5 vil_t39v11 vil_t39v6 vil_edu_primary vil_fem_secondary vil_t39v23 vil_t39v18 vil_t39v22 vil_t39v21 vil_infra /// village stats
				indiv_age indiv_edu_primary /// indiv stats ()
				hh_stats_electricity_pre hh_water_inhouse_pre hh_perm_house_pre /// hh stats
				hh_stats_market_pre hh_stats_water_dist_pre hh_stats_medical_pre hh_stats_privateschool_pre hh_stats_govtschool_pre  ///
				hh_distance_pre_1 hh_distance_pre_2 hh_distance_pre_6 hh_distance_pre_4 hh_distance_pre_5
	*/

	* Fault RHS regressions for table

	local theVarlist_v /// Village
		vil_t39v3 vil_t39v4 vil_t39v5 vil_t39v11 vil_t39v6 vil_edu_primary vil_fem_secondary vil_t39v23 vil_t39v18 vil_t39v22 vil_t39v21 vil_infra

		local theCols ""
	qui foreach var of varlist `theVarlist_v' {

		xi: reg `var' vil_uc_dfl_mean vil_uc_dist_epi vil_uc_slope_mean i.hh_district if tag_village == 1 ///
			, cl(village_code)
			est sto `var'

			local theLabel : var label `var'
			local theCols `"`theCols' "`theLabel'" "`theLabel'""'

			qui sum `var'
			local mean = `r(mean)'
			estadd scalar mean = `mean'

			}

	local theVarlist_i /// Individual
		indiv_male_height indiv_female_height indiv_male_age indiv_female_age indiv_edu_primary_m indiv_edu_primary_f

	qui foreach var of varlist `theVarlist_i' {

		xi: reg `var' hh_faultdist hh_epidist hh_slope i.hh_district if indiv_dead == 0 & indiv_age > 17 ///
			, cl(village_code)
			est sto `var'

			local theLabel : var label `var'
			local theCols `"`theCols' "`theLabel'" "`theLabel' (Living)""'

			qui sum `var' if indiv_dead == 0 & indiv_age > 17
			local mean = `r(mean)'
			estadd scalar mean = `mean'

			}

	qui foreach var of varlist indiv_male_age indiv_female_age indiv_edu_primary_m indiv_edu_primary_f {

		xi: reg `var' hh_faultdist hh_epidist hh_slope i.hh_district if indiv_dead == 1 & indiv_age > 17 ///
			, cl(village_code)
			est sto `var'2

			local theLabel : var label `var'
			local theCols `"`theCols' "`theLabel'" "`theLabel' (Deceased)""'

			qui sum `var' if indiv_dead == 1 & indiv_age > 17
			local mean = `r(mean)'
			estadd scalar mean = `mean'

			}

	qui foreach var of varlist indiv_male_age indiv_female_age indiv_edu_primary_m indiv_edu_primary_f {

		xi: reg `var' hh_faultdist hh_epidist hh_slope i.hh_district if indiv_age > 17 ///
			, cl(village_code)
			est sto `var'3

			local theLabel : var label `var'
			local theCols `"`theCols' "`theLabel'" "`theLabel' (All)""'

			qui sum `var' if indiv_dead == 1 & indiv_age > 17
			local mean = `r(mean)'
			estadd scalar mean = `mean'

			}

	local theVarlist_h /// Household
		hh_stats_electricity_pre hh_water_inhouse_pre hh_perm_house_pre /// hh stats
		hh_stats_market_pre hh_stats_water_dist_pre hh_stats_medical_pre hh_stats_privateschool_pre hh_stats_govtschool_pre  ///
		hh_distance_pre_1 hh_distance_pre_2 hh_distance_pre_6 hh_distance_pre_4 hh_distance_pre_5

	qui foreach var of varlist `theVarlist_h' {

		xi: reg `var' hh_faultdist hh_epidist hh_slope i.hh_district if tag_hh == 1 & touse_shock == 1 ///
			, cl(village_code)
			est sto `var'

			local theLabel : var label `var'
			local theCols `"`theCols' "`theLabel'" "`theLabel'""'

			qui sum `var'
			local mean = `r(mean)'
			estadd scalar mean = `mean'

			}

	xml_tab `theVarlist_v' ///
		`theVarlist_i' indiv_male_age2 indiv_female_age2 indiv_edu_primary_m2 indiv_edu_primary_f2 indiv_male_age3 indiv_female_age3 indiv_edu_primary_m3 indiv_edu_primary_f3 ///
		`theVarlist_h' ///
	  , replace ///
    save("$directory/outputs/T2a_exogeneity.xls") stats(N r2 mean)

* Table 2b. Recovery

	use "${directory}/data/analysis_all.dta", clear

	* Adult stats
		clonevar indiv_health_weight24 = indiv_health_weight if indiv_age < 25
		clonevar indiv_health_height24 = indiv_health_height if indiv_age < 25

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"

		xi: reg indiv_health_weight hh_faultdist `fault_controls' if indiv_age > 17 ///
				, cl(village_code)
				est sto indiv_health_weight

				qui sum indiv_health_weight
				local mean = `r(mean)'
				estadd scalar mean = `mean'

		xi: reg indiv_health_height hh_faultdist `fault_controls' if indiv_age > 17 ///
				, cl(village_code)
				est sto indiv_health_height

				qui sum indiv_health_height
				local mean = `r(mean)'
				estadd scalar mean = `mean'

		xi: reg indiv_health_weight24 hh_faultdist `fault_controls' if indiv_age > 17 ///
				, cl(village_code)
				est sto indiv_health_weight24

				qui sum indiv_health_weight24
				local mean = `r(mean)'
				estadd scalar mean = `mean'

		xi: reg indiv_health_height24 hh_faultdist `fault_controls' if indiv_age > 17 ///
				, cl(village_code)
				est sto indiv_health_height24

				qui sum indiv_health_height24
				local mean = `r(mean)'
				estadd scalar mean = `mean'

	* Household assets & public infrastructure

		use "${directory}/data/analysis_hh.dta", clear

		local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"

		local theVarlist ///
			hh_assets_pca_post hh_infra_index_post hh_perm_house_post hh_stats_electricity_post hh_water_inhouse_post ///
			hh_stats_loggschool_post ///
			hh_stats_logmarket_post hh_stats_logdistrict_post hh_stats_logmedical_post hh_stats_logprischool_post

		local theCols ""
		qui foreach var of varlist `theVarlist' {

			xi: reg `var' hh_faultdist `fault_controls' ///
				, cl(village_code)
				est sto `var'

				local theLabel : var label `var'
				local theCols `"`theCols' "`theLabel'" "`theLabel'""'

				qui sum `var'
				local mean = `r(mean)'
				estadd scalar mean = `mean'

				}

		xml_tab `theVarlist' ///
			indiv_health_height indiv_health_weight indiv_health_height24 indiv_health_weight24 ///
	  , replace ///
      save("${directory}/outputs/T2b_recovery.xls") ///
			lines(COL_NAMES 3 LAST_ROW 3) format((SCLB0) (SCCB0 NCRR2)) ///
			cnames(`theCols' "Adult Height" "Adult Height" "Adult Weight" "Adult Weight" ) ///
			showeq ceq(${numbering}) c("Constant") stats(N r2 mean) ///
			title("Table 2b. Post-Earthquake Recovery") sheet("Table 2b")

* Table 3. Weight and Height

	use "${directory}/data/analysis_children.dta", clear
    keep if m_missing == 0 & indiv_age < 16

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
  , save("${directory}/outputs/T3_impacts.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist disr _IageXhh_fa_1 _IageXhh_fa_2 _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)

* Table 4a. Mother's Education OLS

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "indiv_male"

  areg indiv_theta_mean hh_faultdist m_indiv_edu_binary ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code) a(indiv_age)

    estimates store reg1
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  areg indiv_theta_mean hh_faultdist m_indiv_edu_binary m_edu_fault ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code) a(indiv_age)

    estimates store reg2
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  areg indiv_health_zanthro_height hh_faultdist m_indiv_edu_binary ///
    `fault_controls' `other_controls' ///
  if indiv_age <= 6 ///
  , cl(village_code) a(indiv_age)

    estimates store reg3
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  areg indiv_health_zanthro_height hh_faultdist m_indiv_edu_binary m_edu_fault ///
    `fault_controls' `other_controls' ///
  if indiv_age <= 6 ///
  , cl(village_code) a(indiv_age)

    estimates store reg4
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xml_tab reg1 reg2 reg3 reg4  ///
  , save("${directory}/outputs/T4a_momedu_ols.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3))  ///
    keep(hh_faultdist m_indiv_edu_binary m_edu_fault indiv_male)

* Table 4b. Mother's Education IV

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0
	cap drop m_age
	clonevar m_age = m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male i.indiv_age "
	local mother_controls "m_birthvil_logpop i.m_indiv_momedu_birthteh i.m_age"

	xi: ivreg2 indiv_theta_mean hh_faultdist ///
		( m_indiv_edu_binary = instrument )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age >= 9, ffirst cl(village_code)

		est sto reg1
		estadd scalar f = `e(cdf)'
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	xi: ivreg2 indiv_theta_mean hh_faultdist ///
		( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age >= 9, ffirst cl(village_code)

		est sto reg2
		estadd scalar f = `e(cdf)'
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'


	xi: ivreg2 indiv_health_zanthro_height hh_faultdist ///
		( m_indiv_edu_binary = instrument )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age <= 6 , ffirst cl(village_code)
		est sto reg3
		estadd scalar f = `e(cdf)'
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	xi: ivreg2 indiv_health_zanthro_height hh_faultdist ///
		( m_indiv_edu_binary m_edu_fault  = instrument i_instrument_faultdist )  ///
		`fault_controls' `other_controls' `mother_controls' ///
		if indiv_age <= 6 , ffirst cl(village_code)
		est sto reg4
		estadd scalar f = `e(cdf)'
		su `e(depvar)' if e(sample)
		estadd scalar m = `r(mean)'

	gen f = 0
		label var f "Cragg-Donald F-statistic"
	gen m = 0
		label var m "Dependent Variable Mean"

	xml_tab ///
		reg1 reg2 reg3 reg4 ///
		, save("${directory}/outputs/T4b_momedu_iv.xls") ///
		  replace below stats(m N f  ) ///
			keep( hh_faultdist m_indiv_edu_binary m_edu_fault _Iindiv_mal_1)

* Have a lovely day!
