// Figures

* Figure A2. Illustrative Test Scores

	use "${directory}/data/analysis_children.dta", clear
	keep indiv_age censusid memid

	merge 1:1 censusid memid using "${directory}/data/score_sheet.dta", keep(3) nogen

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

		graph export "${directory}/appendix/FA2_questions.png", replace width(4000)

* Figure A3. Placebo fault tests

  * Test scores

    insheet using "${directory}/data/distance_to_faults.csv" , clear

    tempfile distances
      save `distances'

    use "${directory}/data/analysis_children.dta", clear
    keep if m_missing == 0 & indiv_age < 16

    local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
    local other_controls "indiv_male i.indiv_age"

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

    insheet using "${directory}/data/distance_to_faults.csv" , clear

    tempfile distances
      save `distances'

    use "${directory}/data/analysis_children.dta", clear
    keep if m_missing == 0 & indiv_age < 16

    // gen m_edu_fault = m_indiv_edu_binary * hh_faultdist

    local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
    local other_controls "indiv_male i.indiv_age"

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

      insheet using "${directory}/data/faults_length.csv" , clear

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

      graph export "${directory}/appendix/FA3_placebo.png" ///
        , replace width(4000)

* Figure A4. Effect Size Illustration

  import excel using "${directory}/data/shock_magnitudes.xlsx" , clear

  gen E = -A

  graph hbar E ,  xsize(7) over(C, sort((mean) E)) blab(bar, format(%02.1f)) ///
    ytit("") yline(0.70 ) yline(1.031 0.329 , lp(dash)) ///
    ylab(0 0.70 `" "Estimated" "Effect" "'  1.031  `" "Upper" "Bound" "'  0.329  `" "Lower" "Bound" "' )

    graph export "${directory}/appendix/FA4_effect.png" , width(4000) replace

* Figure A5. IV Leverage

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_tested == 1
	cap drop m_age
	clonevar m_age = m_indiv_age
	clonevar m_birthvil_logpop = m_indiv_momedu_birthvil_logpop

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male i.indiv_age"
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

	local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
	local other_controls "i.indiv_male i.indiv_age"
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

	graph export "${directory}/appendix/FA5_leverage.png" , replace width(4000)

* Figure A6. Food prices data

use "${directory}/data/analysis_all.dta" , clear
  keep village_code vil_uc_dfl_mean
  duplicates drop
  count
  tempfile dist
    save `dist'

use "${directory}/data/prices.dta", clear
merge m:1 censusid using "${directory}/data/analysis_hh.dta" , keepusing(village_code) keep(3) nogen
merge m:1 village_code using `dist' , keepusing(vil_uc_dfl_mean) keep(3) nogen
keep censusid food_item mSec30_q2_units mSec30_q2_code mSec30_q3 village_code vil_uc_dfl_mean
bys food_item : egen mcom = mode(mSec30_q2_code)
  keep if mSec30_q2_code == mcom // Only use comparable prices (drop 3% of data)
  drop mcom mSec30_q2_code
  keep if !missing(mSec30_q2_units)

  encode food_item , gen(code)
    drop food_item

  collapse (mean) price = mSec30_q3  ///
    (firstnm) distance = vil_uc_dfl_mean ///
    [fweight = mSec30_q2_units] ///
    , by(code village_code)

  qui levelsof code , local(codes)
    foreach i in `codes' {
      local l`i' : label (code) `i'
    }

  reshape wide price , i(village_code) j(code)

  foreach i in `codes' {
    lab var price`i' "`l`i''"
  }

  lab var distance "Distance (km)"

  forest reg (price*) , t(distance) b bh sort(global) graphopts(ysize(7)) d

  graph export "${directory}/appendix/FA6_prices.png", replace width(4000)

* Figure A7. Alternate cutoffs

	local counter 0
	qui forv i = 10(5)30 {
	  local ++counter

		* Height by age

			use "${directory}/data/analysis_children.dta", clear
			keep if m_missing == 0 & indiv_childage_pre <= 11
	    replace indiv_near_quake = hh_faultdist <= `i'

	  		tw (histogram indiv_childage_pre if indiv_health_zanthro_height!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
	  			(lpolyci indiv_health_zanthro_height indiv_childage_pre ///
	            if indiv_near_quake==0, degree(1) lc(black) lp(solid) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
	  			(lpolyci indiv_health_zanthro_height indiv_childage_pre ///
	            if indiv_near_quake==1, degree(1) lc(black) lp(dash) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
	  		, $graph_opts $hist_opts title("Height-for-Age (`i'km)") ///
	  			legend(on size(small) order(3 "Far from Fault Line" 5 "Close to Fault Line" ) pos(1) r(1) ring(0) region( lc(white) ) ) ///
	  			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
	  			ytitle(" ") ylabel(0 "Reference" -1 "-1.0 SD" -2 "-2.0 SD" -3 "-3.0 SD" , angle(0))

				graph save "${directory}/appendix/FA7_height-`counter'.gph", replace

	  	use "${directory}/data/analysis_children.dta", clear
			keep if m_missing == 0 & indiv_childage_pre <= 11
	    replace indiv_near_quake = hh_faultdist <= `i'

	  		tw 	(histogram indiv_childage_pre if indiv_theta_mean!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
	  			(lpolyci indiv_theta_mean indiv_childage_pre ///
	            if indiv_near_quake==0, degree(1) lc(black) lp(solid) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
	  			(lpolyci indiv_theta_mean indiv_childage_pre ///
	            if indiv_near_quake==1, degree(1) lc(black) lp(dash) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
	  		, $graph_opts $hist_opts title("Test Scores (`i'km)") ///
	  			legend(on order(3 "Far from Fault" 5 "Close to Fault" ) pos(1) r(1) ring(0) region( lc(white) ) ) ///
	  			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
	  			ytitle(" ") ylabel(-.5 `""-0.5" "SD""' 0 `" " " "Mean" " " "' .5 `""+0.5" "SD""' )

	  			graph save 	"${directory}/appendix/FA7_scores-`counter'.gph", replace

	}

	* Combine

    grc1leg ///
			"${directory}/appendix/FA7_height-1.gph" ///
      "${directory}/appendix/FA7_scores-1.gph" ///
			"${directory}/appendix/FA7_height-2.gph" ///
			"${directory}/appendix/FA7_scores-2.gph" ///
  		"${directory}/appendix/FA7_height-3.gph" ///
			"${directory}/appendix/FA7_scores-3.gph" ///
			"${directory}/appendix/FA7_height-4.gph" ///
			"${directory}/appendix/FA7_scores-4.gph" ///
			"${directory}/appendix/FA7_height-5.gph" ///
 			"${directory}/appendix/FA7_scores-5.gph" ///
			, c(2) ${comb_opts} xcom altshrink

    graph save 	"${directory}/appendix/FA7_cutoffs.gph", replace
    graph combine 	"${directory}/appendix/FA7_cutoffs.gph", ysize(6)

		graph export "${directory}/appendix/FA7_cutoffs.png", replace width(4000)
		
* Figure A8. Birth cohort selection
use "${directory}/data/analysis_children.dta", clear

  lab var indiv_father_age "Father's Age"
  lab var indiv_father_edu "Father Completed Primary School"

  tab hh_occ_code, gen(occ)
    foreach var of varlist occ* {
      local theLabel : var lab `var'
      lab var `var' "Household Head is `=substr("`theLabel'",strpos("`theLabel'","==")+2,.)'"
    }

  foreach var in ///
     indiv_father_age indiv_father_edu indiv_father_height ///
     m_age m_indiv_edu_binary m_indiv_health_height ///
     occ1 occ2 occ3 ///
     {

    local lab : var label `var'
    
    preserve
    keep if hh_near_quake == 1
    collapse (mean) mean = `var' (sem) sem = `var' (count) n = `var' , by(indiv_age)
      gen ul = mean + (1.96 * sem)
      gen ll = mean - (1.96 * sem)
      
      gen cohort = inlist(indiv_age,3,4,5,6)
      
      tw ///
        (lfitci mean indiv_age [aweight = n] if !cohort , lw(none) fc(gs12)) ///
        (lfit mean indiv_age [aweight = n] if !cohort , lp(dash) lw(thin) lc(black)) ///
        (rspike ll ul indiv_age if cohort  , lc(red)) ///
        (rspike ll ul indiv_age if !cohort , lc(black)) ///
        (scatter mean indiv_age , mc (black)) ///
      , title("`lab'") xtit("Survey Age Cohort") xlab(0(1)18) ///
        saving("${directory}/appendix/`var'.gph", replace) nodraw
    restore
    
    local graphs `"`graphs' "${directory}/appendix/`var'.gph""'
  }

graph combine `graphs' , c(3) ysize(6) altshrink
graph export "${directory}/appendix/FA8_cohort.png" , width(4000) replace

// End of figures
