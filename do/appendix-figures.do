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

    insheet using "${directory}/data/distance_to_faults.csv" , clear

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

* Figure A5. Regression visualization
use "${directory}/data/analysis_children.dta"  if m_missing == 0, clear

  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "indiv_male hh_logconscap i.indiv_age"

  cd "${directory}/appendix/"

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

  graph export "${directory}/appendix/FA5_momedu.png" ///
    , replace width(4000)

* Figure A6. IV Leverage

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

	graph export "${directory}/appendix/FA6_leverage.png" , replace width(4000)

// End of figures
