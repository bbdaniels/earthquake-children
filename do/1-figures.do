* Final figure set for eathquake shock paper

/* Figure 1: Map – not reproducible without identifying GPS data


  global gis "/Users/bbdaniels/Box/Earthquake"
	use "${gis}/Data/GIS/studydbf.dta", clear

	spmap using "${gis}/Data/GIS/study.dta", id(id) $graph_opts fc(gs14) oc(black) ///
		point( data("${gis}/Data/GIS/households.dta") x(_____) y(_____) by(hh_stats_permhouse_post) ///
			legenda(off) legc fcolor(black black black) shape(x x T) prop(size) size(small) osize(thin thin medthick) ) ///
		line( data("${gis}/Data/GIS/study_faults_clipped.dta") by(active) color(gray black) pattern(solid dash) size(thin thick))

		graph export "${directory}/outputs/F1_map.png", replace width(4000)
		graph export "${directory}/outputs/F1_map.eps", replace 
*/

* Figure 2: Household distance to activated Fault Line (km)

	use "${directory}/data/analysis_hh.dta", clear

	qui sum hh_faultdist if hh_faultdist <= 60, d

	gen f= -50
		gen pmin=r(p5)
		gen p25=r(p25)
		gen p50=r(p50)
		gen p75=r(p75)
		gen pmax=r(p95)
		gen pmean=r(mean)

	tw ///
		(histogram hh_faultdist , la(center) freq w(5) start(0) $graph_opts gap(10) fc(gs14) lp(solid) lc(black) ) ///
		(rcap pmin p25 f in 1, msize(3) hor bcolor(black) lw(thin) )                 ///
		(rcap p75 pmax f in 1, msize(3) hor bcolor(black) lw(thin) )                 ///
		(rbar p25 p75 f in 1, la(center) barwidth(25) hor fc(none) lp(solid) lw(thin) lc(black) la(center))                   ///
		(rcap p50 p50 f in 1, msize(3) hor bcolor(black) lw(thin) )                    ///
		, xlab(0(5)75, notick) xtit("Distance to Activated Fault Line (km) {&rarr}", align(left) placement(left)) ///
		ylab(0(100)500) ytit("Number of Households", align(center) placement(center)) legend(off) xsize(7)

		graph export "${directory}/outputs/F2_distance.png", replace width(2000)
		graph export "${directory}/outputs/F2_distance.eps", replace 

* Figure 3: Home Destruction from Long Census

	use "${directory}/data/analysis_all.dta", clear

	qui count if indiv_age > 3

	tw  (histogram hh_faultdist, freq yaxis(2) color(gs14) start(0) w(2) gap(10)) ///
		(lpoly indiv_dead_quake hh_faultdist, lc(black) yaxis(1) astyle(ci) bw(.5) degree(1)) ///
		if hh_faultdist < 60 & indiv_age > 3 ///
	,	$graph_opts $hist_opts ylab(0 "0%" .01 "1%" .02 "2%" .03 "3%" .04 "4%" .05 "5%" .06 "6%") xtit("") ytit("") ///
		xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
		legend(off) subtitle("Died During Earthquake (N = `r(N)')", pos(12))

		graph save "${directory}/outputs/F3_deadquake.gph", replace

	qui su c_home_des if tag_hh == 1

	tw  (histogram hh_faultdist, freq yaxis(2) color(gs14) start(0) w(2) gap(10)) ///
		(lpoly c_home_des hh_faultdist, lc(black) yaxis(1) astyle(ci) bw(.5) degree(1)) ///
		if hh_faultdist < 60 & tag_hh == 1 ///
	,	$graph_opts $hist_opts ylab($xpct) xtit("") ytit("") ///
		xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
		legend(off) subtitle("Home Destroyed (N = `r(N)')", pos(12))

		graph save "${directory}/outputs/F3_destroy.gph", replace

	qui count if indiv_age > 3
		cap gen isdead = indiv_time_of_death == 2 | indiv_time_of_death == 3

	tw  (histogram hh_faultdist, freq yaxis(2) color(gs14) start(0) w(2) gap(10)) ///
		(lpoly isdead hh_faultdist, lc(black) yaxis(1) astyle(ci) bw(.5) degree(1)) ///
		if hh_faultdist < 60 & indiv_age > 3   ///
	,	$graph_opts $hist_opts ylab(0 "0%" .01 "1%" .02 "2%" .03 "3%" .04 "4%" .05 "5%" .06 "6%") ///
		ytit("") xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
		subtitle("Died After Earthquake (N = `r(N)')", pos(12)) 	legend(off)

		graph save "${directory}/outputs/F3_death.gph", replace

	qui su vil_facil_destroyed if tag_village == 1

	tw  (histogram vil_uc_dfl_mean, freq yaxis(2) color(gs14) start(0) w(2) gap(10)) ///
		(lpoly vil_facil_destroyed hh_faultdist, lc(black) yaxis(1) astyle(ci) bw(.5) degree(1)) ///
		if tag_village == 1 & vil_uc_dfl_mean < 60 ///
	,	$graph_opts $hist_opts ylab(${xpct}) xtit("")  ytit("") ///
		xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
		legend(off) subtitle("Village Facilities Destruction (N = `r(N)')", pos(12))

		graph save "${directory}/outputs/F3_infra.gph", replace


	* Combine

		graph combine ///
			"${directory}/outputs/F3_destroy.gph" ///
			"${directory}/outputs/F3_infra.gph" ///
			"${directory}/outputs/F3_deadquake.gph" ///
			"${directory}/outputs/F3_death.gph" ///
		, $comb_opts xsize(7)

		graph export "${directory}/outputs/F3_damage.png", replace width(2000)
		graph export "${directory}/outputs/F3_damage.eps", replace 

* Figure 4. Assets

  use "${directory}/data/analysis_hh.dta", clear
	preserve
	  collapse (mean) hh_assets_*_pre
		ren hh_assets_*_pre hh_assets_*
		gen i = 1
		reshape long hh_assets_ , i(i) j(j)
		  gsort - hh_assets_
			qui forvalues i = 1/`c(N)' {
				local vars "`vars' hh_assets_`=j[`i']'_pre"
			}
	restore

		betterbar `vars' , over(hh_far_from_quake) ///
			${graph_opts} xlab(${xpct}) xscale(alt) barcolor(gray black) ///
			xoverhang xsize(6) xtit("Share of Households Owning {&rarr}") ///
			legend(on c(2) pos(12) ring(1) symysize(small) symxsize(small) size(small) ///
				textfirst order(1 "Near Fault Line (<20km)" 2 "Far from Fault Line (20km+)")) ylab(,labsize(small))

	graph export "${directory}/outputs/F4_assets.png", replace width(4000)
	graph export "${directory}/outputs/F4_assets.eps", replace 

* Figure 5. Aid

	use "${directory}/data/analysis_all.dta"  , clear
		keep if tag_hh
			tempfile hh
			save `hh' , replace
	use "${directory}/data/analysis_hh.dta", clear
		merge 1:1 censusid using `hh' , keep(3) nogen keepusing(c_home_des)

	gen hh_aid_immed = hh_aid_total - hh_housing_amount + 25000 * hh_housing_mSec16_q1_trench1

	qui su hh_consumption_food if hh_far_from_quake==1, d
		local food = r(p50)
	qui su hh_consumption_nonfood if hh_far_from_quake==1, d
		local nonfood = r(p50)
	gen total = hh_consumption_food + hh_consumption_nonfood

  qui su total if hh_far_from_quake==1, d
		local total  = round(r(p50),10000)
		local total_50  = round(`total'/2,10000)
		local total_150  = round(`total'*1.5,10000)

	twoway 	///
			(histogram hh_faultdist, freq start(0) w(2) yaxis(2) bstyle(outline) bc(gs14) gap(10)) ///
			(lpoly hh_aid_total hh_faultdist , lw(thick) lp(solid) lc(black)  ) ///
			(lpoly hh_aid_immed hh_faultdist , lw(thick) lp(dash) lc(black) ) ///
			(lpoly hh_aid_immed hh_faultdist if hh_dead  , lw(thick) lp(#-_) lc(black)  ) ///
			(lpoly hh_aid_immed hh_faultdist if c_home_des  , lw(thick) lp(##-) lc(black)  ) ///
			if hh_faultdist < 50 ///
		, ///
			xtitle("Distance to Activated Fault Line (km) {&rarr}") ytitle("") $graph_opts $hist_opts  ///
			legend(on span size(small) ring(1) pos(6) c(4) ///
      order(2 "Total Cash Aid" 0 " " ///
	      3 "Immediate Aid" 0 "(ex. later housing aid)" ///
        4 "Immediate Aid" 0 "(households with a death)" ///
        5 "Immediate Aid" 0 "(if home destroyed)")) ///
			ylab( 0 "No Financial Assistance" `total_50' `""50% of Median" "Annual Consumption" "(PKR `total_50')""' ///
				`total' `""100% of Median" "Annual Consumption" "(PKR `total')""' ///
				`total_150' `""150% of Median" "Annual Consumption" "(PKR `total_150')""' ///
				, angle(0) labsize(small)) xsize(7)

		graph export "${directory}/outputs/F5_aid.png", replace width(4000)
		graph export "${directory}/outputs/F5_aid.eps", replace 

* Figure 6a. Weight

	* Weight by age

		use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_childage_pre <= 11

		tw 	(histogram indiv_childage_pre if indiv_health_zanthro_weight!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
			(lpolyci indiv_health_zanthro_weight indiv_childage_pre ///
          if indiv_near_quake==0, degree(1) lc(black) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
			(lpolyci indiv_health_zanthro_weight indiv_childage_pre ///
          if indiv_near_quake==1, degree(1) lc(black) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
		, $graph_opts $hist_opts title("Weight-for-Age") ///
			legend(on size(small) order(3 "20km+ from Fault Line" 5 "<20km to Fault Line" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
			ytitle(" ") ylabel(0 "Reference" 1 "+1.0 SD" -1 "-1.0 SD" -2 "-2.0 SD" -3 "-3.0 SD" , angle(0))

			graph save "${directory}/outputs/F6a_age.gph", replace

  * By distance

    use "${directory}/data/analysis_children.dta", clear
      keep if m_missing == 0 & indiv_childage_pre <= 11 & hh_faultdist < 60

      recode indiv_agecat (0=3)
      label def indiv_agecat 3 "Age 3+" , modify

      keep if indiv_health_weight != .

      bys indiv_age : egen mean_weight = mean(indiv_health_weight) if hh_near_quake == 0
      bys indiv_age : egen mean_weight_2 = mean(mean_weight)
      gen weight_ex = indiv_health_weight - mean_weight_2

    tw ///
      (lpolyci weight_ex hh_faultdist if indiv_agecat == 1 ///
        , lc(black) lp(dash) astyle(ci) fc(gray%30) alc(%0)) ///
      (lpolyci weight_ex hh_faultdist if indiv_agecat == 2 ///
        , lc(black) lp(longdash) astyle(ci) fc(gray%30) alc(%0)) ///
      (lpolyci weight_ex hh_faultdist if indiv_agecat == 3 ///
        , lc(black) lp(solid) astyle(ci) fc(gray%30) alc(%0)) ///
      , $graph_opts xsize(7) legend(on size(small) pos(12) ring(1) region( lc(none) ) ///
          order(6 "Age 3+" 4 "Newborn - Age 2" 2 "In Utero") r(1)) ///
        xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
        ylab(0 "Reference" -.5 "-0.5kg" -1 "-1kg" .5 "+0.5kg" 1 "+1kg")

        graph save "${directory}/outputs/F6a_dist.gph", replace

	* Combine

    graph combine ///
      "${directory}/outputs/F6a_age.gph" ///
      "${directory}/outputs/F6a_dist.gph" ///
      , ysize(6) c(1)

		graph export "${directory}/outputs/F6a_weight.png", replace width(4000)
		graph export "${directory}/outputs/F6a_weight.eps", replace 

* Figure 6b. Height

	* Height by age

		use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_childage_pre <= 11

		tw (histogram indiv_childage_pre if indiv_health_zanthro_height!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
			(lpolyci indiv_health_zanthro_height indiv_childage_pre ///
          if indiv_near_quake==0, degree(1) lc(black) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
			(lpolyci indiv_health_zanthro_height indiv_childage_pre ///
          if indiv_near_quake==1, degree(1) lc(black) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
		, $graph_opts $hist_opts title("Height-for-Age") ///
			legend(on size(small) order(3 "20km+ from Fault Line" 5 "<20km to Fault Line" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
			ytitle(" ") ylabel(0 "Reference" -1 "-1.0 SD" -2 "-2.0 SD" -3 "-3.0 SD" , angle(0))

			graph save "${directory}/outputs/F6b_age.gph", replace

  * By distance

    use "${directory}/data/analysis_children.dta", clear
      keep if m_missing == 0 & indiv_childage_pre <= 11 & hh_faultdist < 60

      recode indiv_agecat (0=3)
      label def indiv_agecat 3 "Age 3+" , modify

      keep if indiv_health_height != .

      bys indiv_age : egen mean_height = mean(indiv_health_height) if hh_near_quake == 0
      bys indiv_age : egen mean_height_2 = mean(mean_height)
      gen height_ex = indiv_health_height - mean_height_2

    tw ///
      (lpolyci height_ex hh_faultdist if indiv_agecat == 1 ///
        , lc(black) lp(dash) astyle(ci) fc(gray%30) alc(%0)) ///
      (lpolyci height_ex hh_faultdist if indiv_agecat == 2 ///
        , lc(black) lp(longdash) astyle(ci) fc(gray%30) alc(%0)) ///
      (lpolyci height_ex hh_faultdist if indiv_agecat == 3 ///
        , lc(black) lp(solid) astyle(ci) fc(gray%30) alc(%0)) ///
      , $graph_opts xsize(7) legend(on size(small) pos(12) ring(1) region( lc(white) ) ///
          order(6 "Age 3+" 4 "Newborn - Age 2" 2 "In Utero") r(1)) ///
        xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
        ylab(0 "Reference" -4 "-4cm" 4 "+4cm" 2 "+2cm" -2 "-2cm")

  		graph save "${directory}/outputs/F6b_dist.gph", replace

  * Combine

    graph combine ///
      "${directory}/outputs/F6b_age.gph" ///
      "${directory}/outputs/F6b_dist.gph" ///
      , ysize(6) c(1)

    graph export "${directory}/outputs/F6b_height.png", replace width(4000)
    graph export "${directory}/outputs/F6b_height.eps", replace 

* Figure 7a. Education

	* Enrollment

		use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_childage_pre <= 11

		tw 	(histogram indiv_childage_pre if indiv_school_enrolled_post!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
			(lpolyci indiv_school_enrolled_post indiv_childage_pre ///
          if indiv_near_quake==0, degree(1) lc(black) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
			(lpolyci indiv_school_enrolled_post indiv_childage_pre ///
          if indiv_near_quake==1, degree(1) lc(black) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
		, $graph_opts $hist_opts title("Enrollment") ///
			legend(on order(3 "20km+ from Fault" 5 "<20km to Fault" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
			ytitle(" ") ylabel(${xpct})

			graph save 	"${directory}/outputs/F7a_enroll.gph", replace

	* Test Scores

		use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_childage_pre <= 11

		tw 	(histogram indiv_childage_pre if indiv_theta_mean!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
			(lpolyci indiv_theta_mean indiv_childage_pre ///
          if indiv_near_quake==0, degree(1) lc(black) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
			(lpolyci indiv_theta_mean indiv_childage_pre ///
          if indiv_near_quake==1, degree(1) lc(black) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
		, $graph_opts $hist_opts title("Test Scores") ///
			legend(on order(3 "20km+ from Fault" 5 "<20km to Fault" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
			ytitle(" ") ylabel(-.5 `""-0.5" "SD""' 0 `" " " "Mean" " " "' .5 `""+0.5" "SD""' )

			graph save 	"${directory}/outputs/F7a_scores.gph", replace

	* Combine

    grc1leg ///
			"${directory}/outputs/F7a_enroll.gph" ///
			"${directory}/outputs/F7a_scores.gph" ///
			, c(1) ${comb_opts}

    graph save "${directory}/outputs/F7a_education.gph" , replace
    graph combine "${directory}/outputs/F7a_education.gph" , ysize(6)
		graph export "${directory}/outputs/F7a_education.png", replace width(4000)
		graph export "${directory}/outputs/F7a_education.eps", replace 

* Figure 7b. Distance and learning

	use "${directory}/data/analysis_children.dta" if indiv_age>=9, clear

	tw 	 ///
    (histogram hh_faultdist if indiv_theta_mean != .   ///
      , freq s(0) w(2) gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
		(lpolyci indiv_theta_mean hh_faultdist ///
      , degree(1) lc(black) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
    if hh_faultdist < 60 & indiv_male == 0 ///
    , ${graph_opts} ${hist_opts} xtitle("Distance to Activated Fault Line (km) {&rarr}") title("Girls") ///
      ylabel(-.5 `""-0.5" "SD""' 0 `" " " "Mean" " " "' .5 `""+0.5" "SD""' )

    graph save "${directory}/outputs/F7b_girls.gph", replace

  tw 	 ///
    (histogram hh_faultdist if indiv_theta_mean != .   ///
      , freq s(0) w(2) gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
		(lpolyci indiv_theta_mean hh_faultdist ///
      , degree(1) lc(black) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
    if hh_faultdist < 60 & indiv_male == 1 ///
    , ${graph_opts} ${hist_opts} xtitle("Distance to Activated Fault Line (km) {&rarr}") title("Boys") ///
      ylabel(-.5 `""-0.5" "SD""' 0 `" " " "Mean" " " "' .5 `""+0.5" "SD""' )

    graph save "${directory}/outputs/F7b_boys.gph", replace

    graph combine ///
      "${directory}/outputs/F7b_girls.gph" ///
      "${directory}/outputs/F7b_boys.gph" ///
      , r(1) ycom xsize(7)

			graph export "${directory}/outputs/F7b_scores.png", replace width(4000)
			graph export "${directory}/outputs/F7b_scores.eps", replace 


* Figure 8a. Maternal Education and Test Scores

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_childage_pre <= 11

		tw 	(histogram hh_faultdist if indiv_theta_mean!=. , freq gap(10) yaxis(2) start(0) w(2) bstyle(outline) bc(gs14) ) ///
			(lpolyci indiv_theta_mean hh_faultdist ///
          if m_indiv_edu_binary==0, degree(1) lc(black) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
			(lpolyci indiv_theta_mean hh_faultdist ///
          if m_indiv_edu_binary==1, degree(1) lc(black) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
      if hh_faultdist <= 50 ///
		, $graph_opts $hist_opts  ///
			legend(on order(3 "No Educated Mother" 5 "Mother Primary Education" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
			xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
			ytitle(" ") ylabel(-.2 `""-0.2" "SD""' 0 `" " " "Mean" " " "' .2 `""+0.2" "SD""' ) xsize(7)

      graph export "${directory}/outputs/F8a_edu_scores.png", replace width(4000)
      graph export "${directory}/outputs/F8a_edu_scores.eps", replace 

* Figure 8b. Maternal Education and Height

	use "${directory}/data/analysis_children.dta", clear
	keep if m_missing == 0 & indiv_childage_pre <= 2

		tw 	(histogram hh_faultdist if indiv_health_zanthro_height!=. , freq gap(10) yaxis(2) start(0) w(2) bstyle(outline) bc(gs14) ) ///
			(lpolyci indiv_health_zanthro_height hh_faultdist ///
          if m_indiv_edu_binary==0, degree(1) lc(black) lp(solid) astyle(ci) fc(gray%50) alc(%0)) ///
			(lpolyci indiv_health_zanthro_height hh_faultdist ///
          if m_indiv_edu_binary==1, degree(1) lc(black) lp(dash) astyle(ci) fc(gray%50) alc(%0)) ///
      if hh_faultdist <= 50 ///
		, $graph_opts $hist_opts ///
			legend(on order(3 "No Educated Mother" 5 "Mother Primary Education" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
			xsize(7) xtitle("Distance to Activated Fault Line (km) {&rarr}") ///
			ytitle(" ") ylabel(-2 "-2SD" 0 "Reference" -1 "-1 SD" )

      graph export "${directory}/outputs/F8b_edu_height.png", replace width(4000)
      graph export "${directory}/outputs/F8b_edu_height.eps", replace


* Figure 9. Disruption

	use "${directory}/data/analysis_children.dta", clear

		keep if m_missing == 0 & indiv_childage_pre <= 11

		tw  (histogram hh_faultdist if indiv_school_disruption !=. & indiv_theta_mean != ., freq s(0) w(2) gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
			(lpoly indiv_school_disruption hh_faultdist if theta_high != ., degree(1) lp(solid) lc(gs8)  ) ///
			(lpoly indiv_school_disruption hh_faultdist if theta_high == 1, degree(1) lp(-.. ) lc(black)  ) ///
			(lpoly indiv_school_disruption hh_faultdist if theta_high == 0, degree(1) lp(_ ) lc(black)  ) ///
		if hh_faultdist < 60 ///
		, ylab( 0 4 8 12 16 `" " " "16" "Weeks" "' , angle(0) ) ytitle("") $graph_opts $hist_opts ///
			legend(on order(4 "Bottom 50% of test scores"  3 "Top 50% of test scores" 2 "Pooled") cols(1) pos(2) ring(0) region( lc(white) ) ) ///
			xtitle("Distance to Activated Fault Line (km) {&rarr}") xlab(0(10)60) xsize(7)

			graph save "${directory}/outputs/F9a_disruption.gph", replace 
			
	use "${directory}/data/analysis_children.dta", clear

		keep if m_missing == 0 & indiv_childage_pre <= 11 & hh_faultdist < 20
		qui sum disr, d

	gen f= -50
		gen pmin=r(p5)
		gen p25=r(p25)
		gen p50=r(p50)
		gen p75=r(p75)
		gen pmax=r(p95)
		gen pmean=r(mean)

	tw ///
		(histogram disr , la(center) freq w(4) start(0) $graph_opts gap(10) fc(gs14) lp(solid) lc(black) ) ///
		(rcap pmin p25 f in 1, msize(2) hor bcolor(black) lw(thin) )                 ///
		(rcap p75 pmax f in 1, msize(2) hor bcolor(black) lw(thin) )                 ///
		(rbar p25 p75 f in 1, la(center) barwidth(25) hor fc(none) lp(solid) lw(thin) lc(black) la(center))                   ///
		(rcap p50 p50 f in 1, msize(2) hor bcolor(black) lw(thin) )                    ///
		, xlab(0(5)100, notick) xtit("Time Out of School (Weeks) {&rarr}", align(left) placement(left)) ///
		ylab(0(100)500) ytit("Number of Children", align(center) placement(center)) legend(off) xsize(7)

		graph save "${directory}/outputs/F9b_disruption.gph", replace 
		
		graph combine ///
		  "${directory}/outputs/F9a_disruption.gph" ///
		  "${directory}/outputs/F9b_disruption.gph" ///
	  , c(1) ysize(5)
		
		graph export "${directory}/outputs/F9_disruption.png", replace width(4000)
		graph export "${directory}/outputs/F9_disruption.eps", replace 

* Have a lovely day!
