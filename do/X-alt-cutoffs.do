
forv i = 10(5)30 {

	* Height by age

		use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_childage_pre <= 11
    replace indiv_near_quake = hh_faultdist <= `i'

  		tw (histogram indiv_childage_pre if indiv_health_zanthro_height!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
  			(lpolyci indiv_health_zanthro_height indiv_childage_pre ///
            if indiv_near_quake==0, degree(1) lc(black) lp(solid) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
  			(lpolyci indiv_health_zanthro_height indiv_childage_pre ///
            if indiv_near_quake==1, degree(1) lc(black) lp(dash) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
  		, $graph_opts $hist_opts title("Height-for-Age") ///
  			legend(on order(3 "`i'km+ from Fault" 5 "<`i'km to Fault" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
  			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
  			ytitle(" ") ylabel(0 "Reference" -1 "-1.0 SD" -2 "-2.0 SD" -3 "-3.0 SD" , angle(0))

			graph save "${directory}/appendix/FX_height.gph", replace

  	use "${directory}/data/analysis_children.dta", clear
		keep if m_missing == 0 & indiv_childage_pre <= 11
    replace indiv_near_quake = hh_faultdist <= `i'

  		tw 	(histogram indiv_childage_pre if indiv_theta_mean!=. , freq disc gap(10) yaxis(2) bstyle(outline) bc(gs14) ) ///
  			(lpolyci indiv_theta_mean indiv_childage_pre ///
            if indiv_near_quake==0, degree(1) lc(black) lp(solid) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
  			(lpolyci indiv_theta_mean indiv_childage_pre ///
            if indiv_near_quake==1, degree(1) lc(black) lp(dash) lw(medthick) astyle(ci) fc(gray%50) alc(%0)) ///
  		, $graph_opts $hist_opts title("Test Scores") ///
  			legend(on order(3 "`i'km+ from Fault" 5 "<`i'km to Fault" ) pos(1) r(1) ring(1) region( lc(white) ) ) ///
  			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
  			ytitle(" ") ylabel(-.5 `""-0.5" "SD""' 0 `" " " "Mean" " " "' .5 `""+0.5" "SD""' )

  			graph save 	"${directory}/appendix/FX_scores.gph", replace

  	* Combine

      grc1leg ///
  			"${directory}/appendix/FX_height.gph" ///
  			"${directory}/appendix/FX_scores.gph" ///
  			, c(1) ${comb_opts}

  		graph export "${directory}/appendix/FX_alt-cutoffs-`i'.png", replace width(4000)

}

* End
