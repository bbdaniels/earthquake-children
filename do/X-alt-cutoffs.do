
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
  			legend(on order(3 "Far from Fault" 5 "Close to Fault" ) pos(1) r(1) ring(0) region( lc(white) ) ) ///
  			xtitle("Age During Earthquake {&rarr}") xscale(r(-1,11)) xlabel(-1(1)11 -1 `""In" "Utero""' 0 `""New-" "born""', labsize(small) notick) ///
  			ytitle(" ") ylabel(0 "Reference" -1 "-1.0 SD" -2 "-2.0 SD" -3 "-3.0 SD" , angle(0))

			graph save "${directory}/appendix/FAX_height-`counter'.gph", replace

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

  			graph save 	"${directory}/appendix/FAX_scores-`counter'.gph", replace

}
=
  	* Combine

      grc1leg ///
  			"${directory}/appendix/FAX_height-1.gph" ///
        "${directory}/appendix/FAX_scores-1.gph" ///
  			"${directory}/appendix/FAX_height-2.gph" ///
  			"${directory}/appendix/FAX_scores-2.gph" ///
    		"${directory}/appendix/FAX_height-3.gph" ///
  			"${directory}/appendix/FAX_scores-3.gph" ///
  			"${directory}/appendix/FAX_height-4.gph" ///
  			"${directory}/appendix/FAX_scores-4.gph" ///
  			"${directory}/appendix/FAX_height-5.gph" ///
   			"${directory}/appendix/FAX_scores-5.gph" ///
  			, c(2) ${comb_opts} xcom altshrink

      graph save 	"${directory}/appendix/FAX_cutoffs.gph", replace
      graph combine 	"${directory}/appendix/FAX_cutoffs.gph", ysize(6)

  		graph export "${directory}/appendix/FAX_cutoffs.png", replace width(4000)


* End
