! 1.0.1 28jun2005 cfb updated to v8.2
program define ivreg2_cue
	version 8.2
	args todo b lnf
	tempname J 
	qui ivreg2 $IV_lhs $IV_inexog ($IV_endog=$IV_exexog) $IV_wt if $ML_samp==1, b0(`b') $IV_opt
* di "ml: ivreg2 $IV_lhs $IV_inexog ($IV_endog=$IV_exexog) $IV_wt if $ML_samp==1, b0(`b') $IV_opt"
	scalar `J'=e(sargan)
	if `J'==. {
		scalar `J'=e(j)
	}
	scalar `lnf' = -`J'
end
