*! version 1.0.2  28jun2005
*! 1.0.1: 25apr2002 original version
*! 1.0.2: 28jun2005 version 8.2
*! author mes
program define ivreg2_p
	version 8.2
	local myopts "Residuals"
	_pred_se "`myopts'" `0'
	if `s(done)' { 
		exit 
	}
	local vtyp `s(typ)'
	local varn `s(varn)'
	local 0    `"`s(rest)'"'
	syntax [if] [in] [, `myopts']
				/* concatenate switch options together */
	local type "`residuals'"
				/* quickly process default case	*/
	if "`type'"=="" {
		if "`type'"=="" {
			di in gr "(option xb assumed; fitted values)"
		}
		_predict `vtyp' `varn' `if' `in', `offset'
		label var `varn' "Fitted values"
		exit
	}
				/* mark sample			 */
	marksample touse

				/* handle switch options	       */
			/* first do the ones that work both    */
			/* in and out-of-sample.	       */
	if "`type'"=="residuals" {
		tempvar fitted
		qui _predict `vtyp' `fitted' if `touse',  `offset'
		gen `vtyp' `varn' = `e(depvar)'-`fitted' if `touse'
		label var `varn' "Residuals"
		exit
	}

	error 198
end
