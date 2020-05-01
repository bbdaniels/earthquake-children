** Generates better bar graphs

cap prog drop betterbar
prog def betterbar

syntax anything 				/// Variable list should be parenthesis-grouped to produce grouping if desired.
	[using]						/// For output
	[if] [in] , 				///
	[Save(string asis)]			/// Specify graph export location
	[Export(string asis)]		/// Specify graph export location
	[stats(string asis)]		/// Stats to display in caption
	[Labels(string asis)]		/// One label for each variable group. : NOT YET IMPLEMENTED
	[Vertical]					/// Horizontal bar is the default.
	[se]						/// Plots standard error bars.
	[BINomial]					/// Plots standard error bars using binomial distribution; only if se is on.
	[Over(varlist)]				/// Determines groups for comparison at the lowest level.
	[Descending(string asis)]	/// Sorts with largest bar first; enter the logic expression identifying the sort group (ie, one == for each var of varlist). Use (1) if only one group.
	[Ascending(string asis)]	/// Sorts with smallest bar first; enter the logic expression identifying the sort group (ie, one == for each var of varlist). Use (1) if only one group.
	[BARlab(string asis)]		/// Labels bars with means. Specify location: upper, lower, mean, zero.
	[barlook(string asis)]		/// Allows specification of bar look options - write barlook(1 [styles] 2 [styles] ...)
	[by(varname)]				/// Separate variables
	[NOBYLabel]					/// NO labels in by
	[NOOVLabel]					/// NO labels in over
	[*]							/// Allows any normal options for tw to be entered.

preserve
marksample touse
qui keep if `touse'

* Calculate statistics if specified

	if "`stats'" != "" {
		gen N = _N
			label var N "Number of Observations"
		foreach var of varlist `stats' {
			qui sum `var'
				local stat = string(round(`r(mean)',0.01))
			local label : var label `var'
			local addStats `"`addStats' "`label' = `stat'" "'
			}
			
		local addStats `"cap(`addStats', span pos(11))"'
		}

* Set up options

	* Vertical bars
	
		if "`vertical'" == "" {
			local horizontal horizontal
			local labelaxis y
			local valueaxis x
			local scatter x \`barlab'
			local axisfix x zero
			}
		else {
			local horizontal vertical
			local labelaxis x
			local valueaxis y
			local scatter \`barlab' x
			local axisfix zero x
			}

	* Standard error bars
	
		if "`binomial'" == "binomial" {
			local serrstat seb
			}
		else {
			local serrstat sem
			}

		if "`se'" == "se" {
			local semcollapse "(`serrstat') \`semvarlist'" // Set up collapse option for standard errors
			local seplot "(rcap upper lower x, `horizontal' lc(black) lw(vthin) msize(vsmall))" // Set up tw graph for standar errors
			}
			
	* Over groups
	
		if "`over'" == "" {
			gen _over = 1
			local over _over
			}
			
		foreach var of varlist `over' {
			cap drop if `var' == .
			cap drop if `var' == ""
			}
			
	* Bar labels
	
		if "`barlab'" != "" {
									local blabplot (scatter `scatter', mlabel(mean) msymbol(none) mlabc(black) mlabs(1.8) mlabp(3))
			if "`vertical'" != "" 	local blabplot (scatter `scatter', mlabel(mean) msymbol(none) mlabc(black) mlabs(medsmall) mlabp(12))
			}
			
	* Variable sort
	
		if "`ascending'" != ""{
			local sort ""
			local sortLogic `"`ascending'"'
			}
			
		if "`descending'" != ""{
			local sort "-"
			local sortLogic `"`descending'"'
			}
		
	* Separate into variable lists
		
		local x = 1
		while strpos("`anything'",")") > 0 {
			local vargroup_`x' = substr("`anything'",1,strpos("`anything'",")")-1) 	// Take out group of variables up to close-parenthesis
				local vargroup_`x' = subinstr("`vargroup_`x''","(","",1) 			// Remove open-parenthesis.
				unab vargroup_`x' : `vargroup_`x''									// Expand variable names if needed.
				
				if "`by'" != "" {
					local tempvarlist ""
					foreach var of varlist `vargroup_`x'' {
						local theMainLabel : var label `var'
						qui separate `var' , by(`by') short
						local tempvarlist "`tempvarlist' `r(varlist)'"
						foreach newvar in `r(varlist)' {
							local theLabel: var label `newvar'
							if "`nobylabel'" == "" local theNewLabel = subinstr("`theLabel'","`by' == ","`theMainLabel', ",.)
						if "`nobylabel'" != "" local theNewLabel = subinstr("`theLabel'","`by' == ","",.)
							label var `newvar' "`theNewLabel'"
							}
						}
					local vargroup_`x' `tempvarlist'					
					}
					
				
			local anything    = substr("`anything'",strpos("`anything'",")")+1,.) 	// Replace remaining list with everything after close-parenthesis
			local ++x
			}
			
		if `x' == 1 {
			local varlist `anything'
			if "`by'" != "" {
				foreach var of varlist `varlist' {
					local tempvarlist ""
					local theMainLabel : var label `var'
					qui separate `var' , by(`by') short
					local tempvarlist "`tempvarlist' `r(varlist)'"
					foreach newvar in `r(varlist)' {
						local theLabel: var label `newvar'
						if "`nobylabel'" == "" local theNewLabel = subinstr("`theLabel'","`by' == ","`theMainLabel', ",.)
						if "`nobylabel'" != "" local theNewLabel = subinstr("`theLabel'","`by' == ","",.)
						label var `newvar' "`theNewLabel'"
						}
					local varlist `tempvarlist'
					}
				}
			}
		else {
			forvalues i = 1/`x' {
				local varlist `varlist' `vargroup_`i'' // Compile full variable list.
				}
			}
			
		local n_vargroups = `x'
	
* Collapse and reshape so that each bar has an observation. This means one observation will correspond to one variable for one over-group.

	local x = 1
	foreach var of varlist `varlist' {
		local varname_`x' `var'
		local varlab_`x' : var label `var'
		rename `var' var_`x'
		local ++ x
		}
		
	tempfile all
		qui save `all', replace
		
	gen _no = 1

	collapse (mean) var_* (sum) _no, fast by(`over')
		qui reshape long var_, j(varid) i(`over')
		rename var_ mean
		
		qui count
		qui gen varname = ""
		qui gen varlabel = ""
		
		forvalues i = 1/`r(N)' {
			local var = varid[`i']
			qui replace varname = "`varname_`var''" in `i'
			qui replace varlabel = "`varlab_`var''" in `i'
			}
	
	if "`se'" == "se" {
	
		tempfile means
			qui save `means', replace
			
		use `all', clear
		
		collapse (`serrstat') var_* , fast by(`over')
	
			qui reshape long var_, j(varid) i(`over')
			rename var_ sem
			
		qui merge 1:1 varid `over' using `means', nogen
		
		gen upper = mean + 1.96*sem
		gen lower = mean - 1.96*sem
		
		}
			
* Sort groups and set up for graphing

	qui egen group = group(`over')		
		
	qui gen vargroup = 1
		
	if `n_vargroups' > 1 {
		forvalues i = 1/`n_vargroups' {
			foreach varname in `vargroup_`i'' {
				qui replace vargroup = `i' if regexm("`vargroup_`i''",varname)
				}
			}
		}
	
	if "`ascending'" != "" | "`descending'" != "" {
		gen sortmean = `sort'mean
		
		qui sort group vargroup sortmean varid
		tempfile a
			qui save `a', replace
			qui keep if `sortLogic'
				gen varorder = _n
				keep varid varorder
			qui merge 1:m varid using `a', nogen
		}
	else {
		gen varorder = varid
		}
		
	qui sort vargroup varorder group
	
	qui count
	
	qui gen x = 1
	local theLastVarGroup = vargroup[1]
	local theLastVarID = varorder[1]
	
	local x = 1
	
	forvalues i = 2/`r(N)' {
		
		gen meantest = (mean == .)
		local skipSlot = meantest[`i']
		drop meantest
	
		if !`skipSlot' local ++x
	
		local theVarGroup = vargroup[`i']
		local theVarID = varorder[`i']
		
		if `theVarGroup' != `theLastVarGroup' {
			local ++x
			}
		if `theVarID' != `theLastVarID' {
			local ++x
			}
			
		qui replace x = `x' in `i'
		
		local theLastVarGroup = `theVarGroup'
		local theLastVarID = `theVarID'
		
		}
		
	qui sum x
		if "`vertical'" == "" qui replace x = `r(max)' - x
		
	qui separate x, by(group)
		qui sum group
		local n_groups = `r(max)'
		
	forvalues i = 1/`n_groups' {
	
		if regexm(`"`barlook'"',`"`i'"') {
		
			local k = strpos("`barlook'","`i'")+2 
			local j = `i' + 1
			local j = strpos("`barlook'","`j'") - `k' +2
			if `j' <= 0 local j "."
			
	
			local theoptions = substr("`barlook'",`k',`j'-3)
						
			}
	
		local meanplots `meanplots' (bar mean x`i', `horizontal' `theoptions')
		}

	
* Prepare graph labels
	
	qui sum varid
	
	forvalues i = 1/`r(max)' {
		qui sum x if varid == `i'
		local theXMean = `r(mean)' + .8
		if "`noovlabel'" != "" local varlab_`i' = substr("`varlab_`i''",strpos("`varlab_`i''",",")+2,.)
		local theXLabels `" `theXLabels' `theXMean' "`varlab_`i''" "'
		}
		
	qui sum group
	if `r(max)' > 1 {
		forvalues i = 1/`r(max)'{
			local comma ""
			foreach varname of varlist `over' {
				qui sum `varname' if group == `i'
				local theVarMean = `r(mean)'
				local theValLabel : label (`varname') `theVarMean'
				local theGroupLogic_`i' "`theGroupLogic_`i''`comma' `theValLabel'"
				local comma ","
				}
			qui sum _no if group == `i'
			local theLegendOrder `"`theLegendOrder' `i' "`theGroupLogic_`i'' (N=`r(mean)')" "'
			}
		}
	else {
		local theLegendOff off
		}
		
	format mean %9.2f
	gen zero = 0 

* Print using datasheet
	
	if `"`using'"' != `""' {
	
		if "`se'" != "" local sestats upper lower
		if "`over'" != "_over" local overstats `over'
		
		gen val2 = string(round(mean,0.01))
		
		label var varname "Varname"
		label var varlabel "Variable"
		label var val2 "Mean Value"
		cap label var upper "Upper Bound"
		cap label var lower "Lower Bound"
		
		export excel varname varlabel `overstats' val2 `sestats' `using', first(varl) replace
		}
	
* Graph

	tw `meanplots' `seplot' `blabplot' (scatter `axisfix', ms(i)), `labelaxis'lab(`theXLabels', angle(0) nogrid notick labs(medsmall)) yscale(noline) xscale(noline) ///
		`addStats' `valueaxis'lab(, angle(0) nogrid) `labelaxis'tit(" ") `valueaxis'tit(" ") legend(pos(6) `theLegendOff' order(`theLegendOrder') region(lc(white)) ) ///
		graphregion(color(white)) ///
		`options'
		
		if `"`save'"' != `""' {
			graph save `save', replace
			}
			
		if `"`export'"' != `""' {
			graph export `export', width(4000) replace
			}		
	
* End

end
