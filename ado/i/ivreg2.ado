*! ivreg2 2.1.14  4Dec2005
*! authors cfb & mes
*! cloned from official ivreg version 5.0.9  19Dec2001
*! see end of file for version comments

*  Variable naming:
*  lhs = LHS endogenous
*  endo = RHS endogenous (instrumented)
*  inexog = included exogenous (instruments)
*  exexog = excluded exogenous (instruments)
*  iv = {inexog exexog} = all instruments
*  rhs = {endo inexog} = RHS regressors
*  1 at the end of the name means the varlist after duplicates and collinearities removed
*  ..1_ct at the end means a straight count of the list
*  .._ct at the end means ..1_ct with any additional detected cnts removed

program define ivreg2, eclass byable(recall) sortpreserve
	version 8.2
	local lversion 02.1.14
	local ivreg2_cmd "ivreg2"

	if replay() {
		syntax [, FIRST FFIRST RF Level(integer $S_level) NOHEader NOFOoter dropfirst droprf /*
			*/ EForm(string) PLUS VERsion]
		if "`version'" != "" & "`first'`ffirst'`rf'`noheader'`nofooter'`dropfirst'`droprf'`eform'`plus'" != "" {
			di as err "option version not allowed"
			error 198
		}
		if "`version'" != "" {
			di in gr "`lversion'"
			ereturn clear
			ereturn local version `lversion'
			exit
		}
		if `"`e(cmd)'"' != "`ivreg2_cmd'"  {
			error 301
		}
		if "`e(firsteqs)'" != "" & "`dropfirst'" == "" {
* On replay, set flag so saved eqns aren't dropped
			local savefirst "savefirst"
		}
		if "`e(rfeq)'" != "" & "`droprf'" == "" {
* On replay, set flag so saved eqns aren't dropped
			local saverf "saverf"
		}
	}
	else {

		syntax [anything(name=0)] [if] [in] [aw fw pw iw/] [, /*
			*/ FIRST FFIRST SAVEFIRST SAVEFPrefix(name) SMall Beta Robust CLuster(varname) hc2 hc3 /*
			*/ GMM CUE CUEINIT(string) CUEOPTions(string) ORTHOG(string) NOConstant Hascons /*
			*/ Level(integer $S_level) NOHEader NOFOoter NOOUTput /*
			*/ DEPname(string) EForm(string) PLUS /*
			*/ BW(string) kernel(string) Tvar(varname) Ivar(varname)/*
			*/ LIML COVIV FULLER(real 0) Kclass(string) PSCore(string) /*
			*/ REDundant(string) RF SAVERF SAVERFPrefix(name) /*
			*/ B0(string) WMATRIX(string) dofminus(integer 0) nclustcheck ]

		local n 0

		gettoken lhs 0 : 0, parse(" ,[") match(paren)
		IsStop `lhs'
		if `s(stop)' { 
			error 198 
		}
		while `s(stop)'==0 { 
			if "`paren'"=="(" {
				local n = `n' + 1
				if `n'>1 { 
capture noi error 198 
di in red `"syntax is "(all instrumented variables = instrument variables)""'
exit 198
				}
				gettoken p lhs : lhs, parse(" =")
				while "`p'"!="=" {
					if "`p'"=="" {
capture noi error 198 
di in red `"syntax is "(all instrumented variables = instrument variables)""'
di in red `"the equal sign "=" is required"'
exit 198 
					}
					local endo `endo' `p'
					gettoken p lhs : lhs, parse(" =")
				}
* To enable Cragg HOLS estimator, allow for empty endo list
				local temp_ct  : word count `endo'
				if `temp_ct' > 0 {
					tsunab endo : `endo'
				}
* To enable OLS estimator with (=) syntax, allow for empty exexog list
				local temp_ct  : word count `lhs'
				if `temp_ct' > 0 {
					tsunab exexog : `lhs'
				}
			}
			else {
				local inexog `inexog' `lhs'
			}
			gettoken lhs 0 : 0, parse(" ,[") match(paren)
			IsStop `lhs'
		}
		local 0 `"`lhs' `0'"'

		tsunab inexog : `inexog'
		tokenize `inexog'
		local lhs "`1'"
		local 1 " " 
		local inexog `*'

		if "`gmm'`cue'" != "" & "`exexog'" == "" {
			di in red "option `gmm'`cue' invalid: no excluded instruments specified"
			exit 102
		}

* Process options

* Fuller implies LIML
		if "`liml'" == "" & `fuller' != 0 {
			local liml "liml"
			}

		if "`gmm'" != "" & "`cue'" != "" {
di as err "incompatible options: 2-step efficient gmm and cue gmm"
			exit 198
		}
		
* savefprefix implies savefirst
		if "`savefprefix'" != "" & "`savefirst'" == "" {
			local savefirst "savefirst"
		}

* default savefprefix is _ivreg2_
		if "`savefprefix'" == "" {
			local savefprefix "_ivreg2_"
		}

* saverfprefix implies saverf
		if "`saverfprefix'" != "" & "`saverf'" == "" {
			local saverf "saverf"
		}

* default saverfprefix is _ivreg2_
		if "`saverfprefix'" == "" {
			local saverfprefix "_ivreg2_"
		}

* LIML/kclass incompatibilities
		if "`liml'`kclass'" != "" {
			if "`bw'`kernel'" != "" {
di as err "autocorrelation-robust regression not available with LIML or k-class estimators"
			exit 198
			}
			if "`gmm'`cue'" != "" {
di as err "GMM estimation not available with LIML or k-class estimators"
			exit 198
			}
			if `fuller' < 0 {
di as err "invalid Fuller option"
			exit 198
			}
			if "`liml'" != "" & "`kclass'" != "" {
di as err "cannot use liml and kclass options together"
			exit 198
			}
* Process kclass string
			tempname kclass2
			scalar `kclass2'=real("`kclass'")
			if "`kclass'" != "" & (`kclass2' == . | `kclass2' < 0 ) {
di as err "invalid k-class option"
				exit 198
				}			
		}

* HAC estimation.
* If bw is omitted, default `bw' is empty string.
* If bw or kernel supplied, check/set `kernel'.
* Macro `kernel' is also used for indicating HAC in use.
		if "`bw'" != "" | "`kernel'" != "" {
* Need tvar only for markout with time-series stuff
* but data must be tsset for time-series operators in code to work
			if "`tvar'" == "" {
				local tvar "`_dta[_TStvar]'"
			}
			else if "`tvar'"!="`_dta[_TStvar]'" {
di as err "invalid tvar() option - data already tsset"
				exit 5
			}
			if "`ivar'" == "" {
				local ivar "`_dta[_TSpanel]'"
			}
			else if "`ivar'"!="`_dta[_TSpanel]'" {
di as err "invalid ivar() option - data already tsset"
				exit 5
			}
			if "`tvar'" == "" & "`ivar'" != "" {
di as err "missing tvar() option with ivar() option"
				exit 5
			}
			if "`ivar'`tvar'"=="" {
				capture tsset
			}
			else {
				capture tsset `ivar' `tvar'
			}
			capture local tvar "`r(timevar)'"
			capture local ivar "`r(panelvar)'"
			
			if "`tvar'" == "" {
di as err "must tsset data and specify timevar"
				exit 5
			}
			tsreport if `tvar' != .
			if `r(N_gaps)' != 0 & "`ivar'"=="" {
di in gr "Warning: time variable " in ye "`tvar'" in gr " has " /*
	*/ in ye "`r(N_gaps)'" in gr " gap(s) in relevant range"
			}

			if "`bw'" == "" {
di as err "bandwidth option bw() required for HAC-robust estimation"
				exit 102
			}
			local bw real("`bw'")
* Check it's a valid bandwidth
			if   `bw' != int(`bw') | /*
			*/   `bw' == .  | /*
			*/   `bw' <= 0 {
di as err "invalid bandwidth in option bw() - must be integer > 0"
				exit 198
			}
* Convert bw macro to simple integer
			local bw=`bw'

* Check it's a valid kernel
			local validkernel 0
			if lower(substr("`kernel'", 1, 3)) == "bar" | "`kernel'" == "" {
* Default kernel
				local kernel "Bartlett"
				local window "lag"
				local validkernel 1
				if `bw'==1 {
di in ye "Note: kernel=Bartlett and bw=1 implies zero lags used.  Standard errors and"
di in ye "      test statistics are not autocorrelation-consistent."
				}
			}
			if lower(substr("`kernel'", 1, 3)) == "par" {
				local kernel "Parzen"
				local window "lag"
				local validkernel 1
				if `bw'==1 {
di in ye "Note: kernel=Parzen and bw=1 implies zero lags used.  Standard errors and"
di in ye "      test statistics are not autocorrelation-consistent."
				}
			}
			if lower(substr("`kernel'", 1, 3)) == "tru" {
				local kernel "Truncated"
				local window "lag"
				local validkernel 1
			}
			if lower(substr("`kernel'", 1, 9)) == "tukey-han" | lower("`kernel'") == "thann" {
				local kernel "Tukey-Hanning"
				local window "lag"
				local validkernel 1
				if `bw'==1 {
di in ye "Note: kernel=Tukey-Hanning and bw=1 implies zero lags.  Standard errors and"
di in ye "      test statistics are not autocorrelation-consistent."
				}
			}
			if lower(substr("`kernel'", 1, 9)) == "tukey-ham" | lower("`kernel'") == "thamm" {
				local kernel "Tukey-Hamming"
				local window "lag"
				local validkernel 1
				if `bw'==1 {
di in ye "Note: kernel=Tukey-Hamming and bw=1 implies zero lags.  Standard errors and"
di in ye "      test statistics are not autocorrelation-consistent."
				}
			}
			if lower(substr("`kernel'", 1, 3)) == "qua" | lower("`kernel'") == "qs" {
				local kernel "Quadratic spectral"
				local window "spectral"
				local validkernel 1
			}
			if lower(substr("`kernel'", 1, 3)) == "dan" {
				local kernel "Daniell"
				local window "spectral"
				local validkernel 1
			}
			if lower(substr("`kernel'", 1, 3)) == "ten" {
				local kernel "Tent"
				local window "spectral"
				local validkernel 1
			}
			if ~`validkernel' {
				di in red "invalid kernel"
				exit 198
			}
		}

		if "`kernel'" != "" & "`cluster'" != "" {
di as err "cannot use HAC kernel estimator with -cluster- option"
				exit 198
		}

		if "`orthog'`redundant'" != "" {
			capture tsunab orthog    : `orthog'
			capture tsunab redundant : `redundant'
			}

		if "`hc2'`hc3'" != "" {
			if "`hc2'"!="" {
				di in red "option `hc2' invalid"
			}
			else	di in red "option `hc3' invalid"
			exit 198
		}

		if "`beta'" != "" {
			di in red "option `beta' invalid"
			exit 198
		}

		if "`hascons'" != "" & "`noconstant'"=="" {
			local noconstant "noconstant"
		}

* Weights
* fweight and aweight accepted as is
* iweight not allowed with robust or gmm and requires a trap below when used with summarize
* pweight is equivalent to aweight + robust
*   but in HAC case, robust implied by `kernel' rather than `robust'

		tempvar wvar
		if "`weight'" == "fweight" | "`weight'"=="aweight" {
			local wtexp `"[`weight'=`exp']"'
			gen double `wvar'=`exp'
		}
		if "`weight'" == "fweight" & "`kernel'" !="" {
			di in red "fweights not allowed (data are -tsset-)"
			exit 101
		}
		if "`weight'" == "iweight" {
			if "`robust'`cluster'`gmm'`kernel'" !="" {
				di in red "iweights not allowed with robust or gmm"
				exit 101
			}
			else {
				local wtexp `"[`weight'=`exp']"'
				gen double `wvar'=`exp'
			}
		}
		if "`weight'" == "pweight" {
			local wtexp `"[aweight=`exp']"'
			gen double `wvar'=`exp'
			local robust "robust"
		}

* If no kernel (=no HAC) then gmm implies (heteroskedastic-) robust
		if "`kernel'" == "" & "`gmm'" != "" {
			local robust "robust"
		}
		if `dofminus' > 0 {
			local dofmopt "dofminus(`dofminus')"
		}

		marksample touse
		markout `touse' `lhs' `inexog' `exexog' `endo' `cluster' `tvar', strok

* Weight statement
		if "`weight'" ~= "" {
			sum `wvar' if `touse' `wtexp', meanonly
di in gr "(sum of wgt is " %14.4e `r(sum_w)' ")"
		}

* Set local macro T and check that bw < T
* Also make sure only used sample is checked
		if "`bw'" != "" {
			sum `tvar' if `touse', meanonly
			local T = r(max)-r(min)+1
			if `bw' > `T' {
di as err "invalid bandwidth in option bw() - cannot exceed timespan of data"
				exit 198
			}
		}

************* Collinearities and duplicates block *****************

		if "`noconstant'" != "" {
			local rmcnocons "nocons"
		}

* Check for duplicates of variables
* To mimic official ivreg, in the case of duplicates,
* (1)  inexog > endo
* (2)  inexog > exexog
* (3)  endo + exexog = inexog, as if it were "perfectly predicted"
* Below is (1) and (2), and instead of (3),
* (3') endo > exexog, i.e.,
* Cross-list priority: (1) inexog; (2) endo; (3) exexog.
		local dupsen1 : list dups endo
		local endo1   : list uniq endo
		local dupsex1 : list dups exexog
		local exexog1 : list uniq exexog
		local dupsin1 : list dups inexog
		local inexog1 : list uniq inexog
* Remove inexog from endo
		local dupsen2 : list endo1 & inexog1
		local endo1   : list endo1 - inexog1
* Remove inexog from exexog
		local dupsex2 : list exexog1 & inexog1
		local exexog1 : list exexog1 - inexog1
* Remove endo from exexog
		local dupsex3 : list exexog1 & endo1
		local exexog1 : list exexog1 - endo1
		local dups "`dupsen1' `dupsex1' `dupsin1' `dupsen2' `dupsex2' `dupsex3'"
		local dups    : list uniq dups

* Remove collinearities
		local allvars "`exexog1' `endo1' `inexog1'"
		qui _rmcoll `exexog1' `endo1' `inexog1' if `touse' `wtexp', `rmcnocons'
		local ncvars `r(varlist)'
* collin gets collinear variables to be removed
		local collin  : list allvars-ncvars
* Remove collin from exexog1
		local exexog1 : list exexog1-collin
* Remove collin from endo1
		local endo1   : list endo1-collin
* Remove collin from inexog1
		local inexog1 : list inexog1-collin

* Collinearity and duplicates warning messages, if necessary
		if "`dups'" != "" {
di in gr "Warning - duplicate variables detected"
di in gr "Duplicates:" _c
			Disp `dups', _col(16)
		}
		if "`collin'" != "" {
di in gr "Warning - collinearities detected"
di in gr "Vars dropped:" _c
			Disp `collin', _col(16)
		}

**** End of collinearities block ************

		local insts1 `inexog1' `exexog1'
		local rhs1   `endo1'   `inexog1'
		local iv1_ct       : word count `insts1'
		local rhs1_ct      : word count `rhs1'
		local endo1_ct     : word count `endo1'
		local exex1_ct     : word count `exexog1'
		local endoexex1_ct : word count `endo1' `exexog1'
		local inexog1_ct   : word count `inexog1'

		if "`noconstant'" == "" {
			local cons_ct 1
		}
		else {
			local cons_ct 0
		}

		if `rhs1_ct' > `iv1_ct' {
			di in red "equation not identified; must have at " /*
			*/ "least as many instruments not in"
			di in red "the regression as there are "           /*
			*/ "instrumented variables"
			exit 481
		}

		if `rhs1_ct' + `cons_ct' == 0 {
			di in red "error: no regressors specified"
			exit 102
		}

		if "`cluster'"!="" {
			local clopt "cluster(`cluster')"
			if "`robust'"=="" {
				local robust "robust"
			}
		}
		if "`bw'"!="" {
			local bwopt "bw(`bw')"
		}
		if "`kernel'"!="" {
			local kernopt "kernel(`kernel')"
		}
* If depname not provided (default) name is lhs variable
		if "`depname'"=="" {
			local depname `lhs'
		}


************************************************************************************************
* Cross-products and basic IV coeffs, residuals and moment conditions
		tempvar iota y2 yhat ivresid ivresid2 gresid gresid2
		tempname Nprec ysum yy yyc r2u r2c B V ivB gmmB gmmV ivest
		tempname r2 r2_a rss mss rmse sigmasq iv_s2 F Fp Fdf2
		tempname S Sinv ivZu gmmZu A XZ XZa XZb Zy ZZ ZZinv XPZX XPZXinv XPZy
		tempname YY Z2Z2 ZY Z2Y XXa XXb XX Xy Z2Z2inv XXinv
		tempname B V B1 uZWZu j jp sargmse tempmat

* Generate cross-products of y, X, Z
		qui matrix accum `A' = `lhs' `endo1' `exexog1' `inexog1' /*
			*/ if `touse' `wtexp', `noconstant'
		if "`noconstant'"=="" {
			matrix rownames `A' = `lhs' `endo1' `exexog1' /*
				*/ `inexog1' _cons
			matrix colnames `A' = `lhs' `endo1' `exexog1' /*
				*/ `inexog1' _cons
		}
		else {
			matrix rownames `A' = `lhs' `endo1' `exexog1' `inexog1'
			matrix colnames `A' = `lhs' `endo1' `exexog1' `inexog1'
		}
		if `endo1_ct' > 0 {
* X'Z is [endo1 inexog1]'[exexog1 inexog1]
			mat `XZ'=`A'[2..`endo1_ct'+1,`endo1_ct'+2...]
* Append portion corresponding to included exog if they (incl constant) exist
			if 2+`endo1_ct'+`iv1_ct'-(`rhs1_ct'-`endo1_ct') /*
					*/ <= rowsof(`A') {
				mat `XZ'=`XZ' \ /*
					*/ `A'[2+`endo1_ct'+`iv1_ct'- /*
					*/ (`rhs1_ct'-`endo1_ct')..., /*
					*/ `endo1_ct'+2...]
			}
* If included exog (incl const) exist, create XX matrix in 3 steps
			if `inexog1_ct' + `cons_ct' > 0 {
				mat `XXa'  = `A'[2..`endo1_ct'+1, 2..`endo1_ct'+1], /*
					*/ `A'[2..`endo1_ct'+1, `endoexex1_ct'+2...]
				mat `XXb'  = `A'[`endoexex1_ct'+2..., 2..`endo1_ct'+1], /*
					*/ `A'[`endoexex1_ct'+2..., `endoexex1_ct'+2...]
				mat `XX'   = `XXa' \ `XXb'
				mat `Xy'  = `A'[2..`endo1_ct'+1, 1] \ `A'[`endoexex1_ct'+2..., 1]
			}
			else {
				mat `XX'   = `A'[2..`endo1_ct'+1, 2..`endo1_ct'+1]
				mat `Xy'  = `A'[2..`endo1_ct'+1, 1]
			}
		}
		else {
* Cragg HOLS estimator with no endogenous variables
			mat `XZ'= `A'[2+`iv1_ct'-(`rhs1_ct'-`endo1_ct')..., /*
					*/ 2...]
			mat `XX'   = `A'[`endoexex1_ct'+2..., `endoexex1_ct'+2...]
			mat `Xy'  = `A'[`endoexex1_ct'+2..., 1]
		}

		mat `XX'=(`XX'+`XX'')/2
		mat `XXinv'=syminv(`XX')
		mat `Zy'=`A'[`endo1_ct'+2...,1]
		mat `ZZ'=`A'[`endo1_ct'+2...,`endo1_ct'+2...]
		mat `ZZ'=(`ZZ'+`ZZ'')/2
		mat `ZZinv'=syminv(`ZZ')
		local iv_ct = rowsof(`ZZ') - diag0cnt(`ZZinv')
		mat `YY'=`A'[1..`endo1_ct'+1, 1..`endo1_ct'+1]
		mat `ZY' = `A'[`endo1_ct'+2..., 1..`endo1_ct'+1]
		mat `XPZX'=`XZ'*`ZZinv'*`XZ''
		mat `XPZX'=(`XPZX'+`XPZX'')/2
		mat `XPZXinv'=syminv(`XPZX')
		mat `XPZy'=`XZ'*`ZZinv'*`Zy'
		mat `B' = `XPZy''*`XPZXinv''

		if "`b0'" != "" {
			matrix `B'=`b0'
		}

		mat `ivB'=`B'
		qui mat score double `yhat' = `B' if `touse'
		qui gen double `ivresid'=`lhs'-`yhat'
		capture drop `yhat'
		qui gen double `ivresid2'=`ivresid'^2
		qui gen `iota'=1
		qui gen double `y2'=`lhs'^2
* Stata summarize won't work with iweights, so must use matrix cross-product
		qui matrix vecaccum `ysum' = `iota' `y2' `lhs' `ivresid2' `wtexp' if `touse'
* Nprec is ob count from mat accum.  Use this rather than `N' in calculations
* here and below because in official -regress- `N' is rounded if iweights are used.
		scalar `Nprec'=`ysum'[1,4]
		local N=int(`Nprec')
		scalar `rss'=`ysum'[1,3]
		scalar `yy'=`ysum'[1,1]
		scalar `yyc'=`yy'-`ysum'[1,2]^2/`Nprec'
		if "`noconstant'"=="" {
			scalar `mss'=`yyc' - `rss'
		}
		else {
			scalar `mss'=`yy' - `rss'
		}

		scalar `sigmasq'=`rss'/(`Nprec'-`dofminus')
		scalar `iv_s2'=`sigmasq'
		scalar `rmse'=sqrt(`sigmasq')
* Bread of the sandwich
		qui mat vecaccum `ivZu'=`ivresid' `exexog1' `inexog1' /*
			*/ `wtexp' if `touse', `noconstant'
* End of basic IV block

*******************************************************************************************
* Start robust block for robust-HAC S and Sinv

		if "`robust'`cluster'" != "" & "`liml'"=="" & "`kclass'"=="" & "`cue'"=="" {
* Optimal weighting matrix
* If user-supplied wmatrix is used, use it, otherwise use _robust (hence weights allowed)
			if "`wmatrix'" != "" {
				matrix `Sinv'=`wmatrix'
* Need S as well only if call by HAC which isn't also GMM, but do it anyway
				mat `S'=syminv(`Sinv')
				mat `S' = (`S' + `S'') / 2
			}
			else {
* Block calculates S_0 robust matrix
* _robust has same results as
* mat accum `S'=`exexog1' `inexog1' [iweight=`ivresid'^2] if `touse'
* mat `S' = `S'*1/`Nprec'
* _robust doesn't work properly with TS variables, so must first tsrevar
				tsrevar `exexog1' `inexog1'
				local TSinsts1 `r(varlist)'
* Create identity matrix with matching col/row names
				mat `S'=I(colsof(`ivZu'))
				if "`noconstant'"=="" {
					mat colnames `S' = `TSinsts1' "_cons"
					mat rownames `S' = `TSinsts1' "_cons"
				}
				else {
					mat colnames `S' = `TSinsts1'
					mat rownames `S' = `TSinsts1'
				}
				_robust `ivresid' `wtexp' if `touse', variance(`S') `clopt' minus(0)
				if "`cluster'"!="" {
					local N_clust=r(N_clust)
				}
				mat `S' = `S'*1/`Nprec'
* Above doesn't work properly with iweights (i.e. yield same matrix as fw),
*   hence iweight trap at start
				if "`kernel'" != "" {
* HAC block for S_1 onwards matrices
					tempvar vt1
					qui gen double `vt1' = .
					tempname tt tx kw karg ow
* Use insts with TS ops removed and with iota (constant) column
					if "`noconstant'"=="" {
						local insts1c "`TSinsts1' `iota'"
					}
					else {
						local insts1c "`TSinsts1'"
					}
					local iv1c_ct   : word count `insts1c'
* "tau=0 loop" is S_0 block above for all robust code
					local tau 1
* Spectral windows require looping through all T-1 autocovariances
					if "`window'" == "spectral" {
						local TAU `T'-1
di in ye "Computing kernel ..."
					}
					else {
						local TAU `bw'
					}
					if "`weight'" == "" {
* If no weights specified, define neutral weight variables for code below
						gen byte `wvar'=1
						gen byte `ow'=1
						local wtexp `"[fweight=`wvar']"'
					}
					else {
* pweights and aweights
						summ `wvar' if `touse', meanonly
						gen double `ow' = `wvar'/r(mean)
					}
					while `tau' <= `TAU' {
						capture mat drop `tt' 
						local i 1
						while `i' <= `iv1c_ct' {
							local x : word `i' of `insts1c'
* Add lags defined with TS operators
							local Lx "L`tau'.`x'"
							local Livresid "L`tau'.`ivresid'"
							local Low "L`tau'.`ow'"
							qui replace `vt1' = `Lx'*`ivresid'* /*
								*/ `Livresid'*`Low'*`ow' if `touse'
* Use capture here because there may be insufficient observations, e.g., if
*   the IVs include lags and tau=N-1.  _rc will be 2000 in this case.
							capture mat vecaccum `tx' = `vt1' `insts1c' /*
								*/ if `touse', nocons
							if _rc == 0 {
								mat `tt' = nullmat(`tt') \ `tx'
							}
							local i = `i'+1
						}
* bw = bandwidth, karg is argument to kernel function, kw is kernel function (weight)
						scalar `karg' = `tau'/(`bw')
						if "`kernel'" == "Truncated" {
							scalar `kw'=1
						}
						if "`kernel'" == "Bartlett" {
							scalar `kw'=(1-`karg')
						}
						if "`kernel'" == "Parzen" {
							if `karg' <= 0.5 {
								scalar `kw' = 1-6*`karg'^2+6*`karg'^3
							}
							else {
								scalar `kw' = 2*(1-`karg')^3
							}
						}
						if "`kernel'" == "Tukey-Hanning" {
							scalar `kw'=0.5+0.5*cos(_pi*`karg')
						}
						if "`kernel'" == "Tukey-Hamming" {
							scalar `kw'=0.54+0.46*cos(_pi*`karg')
						}
						if "`kernel'" == "Tent" {
							scalar `kw'=2*(1-cos(`tau'*`karg')) / (`karg'^2)
						}
						if "`kernel'" == "Daniell" {
							scalar `kw'=sin(_pi*`karg') / (_pi*`karg')
						}
						if "`kernel'" == "Quadratic spectral" {
							scalar `kw'=25/(12*_pi^2*`karg'^2) /*
								*/ * ( sin(6*_pi*`karg'/5)/(6*_pi*`karg'/5) /*
								*/     - cos(6*_pi*`karg'/5) )
						}

* Need -capture-s here because tt may not exist (because of insufficient observations/lags)
						capture mat `tt' = (`tt'+`tt'')*`kw'*1/`Nprec'
						if _rc == 0 {
							mat `S' = `S' + `tt'
						}
						local tau = `tau'+1
					}
					if "`weight'" == "" {
* If no weights specified, remove neutral weight variables
						local wtexp ""
					}
				}
* To give S the right col/row names
				mat `S'=`S'+0*diag(`ivZu')

* Right approach is to adjust S by N/(N-dofminus) if NOT cluster
* because clustered S is already "adjusted"
				if "`cluster'"=="" {
					mat `S'=`S'*`Nprec'/(`Nprec'-`dofminus')
				}

				mat `S'=(`S'+`S'')/2
				mat `Sinv'=syminv(`S')
			}
		}

* End robust-HAC S and Sinv block

************************************************************************************
* Block for non-robust S and Sinv, including autocorrelation-consistent (AC).

		if "`robust'`cluster'" == "" & "`liml'" == ""  & "`kclass'"=="" & "`cue'"=="" {

* First do with S_0 (=S for simple IV)
			mat `S' = `iv_s2'*`ZZ'*(1/`Nprec')

			if "`kernel'" != "" {
* AC code for S_1 onwards matrices
				tempvar vt1
				qui gen double `vt1' = .
				tempname tt tx kw karg ow sigttj
* Use insts with TS ops removed and with iota (constant) column
				tsrevar `exexog1' `inexog1'
				local TSinsts1 `r(varlist)'
				if "`noconstant'"=="" {
					local insts1c "`TSinsts1' `iota'"
				}
				else {
					local insts1c "`TSinsts1'"
				}
				local iv1c_ct   : word count `insts1c'
* "tau=0 loop" is S_0 block above
				local tau 1
* Spectral windows require looping through all T-1 autocovariances
				if "`window'" == "spectral" {
					local TAU `T'-1
di in ye "Computing kernel ..."
				}
				else {
					local TAU `bw'
				}
				if "`weight'" == "" {
* If no weights specified, define neutral weight variables for code below
					gen byte `wvar'=1
					gen byte `ow'=1
					local wtexp `"[fweight=`wvar']"'
				}
				else {
* pweights and aweights
					summ `wvar' if `touse', meanonly
					gen double `ow' = `wvar'/r(mean)
				}
				while `tau' <= `TAU' {
					capture mat drop `tt' 
					local i 1
* errflag signals problems that make this loop's tt invalid
					local errflag 0
* Additional marksample/markout required so that treatment of MVs is consistent across all IVs
					marksample touse2
					markout `touse2' `insts1c' L`tau'.(`insts1c')
					while `i' <= `iv1c_ct' {
						local x : word `i' of `insts1c'
* Add lags defined with TS operators
						local Lx "L`tau'.`x'"
						local Low "L`tau'.`ow'"
						qui replace `vt1' = `Lx'*`Low'*`ow' if `touse' & `touse2'
* Use capture here because there may be insufficient observations, e.g., if
*   the IVs include lags and tau=N-1.  _rc will be 2000 in this case.
						capture mat vecaccum `tx' = `vt1' `insts1c' /*
							*/ if `touse', nocons
						if _rc == 0 {
							mat `tt' = nullmat(`tt') \ `tx'
						}
						local i = `i'+1
					}
					capture mat `tt' = 1/`Nprec' * `tt'
					if _rc != 0 {
						local errflag = 1
					}
					local Livresid "L`tau'.`ivresid'"
* Should weights appear here as well?
					tempvar ivLiv
					qui gen double `ivLiv' = `ivresid' * `Livresid' if `touse'
					qui sum `ivLiv' if `touse', meanonly
					scalar `sigttj' = r(sum)/`Nprec'

					capture mat `tt' = `sigttj' * `tt'
* bw = bandwidth, karg is argument to kernel function, kw is kernel function (weight)
					scalar `karg' = `tau'/(`bw')
					if "`kernel'" == "Truncated" {
						scalar `kw'=1
					}
					if "`kernel'" == "Bartlett" {
						scalar `kw'=(1-`karg')
					}
					if "`kernel'" == "Parzen" {
						if `karg' <= 0.5 {
							scalar `kw' = 1-6*`karg'^2+6*`karg'^3
						}
						else {
							scalar `kw' = 2*(1-`karg')^3
						}
					}
					if "`kernel'" == "Tukey-Hanning" {
						scalar `kw'=0.5+0.5*cos(_pi*`karg')
					}
					if "`kernel'" == "Tukey-Hamming" {
						scalar `kw'=0.54+0.46*cos(_pi*`karg')
					}
					if "`kernel'" == "Tent" {
						scalar `kw'=2*(1-cos(`tau'*`karg')) / (`karg'^2)
					}
					if "`kernel'" == "Daniell" {
						scalar `kw'=sin(_pi*`karg') / (_pi*`karg')
					}
					if "`kernel'" == "Quadratic spectral" {
						scalar `kw'=25/(12*_pi^2*`karg'^2) /*
							*/ * ( sin(6*_pi*`karg'/5)/(6*_pi*`karg'/5) /*
							*/     - cos(6*_pi*`karg'/5) )
					}

* Need -capture-s here because tt may not exist (because of insufficient observations/lags)
					capture mat `tt' = (`tt'+`tt'')*`kw'
					if _rc != 0 {
						local errflag = 1
					}
* Accumulate if tt is valid
					if `errflag' == 0 {
						capture mat `S' = `S' + `tt'
					}
					local tau = `tau'+1
				}
				if "`weight'" == "" {
* If no weights specified, remove neutral weight variables
					local wtexp ""
				}
			}
* End of AC code
* To give S the right col/row names
			mat `S'=`S'+0*diag(`ivZu')
			mat `S'=(`S'+`S'')/2
			mat `Sinv'=syminv(`S')
		}

* End of non-robust S and Sinv code (including AC)

**************************************************************************************
* Block for user-supplied weighting matrix, all cases 
		if "`wmatrix'" != "" {
			matrix `Sinv'=`wmatrix'
* Need S as well only if call by HAC which isn't also GMM, but do it anyway
			mat `S'=syminv(`Sinv')
			mat `S' = (`S' + `S'') / 2
		}
* End of block for wmatrix

***************************************************************************************
* Block for gmm 2nd step for coefficients, and robust/cluster/AC/HAC for Sargan-Hansen

* Non-robust IV, LIML, k-class, CUE do not enter
		if "`gmm'`robust'`cluster'`kernel'" != "" & "`liml'"=="" & "`kclass'"=="" & "`cue'"=="" {
* Symmetrize before call to syminv
			mat `tempmat'=`XZ'*`Sinv'*`XZ''
			mat `tempmat'=(`tempmat'+`tempmat'')/2
			mat `B1'=syminv(`tempmat')
			mat `B1'=(`B1'+`B1'')/2
			mat `B'=(`B1'*`XZ'*`Sinv'*`Zy')'
* Symmetrize before call to syminv
			mat `tempmat'=`XZ'*`Sinv'*`XZ''
			mat `tempmat'=(`tempmat'+`tempmat'')/2
			mat `V' = syminv(`tempmat')*`Nprec'
			mat `V'=(`V'+`V'')/2

			local rankS = rowsof(`Sinv') - diag0cnt(`Sinv')

			if "`b0'" != "" {
				matrix `B'=`b0'
			}
			
			capture drop `yhat'
			qui mat score double `yhat'=`B' if `touse'
			qui gen double `gresid'=`lhs'-`yhat'
			qui gen double `gresid2'=`gresid'^2
* J or Sargan statistic
			qui mat vecaccum `gmmZu'=`gresid' `exexog1' `inexog1' /*
				*/ `wtexp' if `touse', `noconstant'
			mat `uZWZu'= (`gmmZu'/`Nprec')*`Sinv'*(`gmmZu''/`Nprec')
* If non-robust, it's a Sargan, otherwise it's a J
			scalar `j' = `Nprec'*`uZWZu'[1,1]

* New rss, R2s etc if new coeffs (GMM)
			if "`gmm'"!="" {
				capture drop `ysum'
				qui matrix vecaccum `ysum' = `iota' `gresid2' /*
					*/ `wtexp' if `touse', `noconstant'
				scalar `rss'= `ysum'[1,1]
* Adjust sigma-squared for dofminus
				scalar `sigmasq'=`rss'/(`Nprec'-`dofminus')
				scalar `rmse'=sqrt(`sigmasq')
				if "`noconstant'"=="" {
					scalar `mss'=`yyc'-`rss'
				}
				else {
					scalar `mss'=`yy'-`rss'
				}
			}
		}
* End of second-step gmm code
***************************************************************************************
* Block for cue gmm

		if "`cue'" != "" {
* Set up variables and options as globals
			global IV_lhs "`lhs'"
			global IV_inexog "`inexog1'"
			global IV_endog "`endo1'"
			global IV_exexog "`exexog1'"
			global IV_wt "`wtexp'"
			global IV_opt "`noconstant' `robust' `clopt' `bwopt' `kernopt' `dofmopt'"
* `gmm' not in IV_opt because cue+gmm not allowed
* Initial values use 2-step GMM if robust
			if "`robust'`cluster'"~="" {
				local init_opt "gmm"
			}
			tempname b_init temphold
			capture _estimates hold `temphold', restore
			if _rc==1000 {
di as err "ivreg2 internal error - no room to save temporary estimation results"
di as err "Please drop one or more estimation results using -estimates drop-"
				exit 1000
			}
			qui `ivreg2_cmd' $IV_lhs $IV_inexog ($IV_endog=$IV_exexog) $IV_wt /*
				*/ if `touse', $IV_opt `init_opt'
* Trap here if just-identified
			if e(rankzz)>e(rankxx) {
				if "`cueinit'"== "" {
					mat `b_init'=e(b)
				}
				else {
					mat `b_init'=`cueinit'
				}
* Use ML for numerical optimization
				ml model d0 `ivreg2_cmd'_cue ($IV_lhs = $IV_endog $IV_inexog, `noconstant') $IV_wt /*
					*/ if `touse', maximize init(`b_init') `cueoptions' /*
					*/ crittype(neg GMM obj function -J) /*
					*/ collinear nooutput nopreserve missing noscvars
			}
			else {
di in ye "Equation exactly-identified: CUE and 2-step GMM coincide"
			}
* Remove equation number from column names
			mat `B'=e(b)
			mat colnames `B' = _:
* Last call to get vcv, j, Sinv etc.
			qui `ivreg2_cmd' $IV_lhs $IV_inexog ($IV_endog=$IV_exexog) $IV_wt /*
				*/ if `touse', $IV_opt b0(`B')
* Save all results
			mat `V'=e(V)
			mat `S'=e(S)
* e(W) currently not saved unless GMM
			capture mat `Sinv'=e(W)

			local rankS = e(rankS)
			scalar `j'=e(sargan)
			if `j' == . {
				scalar `j'=e(j)
			}
			if "`cluster'" != "" {
				local N_clust=e(N_clust)
			}
			capture drop `yhat'
			qui mat score double `yhat'=`B' if `touse'
			qui gen double `gresid'=`lhs'-`yhat'
			qui gen double `gresid2'=`gresid'^2
			capture drop `ysum'
			qui matrix vecaccum `ysum' = `iota' `gresid2' /*
				*/ `wtexp' if `touse', `noconstant'
			scalar `rss'= `ysum'[1,1]
* Adjust sigma-squared for dofminus
			scalar `sigmasq'=`rss'/(`Nprec'-`dofminus')
			scalar `rmse'=sqrt(`sigmasq')
			if "`noconstant'"=="" {
				scalar `mss'=`yyc'-`rss'
			}
			else {
				scalar `mss'=`yy'-`rss'
			}

			macro drop IV_lhs IV_inexog IV_endog IV_exexog IV_wt IV_opt
			capture _estimates unhold `temphold'
		}

*********************************************************************************
* IV code
		if ("`robust'`cluster'`gmm'`kernel'" == "") /*
			*/	& ("`liml'"=="") & ("`kclass'"=="") & ("`cue'"=="") {
* Straight IV block for Sargan, coeffs and VCV, no small sample correction
			mat `uZWZu' = (`ivZu'/`Nprec')*`Sinv'*(`ivZu''/`Nprec')
			scalar `j' = `Nprec'*`uZWZu'[1,1]
			mat `B'=`ivB'

* XPZXinv may not exist if there were problems with the original regression
			capture mat `V'=`XPZXinv'*`iv_s2'
			capture mat `V'=(`V'+`V'')/2
		}

		if ("`robust'`cluster'`kernel'" != "") & ("`gmm'" == "") /*
			*/	& ("`liml'"=="") & ("`kclass'"=="") & ("`cue'"=="") {
* Block for IV coeffs with robust SEs of all sorts, no small-sample correction
			mat `B'=`ivB'
			mat `V'=`XPZXinv'*`XZ'*`ZZinv'*`S'*`Nprec'*  /*
				*/ `ZZinv'*`XZ''*`XPZXinv'
			mat `V'=(`V'+`V'')/2
		}

********************************************************************************
* LIML and kclass code
		if "`liml'`kclass'" != "" {

			tempname W W1 Evec Eval Evaldiag target lambda lambda2 khs XhXh XhXhinv ll
			tempvar lresid lresid2

			if "`kclass'" == "" {
* LIML block
				matrix `W'  = `YY' - `ZY''*`ZZinv'*`ZY'

				if `inexog1_ct' + `cons_ct' > 0 {
					mat `Z2Y'  = `A'[`endoexex1_ct'+2..., 1..`endo1_ct'+1]
					mat `Z2Z2' = `A'[`endoexex1_ct'+2..., `endoexex1_ct'+2...]
					mat `Z2Z2'=(`Z2Z2'+`Z2Z2'')/2
					mat `Z2Z2inv' = syminv(`Z2Z2')
					matrix `W1' = `YY' - `Z2Y''*`Z2Z2inv'*`Z2Y'
				}
				else {
* Special case of no included exogenous (incl constant)
					matrix `W1' = `YY'
				}
				matrix `W'=(`W'+`W'')/2
				matrix symeigen `Evec' `Eval' = `W'
				matrix `Evaldiag' = diag(`Eval')
* Replace diagonal elements of Evaldiag with the element raised to the power (-1/2)
				local i 1
				while `i' <= rowsof(`Evaldiag') {
* Need to use capture because with collinearities, diag may be virtually zero
* ... but actually negative
					capture matrix `Evaldiag'[`i',`i'] = /*
						*/ `Evaldiag'[`i',`i']^(-0.5)
					local i = `i'+1
				}
				matrix `target' = (`Evec'*`Evaldiag'*`Evec'') * `W1' /*
					*/ * (`Evec'*`Evaldiag'*`Evec'')
* Re-use macro names
				matrix `target'=(`target'+`target'')/2
				matrix symeigen `Evec' `Eval' = `target'
* Get smallest eigenvalue
* Note that collinearities can yield a nonsense eigenvalue appx = 0
* and just-identified will yield an eigenvalue that is ALMOST exactly = 1
* so require it to be >= 0.9999999999.
				local i 1
				scalar `lambda'=.
				scalar `lambda2'=.
				while `i' <= colsof(`Eval') {
					if (`lambda' > `Eval'[1,`i']) & (`Eval'[1,`i'] >=0.9999999999) {
						scalar `lambda2' = `lambda'
						scalar `lambda' = `Eval'[1,`i']
					}
					local i = `i'+1
				}

				if `fuller'==0 {
* Basic LIML.  Macro kclass2 is the scalar.
					scalar `kclass2'=`lambda'
				}
				else {
* Fuller LIML
					if `fuller' > (`N'-`iv_ct') {
di as err "error: invalid choice of Fuller LIML parameter"
						exit 198
					}
					scalar `kclass2' = `lambda' - `fuller'/(`N'-`iv_ct')
				}
* End of LIML block
			}

			mat `XhXh'=(1-`kclass2')*`XX'+`kclass2'*`XPZX'
			mat `XhXh'=(`XhXh'+`XhXh'')/2
			mat `XhXhinv'=syminv(`XhXh')
			mat `B'=`Xy''*`XhXhinv'*(1-`kclass2') + `kclass2'*`Zy''*`ZZinv'*`XZ''*`XhXhinv'
			capture drop `yhat'
			qui mat score double `yhat'=`B' if `touse'
			qui gen double `lresid'=`lhs' - `yhat'
			qui gen double `lresid2'=`lresid'^2
			capture drop `ysum'
			qui matrix vecaccum `ysum' = `iota' `lresid2' /*
				*/ `wtexp' if `touse', `noconstant'
			scalar `rss'= `ysum'[1,1]
* Adjust sigma-squared for dofminus
			scalar `sigmasq'=`rss'/(`Nprec'-`dofminus')
			scalar `rmse'=sqrt(`sigmasq')
			if "`noconstant'"=="" {
				scalar `mss'=`yyc'-`rss'
			}
			else {
				scalar `mss'=`yy'-`rss'
			}
			if "`liml'" != "" {
* Anderson-Rubin overid stat
* Note dofminus is required because unlike Sargan and 2-step GMM J, doesn't derive from S
				scalar `j'=(`Nprec'-`dofminus')*ln(`lambda')
			}
			else {
* Sargan stat for k-class
				tempvar kclassZu
				qui mat vecaccum `kclassZu'=`lresid' `exexog1' `inexog1' /*
					*/ `wtexp' if `touse', `noconstant'
				mat `S' = `sigmasq'*`ZZ'*(1/`Nprec')
				mat `S' = (`S'+`S'')/2
				mat `Sinv' = syminv(`S')
				mat `uZWZu' = (`kclassZu'/`Nprec')*`Sinv'*(`kclassZu''/`Nprec')
				scalar `j' = `Nprec'*`uZWZu'[1,1]
			}

			if "`robust'`cluster'"!="" {
* Robust SE block
				tsrevar `exexog1' `inexog1'
				local TSinsts1 `r(varlist)'
* Create identity matrix with matching col/row names
				mat `S'=I(colsof(`ivZu'))
				if "`noconstant'"=="" {
					mat colnames `S' = `TSinsts1' "_cons"
					mat rownames `S' = `TSinsts1' "_cons"
				}
				else {
					mat colnames `S' = `TSinsts1'
					mat rownames `S' = `TSinsts1'
				}
				_robust `lresid' `wtexp' if `touse', variance(`S') `clopt' minus(0)
				if "`cluster'"!="" {
					local N_clust=r(N_clust)
				}
				mat `S' = `S'*1/`Nprec'
* To give S the right col/row names
				mat `S'=`S'+0*diag(`ivZu')
				mat `S'=(`S'+`S'')/2
				mat `Sinv'=syminv(`S')
				local rankS = rowsof(`Sinv') - diag0cnt(`Sinv')
				if "`coviv'"== "" {
* Use LIML or k-class cov matrix
					mat `V'=`XhXhinv'*`XZ'*`ZZinv'*`S'*`Nprec'*  /*
						*/ `ZZinv'*`XZ''*`XhXhinv'
				}
				else {
* Use IV cov matrix
					mat `V'=`XPZXinv'*`XZ'*`ZZinv'*`S'*`Nprec'*  /*
						*/ `ZZinv'*`XZ''*`XPZXinv'
				}
				mat `V'=(`V'+`V'')/2
			}

			if "`robust'`cluster'"=="" {
				mat `S' = `sigmasq'*`ZZ'*(1/`Nprec')
				if "`coviv'"== "" {
* LIML or k-class cov matrix
					mat `V'=`sigmasq'*`XhXhinv'
				}
				else {
* IV cov matrix
					mat `V'=`sigmasq'*`XPZXinv'
				}
				mat `V'=(`V'+`V'')/2
			}
		}
* End of LIML/k-class block

********************************************************************************
* Counts, dofs, F-stat, small-sample corrections

* Counts modified to include constant if appropriate
		if "`noconstant'"=="" {
			local iv1_ct  = `iv1_ct' + 1
			local rhs1_ct = `rhs1_ct' + 1
		}
* Correct count of rhs variables accounting for dropped collinear vars
* Count includes constant

		local rhs_ct = rowsof(`XX') - diag0cnt(`XXinv')

		if "`noconstant'"=="" {
			local df_m = `rhs_ct' - 1
		}
		else {
			local df_m = `rhs_ct'
		}

		if "`cluster'"=="" {
* Residual dof adjusted for dofminus
			local df_r = `N' - `rhs_ct' - `dofminus'
		}
		else {
* To match Stata, subtract 1 (why 1 and not `rhs_ct' is a mystery)
			local df_r = `N_clust' - 1
		}

* Sargan-Hansen J dof and p-value
		local jdf = `iv_ct' - `rhs_ct'
		if `jdf' == 0 {
			scalar `j' = 0
		}
		else {
			scalar `jp' = chiprob(`jdf',`j')
		}

* Small sample corrections for var-cov matrix.
* If robust, the finite sample correction is N/(N-K), and with no small
* we change this to 1 (a la Davidson & MacKinnon 1993, p. 554, HC0).
* If cluster, the finite sample correction is (N-1)/(N-K)*M/(M-1), and with no small
* we change this to 1 (a la Wooldridge 2002, p. 193), where M=number of clusters.
* In the adj of the V matrix for non-small, we use Nprec instead of N because
* iweights rounds off N.  Note that iweights are not allowed with robust
* but we use Nprec anyway to maintain consistency of code.

		if "`small'" != "" {
			if "`cluster'"=="" {
				matrix `V'=`V'*(`Nprec'-`dofminus')/(`Nprec'-`rhs_ct'-`dofminus')
			}
			else {
				matrix `V'=`V'*(`Nprec'-1)/(`Nprec'-`rhs_ct') /*
					*/		* `N_clust'/(`N_clust'-1)
			}
			scalar `sigmasq'=`rss'/(`Nprec'-`rhs_ct'-`dofminus')
			scalar `rmse'=sqrt(`sigmasq')
		}

**********************

		scalar `r2u'=1-`rss'/`yy'
		scalar `r2c'=1-`rss'/`yyc'
		if "`noconstant'"=="" {
			scalar `r2'=`r2c'
			scalar `r2_a'=1-(1-`r2')*(`Nprec'-1)/(`Nprec'-`rhs_ct'-`dofminus')
		}
		else {
			scalar `r2'=`r2u'
			scalar `r2_a'=1-(1-`r2')*`Nprec'/(`Nprec'-`rhs_ct'-`dofminus')
		}

* Fstat
* To get it to match Stata's, must post separately with dofs and then do F stat by hand
*   in case weights generate non-integer obs and dofs
* Create copies so they can be posted
		tempname FB FV
		mat `FB'=`B'
		mat `FV'=`V'
		capture ereturn post `FB' `FV'
* If the cov matrix wasn't positive definite, the post fails with error code 506
		local rc = _rc
		if `rc' != 506 {
			local Frhs1 `rhs1'
			capture test `Frhs1'
			if "`small'" == "" {
				if "`cluster'"=="" {
					capture scalar `F' = r(chi2)/`df_m' * `df_r'/(`Nprec'-`dofminus')
				}
				else {
					capture scalar `F' = r(chi2)/`df_m' * /*
						*/ (`N_clust'-1)/`N_clust' * (`Nprec'-`rhs_ct')/(`Nprec'-1)
				}
			}
			else {
				capture scalar `F' = r(chi2)/`df_m'
			}
			capture scalar `Fp'=Ftail(`df_m',`df_r',`F')
			capture scalar `Fdf2'=`df_r'
		}

* If j==. or vcv wasn't full rank, then vcv problems and F is meaningless
		if `j' == . | `rc'==506 {
			scalar `F' = .
			scalar `Fp' = .
		}

* End of counts, dofs, F-stat, small sample corrections
****************************************************************************************

* orthog option: C statistic (difference of Sargan statistics)
* Requires j dof from above
		if "`orthog'"!="" {
			tempname cj cstat cstatp
* Initialize cstat
*			local cstat = 0
			scalar `cstat' = 0
* Each variable listed must be in instrument list.
* To avoid overwriting, use cendo, cinexog1, cexexog, cendo_ct, cex_ct
			local cendo1   "`endo1'"
			local cinexog1 "`inexog1'"
			local cexexog1 "`exexog1'"
			local cinsts1  "`insts1'"
			local crhs1    "`rhs1'"
			local clist1   "`orthog'"
			local clist_ct  : word count `clist1'

* Check to see if c-stat vars are in original list of all ivs
* cinexog1 and cexexog1 are after c-stat exog list vars have been removed
* cendo1 is endo1 after included exog being tested has been added
			foreach x of local clist1 {
				local llex_ct : word count `cexexog1'
				Subtract cexexog1 : "`cexexog1'" "`x'"
				local cex1_ct : word count `cexexog1'
				local ok = `llex_ct' - `cex1_ct'
				if (`ok'==0) {
* Not in excluded, check included and add to endog list if it appears
					local llin_ct : word count `cinexog1'
					Subtract cinexog1 : "`cinexog1'" "`x'"
					local cin1_ct : word count `cinexog1'
					local ok = `llin_ct' - `cin1_ct'
					if (`ok'==0) {
* Not in either list
di in r "Error: `x' listed in orthog() but does not appear as exogenous." 
						error 198
					}
					else {
						local cendo1 "`cendo1' `x'"
					}
				}
			}

* If robust, HAC/AC or GMM (but not LIML or IV), create optimal weighting matrix to pass to ivreg2
*   by extracting the submatrix from the full S and then inverting.
*   This guarantees the C stat will be non-negative.  See Hayashi (2000), p. 220. 
			if "`robust'`cluster'`gmm'`kernel'" != "" & "`liml'"=="" {
				tempname CSa CS CSinv
				if "`noconstant'" !="" {
					local cexin "`cexexog1' `cinexog1'"
				}
				else  {
					local cexin "`cexexog1' `cinexog1' _cons"
				}
				local v1 1
				foreach x of local cexin {
					if `v1'==1 {
					mat `CSa' = `S'[`"`x'"',.]
					local v1 0
					}
					else {
					mat `CSa' = `CSa' \ `S'[`"`x'"',.]
					}
				}
				local v1 1
				foreach x of local cexin {
					if `v1'==1 {
						mat `CS' = `CSa'[.,`"`x'"']
						local v1 0
					}
					else {
					mat `CS' = `CS', `CSa'[.,`"`x'"']
					}
				}
* Symmetrize before call to syminv
				mat `CS'=(`CS'+`CS'')/2
				mat `CSinv'=syminv(`CS')
			}

* Calculate C statistic with recursive call to ivreg2
* Collinearities may cause problems, hence -capture-.
			capture {
				capture _estimates hold `ivest', restore
				if _rc==1000 {
di as err "ivreg2 internal error - no room to save temporary estimation results"
di as err "Please drop one or more estimation results using -estimates drop-"
					exit 1000
				}
				if "`robust'`cluster'`gmm'`kernel'" == "" | "`liml'"!="" {
* Straight IV or LIML block
					capture `ivreg2_cmd' `lhs' `cinexog1' /*
						*/ (`cendo1'=`cexexog1') if `touse' /*
						*/ `wtexp', `noconstant' /*
						*/ `small' `liml' `dofmopt' `options'
					local rc = _rc
					if `rc' == 481 {
						scalar `cstat' = 0
						local cstatdf = 0
						}
					else {
						if "`liml'"== "" {
* If not LIML, use MSE from original Sargan test to ensure p.d.
							scalar `cj'=e(sargan) * e(rss)/`rss'
							local cjdf=e(sargandf)
						}
						else {
							scalar `cj'=e(arubin)
							local cjdf=e(arubindf)
						}
						scalar `cstat' = `j' - `cj'
						local cstatdf  = `jdf' - `cjdf'
					}
				}
				else {
* Robust/HAC/AC/gmm block
					if "`kernel'" != "" {
						local bwopt "bw(`bw')"
						local kernopt "kernel(`kernel')"
					}
* clopt is omitted because it requires calculation of numbers of clusters, which is done
* only when S matrix is calculated
					capture `ivreg2_cmd' `lhs' `cinexog1' /*
						*/ (`cendo1'=`cexexog1') /*
						*/ if `touse' `wtexp', `noconstant' /*
						*/ `options' `small' `robust' /*
						*/ `gmm' `bwopt' `kernopt' `dofmopt' /*
						*/ wmatrix("`CSinv'")

					local rc = _rc
					if `rc' == 481 {
						scalar `cstat' = 0
						local cstatdf = 0
						}
					else {
						if "`e(vcetype)'"=="Robust" {
* Robust/HAC => J
							scalar `cj'=e(j)
							local cjdf=e(jdf)
							}
						else {
* AC => Sargan
							scalar `cj'=e(sargan)
							local cjdf=e(sargandf)
							}
						scalar `cstat' = `j' - `cj'
						local cstatdf  = `jdf' - `cjdf'
					}
				}
				_estimates unhold `ivest'
				scalar `cstatp'= chiprob(`cstatdf',`cstat')
* Collinearities may cause C-stat dof to differ from the number of variables in orthog()
* If so, set cstat=0
				if `cstatdf' != `clist_ct' {
*					local cstat = 0
					scalar `cstat' = 0
				}
			}
		}
* End of orthog block

*********************************************************************************************
* Rank identification and redundancy block

		if `endo1_ct' > 0 {
			tempname ccmat ccrealev ccimagev cc idstat iddf idp
			mat `ccmat' = `XXinv'*`XPZX'
* Need only upper LHS block, which corresponds to included endogenous
			mat `ccmat' = `ccmat'[1..`endo1_ct',1..`endo1_ct']
			mat eigenvalues `ccrealev' `ccimagev' = `ccmat'
			* Real eigenvalues are the squared canonical correlations
* The first reported cc is NOT necessarily the smallest (with mat symeigen the smallest is last).
* Sort so smallest is first.
			vecsort `ccrealev'
			scalar `cc'=`ccrealev'[1,1]
* dof adjustment needed because it doesn't use the adjusted S
			scalar `idstat' = -(`Nprec'-`dofminus')*ln(1-`cc')
			local iddf = `iv_ct' - (`rhs_ct'-1)
			scalar `idp' = chiprob(`iddf',`idstat')
		}

* LR redundancy test
		if `endo1_ct' > 0 & "`redundant'" ~= "" {
* Obtain Anderson zero rank (totally unidentified) statistic for full set of instruments
			tempname unidstat
			scalar `unidstat'=0
			forvalues thiscol=1(1)`endo1_ct' {
* dof adjustment needed because it doesn't use the adjusted S
				scalar `unidstat'=`unidstat'-(`Nprec'-`dofminus')*ln(1-`ccrealev'[1,`thiscol'])
			}
* Diff between this and the stat using the irrelevant excl IVs is chi2 with dof=#endog*#tested
			local redlist1   "`redundant'"
* XZcols are the Z columns, so can use for ZZ too
			local rXZcols : colnames `XZ'
			foreach x of local redlist1 {
				local riv_ct_a : word count `rXZcols'
				Subtract rXZcols : "`rXZcols'" "`x'"
				local riv_ct_b : word count `rXZcols'
				if `riv_ct_a' == `riv_ct_b' {
* Not in list
di in r "Error: `x' listed in redundant() but does not appear as excluded instrument." 
						error 198
					}
			}
			tempname rXZ rZZ rZZtemp rZZinv rXPZX rccmat rccrealev rccimagev runidmat runidstat
			foreach cn of local rXZcols {
				mat `rXZ' = nullmat(`rXZ') , `XZ'[1...,"`cn'"]
				mat `rZZtemp' = nullmat(`rZZtemp') , `ZZ'[1...,"`cn'"]
			}
			foreach cn of local rXZcols {
				mat `rZZ' = nullmat(`rZZ') \ `rZZtemp'["`cn'",1...]
			}
			mat `rZZ'=(`rZZ'+`rZZ'')/2
			mat `rZZinv' = syminv(`rZZ')
			mat `rXPZX' = `rXZ'*`rZZinv'*`rXZ''
			mat `rccmat' = `XXinv'*`rXPZX'
			mat `rccmat' = `rccmat'[1..`endo1_ct',1..`endo1_ct']
			mat eigenvalues `rccrealev' `rccimagev' = `rccmat'
			scalar `runidstat'=0
			forvalues thiscol=1(1)`endo1_ct' {
* dof adjustment needed because it doesn't use the adjusted S
				scalar `runidstat'=`runidstat'-(`Nprec'-`dofminus')*ln(1-`rccrealev'[1,`thiscol'])
			}
			tempname redstat redp
			local riv_ct = rowsof(`rZZ') - diag0cnt(`rZZinv')
			if `riv_ct' < `rhs_ct' {
* Not in list
di in r "Error: specification with redundant() option is unidentified (fails rank condition)" 
				error 198
			}
			local redlist_ct=`iv_ct'-`riv_ct'
			scalar `redstat' = `unidstat' - `runidstat'
			local reddf = `endo1_ct'*`redlist_ct'
			scalar `redp' = chiprob(`reddf',`redstat')
		}

* End of identification stats block

*********************************************************************************************
* Error-checking block

* Check if adequate number of observations
		if `N' <= `iv_ct' {
di in r "Error: number of observations must be greater than number of instruments"
di in r "       including constant."
			error 2001
		}

* Check if adequate number of clusters
		if "`cluster'" != "" & "`nclustcheck'" == "" {
			if `N_clust' <= `iv_ct' {
di in r "Error: number of clusters must be greater than number of instruments"
				error 498
			}
		}

* Check if robust VCV matrix is of full rank
		if "`gmm'`robust'`cluster'`kernel'" != "" {
* Robust covariance matrix not of full rank means either a singleton dummy (in which
*   case the indiv SEs are OK but no F stat or 2-step GMM is possible), or
*   there are too few clusters, or too many AC/HAC-lags, or the HAC covariance estimator
*   isn't positive definite (possible with truncated and Tukey-Hanning kernels)
			if `rankS' < `iv_ct' {
* If two-step GMM then exit with error ...
				if "`gmm'" != "" {
di in r "Error: estimated covariance matrix of moment conditions not of full rank;"
di in r "       cannot calculate optimal weighting matrix for GMM estimation."
di in r "Possible causes:"
					if "`cluster'" != "" {
di in r "  number of clusters insufficient to calculate optimal weighting matrix"
					}
					if "`kernel'" != "" {
di in r "  estimated covariance matrix of moment conditions not positive definite"
di in r "  estimated covariance matrix uses too many lags"
					}
di in r "  singleton dummy variable (dummy with 1 one and N-1 zeros or visa-versa)"
					error 498
				}
* Estimation isn't two-step GMM so continue but J, F, and C stat (if present) all meaningless
*   and VCV is also meaningless unless the cause is a singleton dummy
* Must set Sargan-Hansen j = missing so that problem can be reported in output
				else {
					scalar `j' = .
					if "`orthog'"!="" {
						scalar `cstat' = .
					}
				}
			}
		}

* End of error-checking block
********************************************************************************************

* Reduced form and first stage regression options
* Relies on proper count of (non-collinear) IVs generated earlier.
* Note that nocons option + constant in instrument list means first-stage
* regressions are reported with nocons option.  First-stage F-stat therefore
* correctly includes the constant as an explanatory variable.

		if "`rf'`saverf'`first'`ffirst'`savefirst'" != "" & (`endo1_ct' > 0) {
* Reduced form needed for AR first-stage test stat.  Also estimated if requested.
				tempname archi2 archi2p arf arfp ardf ardf_r
				doRF "`lhs'" "`inexog1'" "`exexog1'" /*
					*/ `touse' `"`wtexp'"' `"`noconstant'"' `"`robust'"' /*
					*/ `"`clopt'"' `"`bwopt'"' `"`kernopt'"' /*
					*/	`"`saverfprefix'"' /*
					*/ "`dofminus'" "`ivreg2_cmd'"
				scalar `archi2'=r(archi2)
				scalar `archi2p'=r(archi2p)
				scalar `arf'=r(arf)
				scalar `arfp'=r(arfp)
				scalar `ardf'=r(ardf)
				scalar `ardf_r'=r(ardf_r)
				local rfeq "`r(rfeq)'"
* Drop saved rf results if needed only for first-stage estimations
				if "`rf'`saverf'" == "" {
					capture estimates drop `rfeq'
				}
		}

		if "`first'`ffirst'`savefirst'" != ""  & (`endo1_ct' > 0) {

* Cragg-Donald, Anderson etc.
			tempname cdchi2 cdchi2p ccf cdf
			tempname cdeval cd
			local evcols = colsof(`ccrealev')
			mat `cdeval' = J(1,`evcols',.)
			forval i=1/`evcols' {
				mat `cdeval'[1,`i'] = `ccrealev'[1,`i'] / (1 - `ccrealev'[1,`i'])
			}
			scalar `cd'=`cdeval'[1,1]
* dofminus used because it doesn't use adjusted S
			local ddf = `Nprec'-`iv_ct'-`dofminus'
			local ndf = `exex1_ct'
			scalar `cdchi2'=`cd'*(`Nprec'-`dofminus')
			scalar `cdchi2p' = chiprob(`iddf',`cdchi2')
			scalar `cdf' =`cd'*`ddf'/`ndf'
			scalar `ccf' =`cc'*`ddf'/`ndf'

* Godfrey method of Shea partial R2 uses IV and OLS estimates without robust vcvs:
* Partial R2 = OLS V[d,d] / IV V[d,d] * IV s2 / OLS s2
* where d,d is the diagonal element corresponding to the endog regressor
* ... but this simplifies to matrices that have already been calculated:
*            = XXinv[d,d] / XPZXinv[d,d]
			tempname godfrey sols siv
			tempname firstmat sheapr2 pr2 pr2F pr2p
			mat `godfrey' = J(1,`endo1_ct',0)
			mat colnames `godfrey' = `endo1'
			mat rownames `godfrey' = "sheapr2"
			local i 1
			foreach w of local endo1 {
				mat `sols'=`XXinv'["`w'","`w'"]
				mat `siv'=`XPZXinv'["`w'","`w'"]
				mat `godfrey'[1,`i'] = `sols'[1,1]/`siv'[1,1]
				local i = `i'+1
			}

			if `iv1_ct' > `iv_ct' {
di
di in gr "Warning: collinearities detected among instruments"
di in gr "1st stage tests of excluded exogenous variables may be incorrect"
			}

			doFirst "`endo1'" "`inexog1'" "`exexog1'" /*
				*/ `touse' `"`wtexp'"' `"`noconstant'"' `"`robust'"' /*
				*/ `"`clopt'"' `"`bwopt'"' `"`kernopt'"' /*
				*/	`"`savefprefix'"' /*
				*/ `"`dofmopt'"' "`ivreg2_cmd'"

			local firsteqs "`r(firsteqs)'"
			capture mat `firstmat'=`godfrey' \ r(firstmat)
			if _rc != 0 {
di in ye "Warning: missing values encountered; first stage regression results not saved"
			}
		}
* End of first-stage regression code
**********************************************************************************************

* Post results.

* NB: Would like to use -Nprec- in obs() in case weights generate non-integer obs
*     but Stata complains.  Using -Nprec- with dof() makes no difference - seems to round it
		if "`small'"!="" {
			local NminusK = `N'-`rhs_ct'
			capture ereturn post `B' `V', dep(`depname') obs(`N') esample(`touse') /*
				*/ dof(`NminusK')
			local rc = _rc
			if `rc' == 506 {
di in red "Error: estimated variance-covariance matrix not positive-definite"
				exit 506
			}
		}
		else {
			capture ereturn post `B' `V', dep(`depname') obs(`N') esample(`touse')
			local rc = _rc
			if `rc' == 506 {
di in red "Error: estimated variance-covariance matrix not positive-definite"
				exit 506
			}
		}

		ereturn local instd `endo1'
		local insts : colnames `S'
		local insts : subinstr local insts "_cons" ""
		ereturn local insts `insts'
		ereturn local inexog `inexog'
		ereturn local exexog `exexog'
		if "`collin'`dups'" != "" {
			ereturn local collin `collin'
			ereturn local dups `dups'
			ereturn local instd1 `endo1'
			ereturn local inexog1 `inexog1'
			ereturn local exexog1 `exexog1'
		}

		if "`gmm'"!="" | "`kernel'"!="" {
			ereturn matrix W `Sinv'
		}

		if "`wmatrix'" == "" {
			ereturn matrix S `S'
		}

		if "`kernel'"!="" {
			ereturn local kernel "`kernel'"
			ereturn scalar bw=`bw'
			ereturn local tvar "`tvar'"
			if "`ivar'" ~= "" {
				ereturn local ivar "`ivar'"
			}
		}

		if "`small'"!="" {
			ereturn scalar df_r=`df_r'
			ereturn local small "small"
		}

		if "`cluster'"!="" {
			ereturn scalar N_clust=`N_clust'
			ereturn local clustvar `cluster'
		}

		if "`robust'`cluster'" != "" {
			ereturn local vcetype "Robust"
		}

		ereturn scalar df_m=`df_m'
		ereturn scalar r2=`r2'
		ereturn scalar rmse=`rmse'
		ereturn scalar rss=`rss'
		ereturn scalar mss=`mss'
		ereturn scalar r2_a=`r2_a'
		ereturn scalar F=`F'
		ereturn scalar Fp=`Fp'
		ereturn scalar Fdf2=`Fdf2'
		ereturn scalar yy=`yy'
		ereturn scalar yyc=`yyc'
		ereturn scalar r2u=`r2u'
		ereturn scalar r2c=`r2c'
		ereturn scalar rankzz=`iv_ct'
		if "`gmm'`robust'`cluster'`kernel'" != "" {
			ereturn scalar rankS=`rankS'
		}
		ereturn scalar rankxx=`rhs_ct'

		if "`liml'"!="" {
			ereturn scalar arubin=`j'
			ereturn scalar arubindf=`jdf'
			if `j' != 0  & `j' != . {
				ereturn scalar arubinp=`jp'
			}
		}
		else if ("`robust'`cluster'"=="") | ("`kclass'" != "") {
			ereturn scalar sargan=`j'
			ereturn scalar sargandf=`jdf'
			if `j' != 0  & `j' != . {
				ereturn scalar sarganp=`jp'
			}
		}
		else {
			ereturn scalar j=`j'
			ereturn scalar jdf=`jdf'
			if `j' != 0 & `j' != . {
				ereturn scalar jp=`jp'
			}
		}

		if "`orthog'"!="" {
			ereturn scalar cstat=`cstat'
			if `cstat'!=0  & `cstat' != . {
				ereturn scalar cstatp=`cstatp'
				ereturn scalar cstatdf=`cstatdf'
				ereturn local clist `clist1'
			}
		}

		if `endo1_ct' > 0 {
			ereturn scalar idstat=`idstat'
			ereturn scalar iddf=`iddf'
			ereturn scalar idp=`idp'
			ereturn matrix ccev=`ccrealev'
		}
		
		if "`redundant'"!="" {
			ereturn scalar redstat=`redstat'
			ereturn scalar redp=`redp'
			ereturn scalar reddf=`reddf'
			ereturn local redlist `redlist1'
		}
		
		if "`first'`ffirst'`savefirst'" != "" & `endo1_ct'>0 {
* Capture here because firstmat empty if mvs encountered in 1st stage regressions
			capture ereturn matrix first `firstmat'
			capture ereturn matrix cdev `cdeval'
			ereturn scalar  cdchi2=`cdchi2'
			ereturn scalar  cdchi2p=`cdchi2p'
			ereturn scalar  cdf=`cdf'
			ereturn scalar  arf=`arf'
			ereturn scalar  arfp=`arfp'
			ereturn scalar  archi2=`archi2'
			ereturn scalar  archi2p=`archi2p'
			ereturn scalar  ardf=`ardf'
			ereturn scalar  ardf_r=`ardf_r'
			ereturn local   firsteqs `firsteqs'
		}
		if "`rf'`saverf'" != "" & `endo1_ct'>0 {
			ereturn local   rfeq `rfeq'
		}

		ereturn local depvar `lhs'

		if "`liml'"!="" {
			ereturn local model "liml"
			ereturn scalar kclass=`kclass2'
			ereturn scalar lambda=`lambda'
			if `fuller' > 0 & `fuller' < . {
				ereturn scalar fuller=`fuller'
			}
		}
		else if "`kclass'" != "" {
			ereturn local model "kclass"
			ereturn scalar kclass=`kclass2'
		}
		else if "`gmm'`cue'"=="" {
			if "`endo1'" == "" {
				ereturn local model "ols"
			}
			else {
				ereturn local model "iv"
			}
		}
		else if "`gmm'"=="" {
				ereturn local model "cue"
			}
		else {
			ereturn local model "gmm"
		}

		if "`weight'" != "" { 
* corr 2005-06-28
*			ereturn local wexp "=`wtexp'"
			ereturn local wexp "=`exp'"
			ereturn local wtype `weight'
		}
		ereturn local cmd `ivreg2_cmd'
		ereturn local version `lversion'
		if "`noconstant'"!="" {
			ereturn scalar cons=0
		}
		else {
			ereturn scalar cons=1
		}
		ereturn local predict "`ivreg2_cmd'_p"
		
* pscore option
		if `"`pscore'"' != "" {
			quietly `ivreg2_cmd'_p double `pscore' if e(sample), residuals
			ereturn local pscorevars `pscore'
		}
	}
	
***************************************************************
* Display results unless ivreg2 called just to generate stats or nooutput option

	if "`b0'" == "" & "`nooutput'" == "" {
		if "`savefirst'`saverf'" != "" {
			DispStored `"`saverf'"' `"`savefirst'"'
		}
		if "`rf'" != "" {
			DispRF
		}
		if "`first'" != "" {
			DispFirst
		}
		if "`first'`ffirst'" != "" {
			DispFFirst
		}
		if "`eform'"!="" {
			local efopt "eform(`eform')"
		}
		DispMain `"`noheader'"' `"`plus'"' `"`efopt'"' `"`level'"' `"`nofooter'"'
	}

* Drop first stage estimations unless explicitly saved or if replay
	if "`savefirst'" == "" {
		local firsteqs "`e(firsteqs)'"
		foreach eqname of local firsteqs {
			capture estimates drop `eqname'
		}
		ereturn local firsteqs
	}
	
* Drop reduced form estimation unless explicitly saved or if replay
	if "`saverf'" == "" {
		local eqname "`e(rfeq)'"
		capture estimates drop `eqname'
		ereturn local rfeq
	}

end

****************************************************************

program define DispMain, eclass
	args noheader plus efopt level nofooter
	version 8.2
* Prepare for problem resulting from rank(S) being insufficient
* Results from insuff number of clusters, too many lags in HAC,
*   to calculate robust S matrix, HAC matrix not PD, singleton dummy,
*   and indicated by missing value for j stat
* Macro `rprob' is either 1 (problem) or 0 (no problem)
	capture local rprob ("`e(j)'"=="." | "`e(sargan)'"=="." | "`e(arubin)'"==".")

	if "`noheader'"=="" {
		if "`e(model)'"=="liml" {
			if "`e(instd)'"=="" {
				if "`e(vcetype)'" == "Robust" {
di in gr _n "Maximum likelihood (ML) regression with robust standard errors"
di in gr "{hline 62}"
				}
				else {
di in gr _n "Maximum likelihood (ML) regression"
di in gr "{hline 34}"
				}
			}
			else {
				if "`e(vcetype)'" == "Robust" {
di in gr _n "LIML regression with robust standard errors"
di in gr "{hline 43}"
				}
				else {
di in gr _n "Limited-Information Maximum Likelihood (LIML) regression"
di in gr "{hline 56}"
				}
			}
		}
		else if "`e(model)'"=="kclass" {
			if "`e(vcetype)'" == "Robust" {
di in gr _n "k-class regression with robust standard errors"
di in gr "{hline 46}"
			}
			else {
di in gr _n "k-class estimation"
di in gr "{hline 18}"
			}
		}
		else if "`e(model)'"=="gmm" {
			if "`e(instd)'"=="" {
di in gr _n "HOLS-GMM estimation"
di in gr "{hline 19}"
			}
			else {
di in gr _n "GMM estimation"
di in gr "{hline 14}"
			}
		}
		else if "`e(model)'"=="cue" {
			if "`e(instd)'"=="" {
di in gr _n "HOLS-CUE estimation"
di in gr "{hline 19}"
			}
			else {
di in gr _n "CUE estimation"
di in gr "{hline 14}"
			}
		}
		else {
			if "`e(instd)'"=="" {
				if "`e(vcetype)'" == "Robust" {
di in gr _n "OLS regression with robust standard errors"
di in gr "{hline 42}"
				}
				else {
di in gr _n "Ordinary Least Squares (OLS) regression"
di in gr "{hline 39}"
				}
			}
			else {
				if "`e(vcetype)'" == "Robust" {
di in gr _n "IV (2SLS) regression with robust standard errors"
di in gr "{hline 48}"
				}
				else {
di in gr _n "Instrumental variables (2SLS) regression"
di in gr "{hline 40}"
				}
			}
		}
		if "`e(model)'"=="liml" | "`e(model)'"=="kclass" {
di in gr "k               =" %7.5f `e(kclass)'
		}
		if "`e(model)'"=="liml" {
di in gr "lambda          =" %7.5f `e(lambda)'
		}
		if e(fuller) > 0 & e(fuller) < . {
di in gr "Fuller parameter=" %-5.0f `e(fuller)'
		}
		if "`e(kernel)'"!="" {
			if "`e(vcetype)'" == "Robust" {
di in gr "Heteroskedasticity and autocorrelation-consistent statistics"
			}
			else {
di in gr "Autocorrelation-consistent statistics"
			}
di in gr "  kernel=`e(kernel)'; bandwidth=`e(bw)'"
di in gr "  time variable (t):  " in ye e(tvar)
			if "`e(ivar)'" != "" {
di in gr "  group variable (i): " in ye e(ivar)
			}
		}
		di
		if "`e(clustvar)'"!="" {
di in gr "Number of clusters (" "`e(clustvar)'" ") = " in ye %-4.0f e(N_clust) _continue
		}
		else {
di in gr "                                   " _continue
		}
di in gr _col(55) "Number of obs = " in ye %8.0f e(N)

		if "`e(clustvar)'"=="" {
			local Fdf2=e(N)-e(rankxx)
		}
		else {
			local Fdf2=e(N_clust)-1
		}

di in gr _c _col(55) "F(" %3.0f e(df_m) "," %6.0f e(Fdf2) ") = "
		if e(F) < 99999 {
di in ye %8.2f e(F)
		}
		else {
di in ye %8.2e e(F)
		}
di in gr _col(55) "Prob > F      = " in ye %8.4f e(Fp)

di in gr "Total (centered) SS     = " in ye %12.0g e(yyc) _continue
di in gr _col(55) "Centered R2   = " in ye %8.4f e(r2c)
di in gr "Total (uncentered) SS   = " in ye %12.0g e(yy) _continue
di in gr _col(55) "Uncentered R2 = " in ye %8.4f e(r2u)
di in gr "Residual SS             = " in ye %12.0g e(rss) _continue
di in gr _col(55) "Root MSE      = " in ye %8.4g e(rmse)
di
	}

* Display coefficients etc.
* Unfortunate but necessary hack here: to suppress message about cluster adjustment of
*   standard error, clear e(clustvar) and then reset it after display
	local cluster `e(clustvar)'
	ereturn local clustvar
	ereturn display, `plus' `efopt' level(`level')
	ereturn local clustvar `cluster'

* Display footer
* Footer not displayed if -nofooter- option or if pure OLS, i.e., model="ols" and Sargan-Hansen=0
	if ~("`nofooter'"~="" | (e(model)=="ols" & (e(sargan)==0 | e(j)==0))) {

* Report Anderson rank ID test
		if "`e(instd)'"~="" {
di in gr _c "Anderson canon. corr. LR statistic (identification/IV relevance test):"
di in ye _col(71) %8.3f e(idstat)
di in gr _col(52) "Chi-sq(" in ye e(iddf) /* 
	       			*/  in gr ") P-val =  " in ye _col(73) %6.4f e(idp)
* LR IV redundancy statistic
			if "`e(redlist)'"!="" {
di in gr "-redundant- option:"
di in gr _c "LR IV redundancy test (redundancy of specified instruments):"
di in ye _col(71) %8.3f e(redstat)
di in gr _col(52) "Chi-sq(" in ye e(reddf) /* 
	       			*/  in gr ") P-val =  " in ye _col(73) %6.4f e(redp)
di in gr "Instruments tested: " _c
					Disp `e(redlist)', _col(23)
			}

di in smcl in gr "{hline 78}"
		}

* Report either (a) Sargan-Hansen-C stats, or (b) robust covariance matrix problem
		if `rprob' == 0 {
* Display overid statistic
			if "`e(model)'" == "liml" {
				if "`e(instd)'" != "" {
di in gr _c "Anderson-Rubin statistic (overidentification test of all instruments):"
				}
				else {
di in gr _c "Anderson-Rubin statistic (LR test of excluded instruments):"
				}
di in ye _col(72) %7.3f e(arubin)
				local overiddf e(arubindf)
				local overidp e(arubinp)
			}
			else if "`e(vcetype)'" == "Robust" & "`e(model)'" != "kclass" {
				if "`e(instd)'" != "" {
di in gr _c "Hansen J statistic (overidentification test of all instruments):"
				}
				else {
di in gr _c "Hansen J statistic (Lagrange multiplier test of excluded instruments):"
				}
di in ye _col(71) %8.3f e(j)
				local overiddf e(jdf)
				local overidp e(jp)
			}
			else {
				if "`e(instd)'" != "" {
di in gr _c "Sargan statistic (overidentification test of all instruments):"
				}
				else {
di in gr _c "Sargan statistic (Lagrange multiplier test of excluded instruments):"
				}
di in ye _col(71) %8.3f e(sargan)
				local overiddf e(sargandf)
				local overidp e(sarganp)
			}
			if e(rankxx) < e(rankzz) {
di in gr _col(52) "Chi-sq(" in ye `overiddf' /* 
	       			*/  in gr ") P-val =  " in ye _col(73) %6.4f `overidp'
			}
			else {
di in gr _col(50) "(equation exactly identified)"
			}

* Display orthog option: C statistic (difference of Sargan statistics)
			if e(cstat) != . {
* If C-stat = 0 then warn, otherwise output
				if e(cstat) > 0  {
di in gr "-orthog- option:"
					if "`e(model)'" == "liml" {
						tempname arubin_u arubindf_u arubinp_u
						scalar `arubin_u'=e(arubin)-e(cstat)
						scalar `arubindf_u'=e(arubindf)-e(cstatdf)
						scalar `arubinp_u' = chiprob(`arubindf_u',`arubin_u')
di in gr _c "Anderson-Rubin statistic (eqn. excluding suspect orthog. conditions):"
di in ye _col(71) %8.3f `arubin_u'
di in gr _col(52) "Chi-sq(" in ye `arubindf_u' /* 
	       			*/  in gr ") P-val =  " in ye _col(73) %6.4f `arubinp_u'
					}
					else if "`e(vcetype)'" == "Robust" & "`e(model)'" != "kclass" {
						tempname j_u jdf_u jp_u
						scalar `j_u'=e(j)-e(cstat)
						scalar `jdf_u'=e(jdf)-e(cstatdf)
						scalar `jp_u' = chiprob(`jdf_u',`j_u')
di in gr _c "Hansen J statistic (eqn. excluding suspect orthog. conditions): "
di in ye _col(71) %8.3f `j_u'
di in gr _col(52) "Chi-sq(" in ye `jdf_u' /* 
	       			*/  in gr ") P-val =  " in ye _col(73) %6.4f `jp_u'
					}
					else {
						tempname sargan_u sargandf_u sarganp_u
						scalar `sargan_u'=e(sargan)-e(cstat)
						scalar `sargandf_u'=e(sargandf)-e(cstatdf)
						scalar `sarganp_u' = chiprob(`sargandf_u',`sargan_u')
di in gr _c "Sargan statistic (eqn. excluding suspect orthogonality conditions):"
di in ye _col(71) %8.3f `sargan_u'
di in gr _col(52) "Chi-sq(" in ye `sargandf_u' /* 
	       			*/  in gr ") P-val =  " in ye _col(73) %6.4f `sarganp_u'
					}
di in gr _c "C statistic (exogeneity/orthogonality of suspect instruments): "
di in ye _col(71) %8.3f e(cstat)
di in gr _col(52) "Chi-sq(" in ye e(cstatdf) /* 
	       			*/  in gr ") P-val =  " in ye _col(73) %6.4f e(cstatp)
di in gr "Instruments tested:  " _c
					Disp `e(clist)', _col(23)
				}
				if e(cstat) == 0 {
di in gr _n "Collinearity/identification problems in eqn. excl. suspect orthog. conditions:"
di in gr "  C statistic not calculated for orthog option"
				}
			}
		}
		else {
* Problem exists with robust VCV - notify and list possible causes
di in r "Error: covariance matrix of moment conditions not of full rank; overidentification"
di in r "       statistic not reported, and standard errors and model tests should be"
di in r "       interpreted with caution and may be meaningless"
di in r "Possible causes:"
			if e(N_clust) < e(rankzz) {
di in r "  number of clusters insufficient to calculate robust covariance matrix"
			}
			if "`e(kernel)'" != "" {
di in r "  estimated covariance matrix of moment conditions not positive definite"
di in r "  estimated covariance matrix uses too many lags"
			}
di in r "  singleton dummy variable (dummy with 1 one and N-1 zeros or visa-versa)"
		}

		di in smcl in gr "{hline 78}"

* Warn about dropped instruments if any
* (Re-)calculate number of user-supplied instruments
		local iv1_ct : word count `e(insts)'
		local iv1_ct = `iv1_ct' + `e(cons)'

		if `iv1_ct' > e(rankzz) {
di in gr "Collinearities detected among instruments: " _c
di in gr `iv1_ct'-e(rankzz) " instrument(s) dropped"
		}

		if "`e(collin)'`e(dups)'" != "" {
* If collinearities or duplicates, abbreviated varlists saved with a 1 at the end
			local one "1"
		}
		if "`e(instd)'" != "" {
			di in gr "Instrumented:" _c
			Disp `e(instd`one')', _col(23)
		}
		if "`e(inexog)'" != "" {
			di in gr "Included instruments:" _c
			Disp `e(inexog`one')', _col(23)
		}
		if "`e(exexog)'" != "" {
			di in gr "Excluded instruments:" _c
			Disp `e(exexog`one')', _col(23)
		}
		if "`e(dups)'" != "" {
			di in gr "Duplicates:" _c
			Disp `e(dups)', _col(23)
		}
		if "`e(collin)'" != "" {
			di in gr "Dropped collinear:" _c
			Disp `e(collin)', _col(23)
		}
		di in smcl in gr "{hline 78}"
	}

end

**************************************************************************************

program define DispRF
	version 8.2
	local eqname "`e(rfeq)'"
	local depvar "`e(depvar)'"
	local strlen : length local depvar
	local strlen = `strlen'+25
di
di in gr "Reduced-form regression: `e(depvar)'"
di in smcl in gr "{hline `strlen'}"
	capture estimates replay `eqname'
	if "`eqname'"=="" | _rc != 0 {
di in ye "Unable to display reduced-form regression of `e(depvar)'."
di in ye "There may be insufficient room to store results using -estimates store-."
di in ye "Try dropping one or more estimation results using -estimates drop-."
di
	}
	else {
		estimates replay `eqname', noheader
di
	}
end

program define DispFirst
	version 8.2
	tempname firstmat ivest sheapr2 pr2 F df df_r pvalue
	mat `firstmat'=e(first)
	if `firstmat'[1,1] == . {
di
di in ye "Unable to display first-stage estimates; macro e(first) is missing"
		exit
	}
di in gr _newline "First-stage regressions"
di in smcl in gr "{hline 23}"
di
	local endo1 : colnames(`firstmat')
	local nrvars : word count `endo1'
	local firsteqs "`e(firsteqs)'"
	local nreqs : word count `firsteqs'
	if `nreqs' < `nrvars' {
di in ye "Unable to display all first-stage regressions."
di in ye "There may be insufficient room to store results using -estimates store-."
di in ye "Try dropping one or more estimation results using -estimates drop-."
di
	}
	local robust "`e(vcetype)'"
	local cluster "`e(clustvar)'"
	local kernel "`e(kernel)'"
	foreach eqname of local firsteqs {
		_estimates hold `ivest'
		capture estimates restore `eqname'
		if _rc != 0 {
di
di in ye "Unable to list stored estimation `eqname'."
di in ye "There may be insufficient room to store results using -estimates store-."
di in ye "Try dropping one or more estimation results using -estimates drop-."
di
		}
		else {
			local vn "`e(depvar)'"
di in gr "First-stage regression of `vn':"
			estimates replay `eqname', noheader
			mat `sheapr2' =`firstmat'["sheapr2","`vn'"]
			mat `pr2'     =`firstmat'["pr2","`vn'"]
			mat `F'       =`firstmat'["F","`vn'"]
			mat `df'      =`firstmat'["df","`vn'"]
			mat `df_r'    =`firstmat'["df_r","`vn'"]
			mat `pvalue'  =`firstmat'["pvalue","`vn'"]
di in gr "Partial R-squared of excluded instruments: " _c
di in ye %8.4f `pr2'[1,1]
di in gr "Test of excluded instruments:"
di in gr "  F(" %3.0f `df'[1,1] "," %6.0f `df_r'[1,1] ") = " in ye %8.2f `F'[1,1]
di in gr "  Prob > F      = " in ye %8.4f `pvalue'[1,1]
di
		}
		_estimates unhold `ivest'
	}
end

program define DispStored
	args saverf savefirst
	version 8.2
	if "`saverf'" != "" {
		local eqlist "`e(rfeq)'"
	}
	if "`savefirst'" != "" {
		local eqlist "`eqlist' `e(firsteqs)'"
	}
	local eqlist : list retokenize eqlist
di in gr _newline "Stored estimation results"
di in smcl in gr "{hline 25}" _c
	capture estimates dir `eqlist'
	if "`eqlist'" != "" & _rc == 0 {
* Estimates exist and can be listed
		estimates dir `eqlist'
	}
	else if "`eqlist'" != "" & _rc != 0 {
di
di in ye "Unable to list stored estimations."
di in ye "There may be insufficient room to store results using -estimates store-."
di in ye "Try dropping one or more estimation results using -estimates drop-."
di
	}
end

program define DispFFirst
	version 8.2
	tempname firstmat sheapr2 pr2 pr2F pr2p
	mat `firstmat'=e(first)
	if `firstmat'[1,1] == . {
di
di in ye "Unable to display summary of first-stage estimates; macro e(first) is missing"
		exit
	}
	local endo   : colnames(`firstmat')
	local nrvars : word count `endo'
	local robust   "`e(vcetype)'"
	local cluster  "`e(clustvar)'"
	local kernel   "`e(kernel)'"
	local efirsteqs  "`e(firsteqs)'"
di
di in gr _newline "Summary results for first-stage regressions"
di in smcl in gr "{hline 43}"
di
di in gr    "                Shea"
di in gr _c "Variable    | Partial R2    |    Partial R2    F("
di in ye %3.0f `firstmat'[4,1] in gr "," in ye %6.0f `firstmat'[5,1] in gr ")    P-value"
	local i = 1
	while `i' <= `nrvars' {
		local vn : word `i' of `endo'
		scalar `sheapr2'=`firstmat'[1,`i']
		scalar `pr2'=`firstmat'[2,`i']
		scalar `pr2F'=`firstmat'[3,`i']
		scalar `pr2p'=`firstmat'[6,`i']
di in y %-12s "`vn'" _col(13) in gr "|" _col(15) in y %8.4f `sheapr2' _col(29) in gr "|" /*
			*/ _col(34) in y %8.4f `pr2' _col(49) %8.2f `pr2F' _col(64) %8.4f `pr2p'
		local i = `i' + 1
	}
di
	if "`robust'`cluster'" != "" {
		if "`cluster'" != "" {
di in gr "NB: first-stage F-stat cluster-robust"
		}
		else if "`kernel'" != "" {
di in gr "NB: first-stage F-stat heteroskedasticity and autocorrelation-robust"
		}
		else {
di in gr "NB: first-stage F-stat heteroskedasticity-robust"
		}
di
	}
	else if "`kernel'" != "" {
di in gr "NB: first-stage F-stat autocorrelation-robust"
	}
	
	tempname iddf idstat idp cdchi2 cdchi2p cdf
	scalar `iddf'=e(iddf)
	scalar `idstat'=e(idstat)
	scalar `idp'=e(idp)
	scalar `cdchi2'=e(cdchi2)
	scalar `cdchi2p'=e(cdchi2p)
	scalar `cdf'=e(cdf)
di in gr "Underidentification tests:"
di in gr _col(50) "Chi-sq(" in ye `iddf' in gr ")" _col(65) "P-value"
di in ye "Anderson canon. corr. likelihood ratio stat." _col(49) %8.2f `idstat' _col(65) %7.4f `idp'
di in ye "Cragg-Donald N*minEval stat." _col(49) %8.2f `cdchi2' _col(65) %7.4f `cdchi2p'
	
di in gr "Ho: matrix of reduced form coefficients has rank=K-1 (underidentified)"
di in gr "Ha: matrix has rank>=K (identified)"
di
di in gr "Weak identification statistics:"
di in ye "Cragg-Donald (N-L)*minEval/L2 F-stat" _col(40) %8.2f `cdf'
di
	if "`robust'`cluster'" != "" {
di in gr "NB: identification statistics not robust"
	}
di
	tempname arf arfp archi2 archi2p ardf ardf_r
di in gr "Anderson-Rubin test of joint significance of"
di in gr "endogenous regressors B1 in main equation, Ho:B1=0"
* Needs to be small so that adjusted dof is reflected in F stat
	scalar `arf'=e(arf)
	scalar `arfp'=e(arfp)
	scalar `archi2'=e(archi2)
	scalar `archi2p'=e(archi2p)
	scalar `ardf'=e(ardf)
	scalar `ardf_r'=e(ardf_r)
di in gr "  F(" in ye `ardf' in gr "," in ye `ardf_r' in gr ")=" /*
		*/	_col(18) in ye %-7.2f `arf'    _col(28) in gr "P-val=" in ye %6.4f `arfp'
di in gr "  Chi-sq(" in ye `ardf' in gr ")=" /*
		*/	_col(18) in ye %-7.2f `archi2' _col(28) in gr "P-val=" in ye %6.4f `archi2p'
	if "`robust'`cluster'" != "" {
		if "`cluster'" != "" {
di in gr "NB: Anderson-Rubin stat cluster-robust"
		}
		else if "`kernel'" != "" {
di in gr "NB: Anderson-Rubin stat heteroskedasticity and autocorrelation-robust"
		}
		else {
di in gr "NB: Anderson-Rubin stat heteroskedasticity-robust"
		}
	}
	else if "`kernel'" != "" {
di in gr "NB: Anderson-Rubin stat autocorrelation-robust"
	}
	
di
	if "`cluster'" != "" {
di in gr "Number of clusters     N_clust     = " in ye %10.0f e(N_clust)
	}
di in gr "Number of observations N           = " in ye %10.0f e(N)
di in gr "Number of regressors   K           = " in ye %10.0f e(rankxx)
di in gr "Number of instruments  L           = " in ye %10.0f e(rankzz)
di in gr "Number of excluded instruments L2  = " in ye %10.0f e(ardf)

di

end

* Performs first-stage regressions

program define doFirst, rclass
	version 8.2
	args    endog	/*  variable list  (including depvar)
		*/  inexog	/*  list of included exogenous
		*/  exexog	/*  list of excluded exogenous
		*/  touse	/*  touse sample
		*/  weight	/*  full weight expression w/ []
		*/  nocons	/*
		*/  robust	/*
		*/  clopt	/*
		*/  bwopt	/*
		*/  kernopt	/*
		*/  savefprefix /*
		*/  dofmopt /*
		*/  ivreg2_cmd

	tokenize `endog'
	tempname statmat statmat1
	local i 1
	while "``i''" != "" {
		capture `ivreg2_cmd' ``i'' `inexog' `exexog' `weight' /*
				*/ if `touse', `nocons' `robust' `clopt' `bwopt' `kernopt' `dofmopt' small
		if _rc ~= 0 {
* First-stage regression failed
di in ye "Unable to estimate first-stage regression of ``i''"
			if _rc == 506 {
di in ye "  var-cov matrix of first-stage regression of ``i'' not positive-definite"
			}
		}
		else {
* First-stage regression successful
* Check if there is enough room to save results; leave one free.  Allow for overwriting.
* Max is 20-1=19 for Stata 9.0 and earlier, 300-1=299 for Stata 9.1+
			if c(stata_version) < 9.1 {
				local maxest=19
			}
			else {
				local maxest=299
			}
			local eqname "`savefprefix'``i''"
			local eqname : subinstr local eqname "." "_"
			qui estimates dir
			local est_list  "`r(names)'"
			Subtract est_list : "`est_list'" "`eqname'"
			local est_ct : word count `est_list'
			if `est_ct' < `maxest' {
				capture est store `eqname', title("First-stage regression: ``i''")
				if _rc == 0 {
					local firsteqs "`firsteqs' `eqname'"
				}
			}
			else {
di
di in ye "Unable to store first-stage regression of ``i''."
di in ye "There may be insufficient room to store results using -estimates store-."
di in ye "Try dropping one or more estimation results using -estimates drop-."
di
			}
			tempvar y2 iota xhat
			tempname ysum yy rssall rssinc pr2 F p
			quietly predict double `xhat' if `touse', xb
			local endoghat "`endoghat' `xhat'"
			quietly test `exexog'
			scalar `F'=r(F)
			scalar `p'=r(p)
			local df=r(df)
			local df_r=r(df_r)
			scalar `rssall'=e(rss)
			qui gen double `y2'=``i''^2
* Stata summarize won't work with iweights, so must use matrix cross-product
			qui gen `iota'=1
			qui matrix vecaccum `ysum' = `iota' `y2' `weight' if `touse', noconstant
			scalar `yy'=`ysum'[1,1]
* 1st stage regression without excluded exogenous
			capture `ivreg2_cmd' ``i'' `inexog' `weight' /*
				*/ if `touse', `nocons' `robust' `clopt' `bwopt' `kernopt' `dofmopt' small
			scalar `rssinc'=e(rss)
* NB: uncentered R2 for main regression is 1-rssall/yy; for restricted is 1-rssinc/yy;
*     squared semipartial correlation=(rssinc-rssall)/yy=diff of 2 R2s
* Squared partial correlation (="partialled-out R2")
			scalar `pr2'=(`rssinc'-`rssall')/`rssinc'
* End of first-stage successful block
		}
		capture {
			mat `statmat1' = (`pr2' \ `F' \ `df' \ `df_r' \ `p')
			mat colname `statmat1' = ``i''
			if `i'==1 {
				mat `statmat'=`statmat1'
			}
			else {
				mat `statmat' = `statmat' , `statmat1'
			}
		}
		local i = `i' + 1
	}
	capture mat rowname `statmat' = pr2 F df df_r pvalue
	if _rc==0 {
		return matrix firstmat `statmat'
	}
	return local firsteqs "`firsteqs'"
end

program define doRF, rclass
	version 8.2
	args    lhs		/*
		*/  inexog	/*  list of included exogenous
		*/  exexog	/*  list of excluded exogenous
		*/  touse	/*  touse sample
		*/  weight	/*  full weight expression w/ []
		*/  nocons	/*
		*/  robust	/*
		*/  clopt	/*
		*/  bwopt	/*
		*/  kernopt	/*
		*/	saverfprefix /*
		*/	dofminus /*
		*/  ivreg2_cmd

* Anderson-Rubin test of signif of endog regressors (Bo=0)
* In case ivreg2 called with adjusted dof, first stage should adjust dof as well
	tempname arf arfp archi2 archi2p ardf ardf_r tempest
	capture _estimates hold `tempest'
	if _rc==1000 {
di as err "ivreg2 internal error - no room to save temporary estimation results"
di as err "Please drop one or more estimation results using -estimates drop-"
		exit 1000
	}
* Needs to be small so that adjusted dof is reflected in F stat
	qui `ivreg2_cmd' `lhs' `inexog' `exexog' `wtexp' if `touse', /*
		*/	small `nocons' dofminus(`dofminus') `robust' `clopt' `bwopt' `kernopt'
	qui test `exexog'
	scalar `arf'=r(F)
	scalar `arfp'=r(p)
	scalar `ardf'=r(df)
	scalar `ardf_r'=r(df_r)
	if "`clopt'"=="" {
		scalar `archi2'=`arf'*`ardf'*(e(N)-`dofminus')/(e(N)-e(rankxx)-`dofminus')
	}
	else {
		scalar `archi2'=`arf'*`ardf'*e(N_clust)/r(df_r)*(e(N)-1)/(e(N)-e(rankxx))
	}
	scalar `archi2p'=chiprob(`ardf',`archi2')

* Check if there is enough room to save results; leave one free.  Allow for overwriting.
* Max is 20-1=19 for Stata 9.0 and earlier, 300-1=299 for Stata 9.1+
	if c(stata_version) < 9.1 {
		local maxest=19
	}
	else {
		local maxest=299
	}
	local eqname "`saverfprefix'`lhs'"
	local eqname : subinstr local eqname "." "_"
	qui estimates dir
	local est_list  "`r(names)'"
	Subtract est_list : "`est_list'" "`eqname'"
	local est_ct : word count `est_list'
	if `est_ct' < `maxest' {
		capture est store `eqname', title("Reduced-form regression: `lhs'")
		return local rfeq "`eqname'"
	}
	else {
di
di in ye "Unable to store reduced-form regression of `lhs'."
di in ye "There may be insufficient room to store results using -estimates store-."
di in ye "Try dropping one or more estimation results using -estimates drop-."
di
	}
	_estimates unhold `tempest'
	return scalar arf=`arf'
	return scalar arfp=`arfp'
	return scalar ardf=`ardf'
	return scalar ardf_r=`ardf_r'
	return scalar archi2=`archi2'
	return scalar archi2p=`archi2p'
end

**************************************************************************************
program define IsStop, sclass
				/* sic, must do tests one-at-a-time, 
				 * 0, may be very large */
	version 8.2
	if `"`0'"' == "[" {		
		sret local stop 1
		exit
	}
	if `"`0'"' == "," {
		sret local stop 1
		exit
	}
	if `"`0'"' == "if" {
		sret local stop 1
		exit
	}
* per official ivreg 5.1.3
	if substr(`"`0'"',1,3) == "if(" {
		sret local stop 1
		exit
	}
	if `"`0'"' == "in" {
		sret local stop 1
		exit
	}
	if `"`0'"' == "" {
		sret local stop 1
		exit
	}
	else	sret local stop 0
end

program define Disp 
	version 8.2
	syntax [anything] [, _col(integer 15) ]
	local len = 80-`_col'+1
	local piece : piece 1 `len' of `"`anything'"'
	local i 1
	while "`piece'" != "" {
		di in gr _col(`_col') "`first'`piece'"
		local i = `i' + 1
		local piece : piece `i' `len' of `"`anything'"'
	}
	if `i'==1 { 
		di 
	}
end



*  Remove all tokens in dirt from full
*  Returns "cleaned" full list in cleaned

program define Subtract		/* <cleaned> : <full> <dirt> */
	version 8.2
	args	    cleaned     /*  macro name to hold cleaned list
			*/  colon		/*  ":"
			*/  full		/*  list to be cleaned 
			*/  dirt		/*  tokens to be cleaned from full */
	
	tokenize `dirt'
	local i 1
	while "``i''" != "" {
		local full : subinstr local full "``i''" "", word all
		local i = `i' + 1
	}

	tokenize `full'			/* cleans up extra spaces */
	c_local `cleaned' `*'       
end

program define vecsort		/* Also clears col/row names */
	version 8.2
	args vmat
	tempname hold
	mat `vmat'=`vmat'+J(rowsof(`vmat'),colsof(`vmat'),0)
	local lastcol = colsof(`vmat')
	local i 1
	while `i' < `lastcol' {
		if `vmat'[1,`i'] > `vmat'[1,`i'+1] {
			scalar `hold' = `vmat'[1,`i']
			mat `vmat'[1,`i'] = `vmat'[1,`i'+1]
			mat `vmat'[1,`i'+1] = `hold'
			local i = 1
		}
		else {
			local i = `i' + 1
		}
	}
end

exit

********************************** VERSION COMMENTS **********************************
*  1.0.2:  add logic for reg3. Sargan test
*  1.0.3:  add prunelist to ensure that count of excluded exogeneous is correct 
*  1.0.4:  revise option to exog(), allow included exog to be specified as well
*  1.0.5:  switch from reg3 to regress, many options and output changes
*  1.0.6:  fixed treatment of nocons in Sargan and C-stat, and corrected problems
*          relating to use of nocons combined with a constant as an IV
*  1.0.7:  first option reports F-test of excluded exogenous; prunelist bug fix
*  1.0.8:  dropped prunelist and switched to housekeeping of variable lists
*  1.0.9:  added collinearity checks; C-stat calculated with recursive call;
*          added ffirst option to report only F-test of excluded exogenous
*          from 1st stage regressions
*  1.0.10: 1st stage regressions also report partial R2 of excluded exogenous
*  1.0.11: complete rewrite of collinearity approach - no longer uses calls to
*          _rmcoll, does not track specific variables dropped; prunelist removed
*  1.0.12: reorganised display code and saved results to enable -replay()-
*  1.0.13: -robust- and -cluster- now imply -small-
*  1.0.14: fixed hascons bug; removed ivreg predict fn (it didn't work); allowed
*          robust and cluster with z stats and correct dofs
*  1.0.15: implemented robust Sargan stat; changed to only F-stat, removed chi-sq;
*          removed exog option (only orthog works)
*  1.0.16: added clusterised Sargan stat; robust Sargan handles collinearities;
*          predict now works with standard SE options plus resids; fixed orthog()
*          so it accepts time series operators etc.
*  1.0.17: fixed handling of weights.  fw, aw, pw & iw all accepted.
*  1.0.18: fixed bug in robust Sargan code relating to time series variables.
*  1.0.19: fixed bugs in reporting ranks of X'X and Z'Z
*          fixed bug in reporting presence of constant
*  1.0.20: added GMM option and replaced robust Sargan with (equivalent) J;
*          added saved statistics of 1st stage regressions
*  1.0.21: added Cragg HOLS estimator, including allowing empty endog list;
*          -regress- syntax now not allowed; revised code searching for "_cons"
*  1.0.22: modified cluster output message; fixed bug in replay for Sargan/Hansen stat;
*          exactly identified Sargan/Hansen now exactly zero and p-value not saved as e();
*          cluster multiplier changed to 1 (from buggy multiplier), in keeping with
*          eg Wooldridge 2002 p. 193.
*  1.0.23: fixed orthog option to prevent abort when restricted equation is underid.
*  1.0.24: fixed bug if 1st stage regressions yielded missing values for saving in e().
*  1.0.25: Added Shea version of partial R2
*  1.0.26: Replaced Shea algorithm with Godfrey algorithm
*  1.0.27: Main call to regress is OLS form if OLS or HOLS is specified; error variance
*          in Sargan and C statistics use small-sample adjustment if -small- option is
*          specified; dfn of S matrix now correctly divided by sample size
*  1.0.28: HAC covariance estimation implemented
*          Symmetrize all matrices before calling syminv
*          Added hack to catch F stats that ought to be missing but actually have a
*          huge-but-not-missing value
*          Fixed dof of F-stat - was using rank of ZZ, should have used rank of XX (couldn't use df_r
*          because it isn't always saved.  This is because saving df_r triggers small stats
*          (t and F) even when -post- is called without dof() option, hence df_r saved only
*          with -small- option and hence a separate saved macro Fdf2 is needed.
*          Added rankS to saved macros
*          Fixed trap for "no regressors specified"
*          Added trap to catch gmm option with no excluded instruments
*          Allow OLS syntax (no endog or excluded IVs specified)
*          Fixed error messages and traps for rank-deficient robust cov matrix; includes
*          singleton dummy possibility
*          Capture error if posting estimated VCV that isn't pos def and report slightly
*          more informative error message
*          Checks 3 variable lists (endo, inexog, exexog) separately for collinearities
*          Added AC (autocorrelation-consistent but conditionally-homoskedastic) option
*          Sargan no longer has small-sample correction if -small- option
*          robust, cluster, AC, HAC all passed on to first-stage F-stat
*          bw must be < T
*  1.0.29  -orthog- also displays Hansen-Sargan of unrestricted equation
*          Fixed collinearity check to include nocons as well as hascons
*          Fixed small bug in Godfrey-Shea code - macros were global rather than local
*          Fixed larger bug in Godfrey-Shea code - was using mixture of sigma-squares from IV and OLS
*            with and without small-sample corrections
*          Added liml and kclass
*  1.0.30  Changed order of insts macro to match saved matrices S and W
*  2.0.00  Collinearities no longer -qui-
*          List of instruments tested in -orthog- option prettified
*  2.0.01  Fixed handling of nocons with no included exogenous, including LIML code
*  2.0.02  Allow C-test if unrestricted equation is just-identified.  Implemented by
*          saving Hansen-Sargan dof as = 0 in e() if just-identified.
*  2.0.03  Added score() option per latest revision to official ivreg
*  2.0.04  Changed score() option to pscore() per new official ivreg
*  2.0.05  Fixed est hold bug in first-stage regressions
*          Fixed F-stat finite sample adjustment with cluster option to match official Stata
*          Fixed F-stat so that it works with hascons (collinearity with constant is removed)
*          Fixed bug in F-stat code - wasn't handling failed posting of vcv
*          No longer allows/ignores nonsense options
*  2.0.06  Modified lsStop to sync with official ivreg 5.1.3
*  2.0.07a Working version of CUE option
*          Added sortpreserve, ivar and tvar options
*          Fixed smalls bug in calculation of T for AC/HAC - wasn't using the last ob
*          in QS kernel, and didn't take account of possible dropped observations
*  2.0.07b Fixed macro bug that truncated long varlists
*  2.0.07c Added dof option.
*          Changed display of RMSE so that more digits are displayed (was %8.1g)
*          Fixed small bug where cstat was local macro and should have been scalar
*          Fixed bug where C stat failed with cluster.  NB: wmatrix option and cluster are not compatible!
*  2.0.7d  Fixed bug in dof option
*  2.1.0   Added first-stage identification, weak instruments, and redundancy stats
*  2.1.01  Tidying up cue option checks, reporting of cue in output header, etc.
*  2.1.02  Used Poskitt-Skeels (2002) result that C-D eval = cceval / (1-cceval)
*  2.1.03  Added saved lists of separate included and excluded exogenous IVs
*  2.1.04  Added Anderson-Rubin test of signif of endog regressors
*  2.1.05  Fix minor bugs relating to cluster and new first-stage stats
*  2.1.06  Fix bug in cue: capture estimates hold without corresponding capture on estimates unhold
*  2.1.07  Minor fix to ereturn local wexp, promote to version 8.2
*  2.1.08  Added dofminus option, removed dof option.  Added A-R test p-values to e().
*          Minor bug fix to A-R chi2 test - was N chi2, should have been N-L chi2.
*          Changed output to remove potentially misleading refs to N-L etc.
*          Bug fix to rhs count - sometimes regressors could have exact zero coeffs
*          Bug fix related to cluster - if user omitted -robust-, orthog would use Sargan and not J
*          Changed output of Shea R2 to make clearer that F and p-values do not refer to it
*          Improved handling of collinearites to check across inexog, exexog and endo lists
*          Total weight statement moved to follow summ command
*          Added traps to catch errors if no room to save temporary estimations with _est hold
*          Added -savefirst- option. Removed -hascons-, now synonymous with -nocons-.
*  2.1.09  Fixes to dof option with cluster so it no longer mimics incorrect areg behavior
*          Local ivreg2_cmd to allow testing under name ivreg3
*          If wmatrix supplied, used (previously not used if non-robust sargan stat generated)
*          Allowed OLS using (=) syntax (empty endo and exexog lists)
*          Clarified error message when S matrix is not of full rank
*          cdchi2p, ardf, ardf_r added to saved macros
*          first and ffirst replay() options; DispFirst and DispFFirst separately codes 1st stage output
*          Added savefprefix, macro with saved first-stage equation names.
*          Added version option.
*          Added check for duplicate variables to collinearity checks
*          Rewrote/simplified Godfrey-Shea partial r2 code
* 2.1.10   Added NOOUTput option
*          Fixed rf bug so that first does not trigger unnecessary saved rf
*          Fixed cue bug - was not starting with robust 2-step gmm if robust/cluster
* 2.1.11   Dropped incorrect/misleading dofminus adjustments in first-stage output summary
* 2.1.12   Collinearity check now checks across inexog/exexog/endog simultaneously
* 2.1.13   Added check to catch failed first-stage regressions
*          Fixed misleading failed C-stat message
* 2.1.14   Fixed mishandling of missing values in AC (non-robust) block

